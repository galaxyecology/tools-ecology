#!/usr/bin/env python3
"""
safran_extract.py
-----------------
Extract SAFRAN climate data from the GEOSAS OGC-EDR API for one or more
locations defined in a coordinate file, and produce a SINGLE merged output file.

API standard : OGC Environmental Data Retrieval (EDR) v1.1
Service URL  : https://api.geosas.fr/edr/collections/safran-isba/
Documentation: https://geosas.fr/web/?page_id=6345

Confirmed available endpoints:
  /position  → POINT, MULTIPOINT
  /cube      → bounding-box (bbox=minx,miny,maxx,maxy)

Geometry routing (automatic):
  POINT / MULTIPOINT → /position  (coords=WKT)
  LINESTRING         → /position  (vertices → MULTIPOINT)
  POLYGON            → /cube      (bounding box extracted)

Merge strategy per format:
  CSV         → pd.concat, 'site_name' column added, single .csv
  parquet     → pd.concat, 'site_name' column added, single .parquet
  CoverageJSON→ wrapped in a CoverageCollection JSON, single .json
  netCDF4     → xarray.concat along new 'site' dimension, single .nc

Example usage:
python safran_extract.py \
    --parameters T_Q ETP_Q \
    --coord-file test_single_point.csv \
    --start-date 1990-01-01 \
    --end-date 2000-12-31
"""

import argparse
import io
import json
import os
import sys
import logging
import tempfile
from datetime import datetime, date
from pathlib import Path

import pandas as pd
import geopandas as gpd
import requests
from shapely import wkt as shapely_wkt
from shapely.geometry import Point, MultiPoint

# ─────────────────────────────────────────────────────────────────────────────
# Constants
# ─────────────────────────────────────────────────────────────────────────────

EDR_BASE_URL = "https://api.geosas.fr/edr/collections/safran-isba"

SAFRAN_OLDEST_DATE = date(1958, 8, 1)
SAFRAN_NEWEST_DATE = date.today()

DEFAULT_START_DATE = "1960-01-01"
DEFAULT_END_DATE = "2020-12-31"

VALID_PARAMETERS = [
    "T_Q",           # Mean daily air temperature (°C)
    "TINF_H_Q",      # Min of 24 hourly temperatures (°C)
    "TSUP_H_Q",      # Max of 24 hourly temperatures (°C)
    "ETP_Q",         # Daily potential evapotranspiration – Penman-Monteith (mm)
    "PRELIQ_Q",      # Daily liquid precipitation (mm)
    "PRENEI_Q",      # Daily solid precipitation / snowfall (mm)
    "FF_Q",          # Mean daily wind speed at 10 m (m/s)
    "HU_Q",          # Mean daily relative humidity (%)
    "SSI_Q",         # Daily visible / shortwave radiation (J/cm²)
    "DLI_Q",         # Daily atmospheric / longwave radiation (J/cm²)
    "SWI_Q",         # Daily soil wetness index (%)
    "DRAINC_Q",      # Daily drainage (mm)
    "RUNC_Q",        # Daily surface runoff (mm)
    "RESR_NEIGE_Q",  # Daily snow water equivalent (mm)
]

VALID_OUTPUT_FORMATS = ["CSV", "CoverageJSON", "netCDF4", "parquet"]

FORMAT_EXTENSIONS = {
    "CSV": "csv",
    "CoverageJSON": "json",
    "netCDF4": "nc",
    "parquet": "parquet",
}

DATE_FMT = "%Y-%m-%d"

# Column-name detection used to reshape the API responses into the target
# output layout:  site_name, date, <variables…>, longitude, latitude
TIME_COL_CANDIDATES = ("time", "date", "datetime", "valid_time", "t", "phenomenontime")
LON_COL_CANDIDATES = ("longitude", "lon", "long", "x")
LAT_COL_CANDIDATES = ("latitude", "lat", "y")
# Coordinate / housekeeping columns coming from the API that we drop, because
# the longitude/latitude written to the output come from the INPUT file.
DROP_COORD_CANDIDATES = (
    "longitude", "lon", "long", "x",
    "latitude", "lat", "y", "z",
    "crs", "coords", "geometry", "wkt",
)

# ─────────────────────────────────────────────────────────────────────────────
# Logging
# ─────────────────────────────────────────────────────────────────────────────

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S",
)
log = logging.getLogger(__name__)

# ─────────────────────────────────────────────────────────────────────────────
# Date helpers
# ─────────────────────────────────────────────────────────────────────────────


def parse_date(value: str, field_name: str = "date") -> date:
    try:
        return datetime.strptime(str(value).strip(), DATE_FMT).date()
    except ValueError:
        raise argparse.ArgumentTypeError(
            f"Invalid {field_name} '{value}'. Expected format: YYYY-MM-DD"
        )


def validate_date_range(start: date, end: date, context: str = "") -> None:
    ctx = f" ({context})" if context else ""
    if start < SAFRAN_OLDEST_DATE:
        raise ValueError(
            f"Start date {start}{ctx} is before the oldest available SAFRAN "
            f"record ({SAFRAN_OLDEST_DATE}). Please choose a later date."
        )
    if end > SAFRAN_NEWEST_DATE:
        raise ValueError(
            f"End date {end}{ctx} is in the future ({SAFRAN_NEWEST_DATE} is today)."
        )
    if start > end:
        raise ValueError(f"Start date {start}{ctx} is after end date {end}{ctx}.")

# ─────────────────────────────────────────────────────────────────────────────
# Geometry helpers
# ─────────────────────────────────────────────────────────────────────────────


def build_geometry_from_row(row: pd.Series) -> tuple[str, str]:
    """
    Return (wkt_string, GEOM_TYPE_UPPER) for a single DataFrame row.

    Detection priority:
      1. Native Shapely geometry object (GeoDataFrame only).
      2. 'geometry'/'wkt'/'WKT'/'geom' column containing WKT text.
      3. 'latitude'/'lat' + 'longitude'/'lon' columns → POINT.
    """
    from shapely.geometry.base import BaseGeometry

    if hasattr(row, "geometry") and isinstance(row.geometry, BaseGeometry):
        geom = row.geometry
        return geom.wkt, geom.geom_type.upper()

    for col in ("geometry", "wkt", "WKT", "geom"):
        if col in row.index and pd.notna(row[col]):
            geom = shapely_wkt.loads(str(row[col]))
            return geom.wkt, geom.geom_type.upper()

    lat_col = next((c for c in row.index if c.lower() in ("latitude", "lat")), None)
    lon_col = next((c for c in row.index if c.lower() in ("longitude", "lon", "long")), None)
    if lat_col and lon_col:
        geom = Point(float(row[lon_col]), float(row[lat_col]))
        return geom.wkt, "POINT"

    raise ValueError(
        "Cannot determine geometry from row. "
        "Provide 'latitude'+'longitude' columns, or a 'geometry'/'wkt' column with WKT text."
    )


def extract_lonlat(row: pd.Series, wkt_str: str, geom_type: str) -> tuple[float | None, float | None]:
    """
    Return a representative (longitude, latitude) for the row, preferring the
    explicit lat/lon columns of the INPUT file (so the exact input coordinates
    are preserved in the output), and falling back to the geometry otherwise.
    """
    lat_col = next((c for c in row.index if c.lower() in ("latitude", "lat")), None)
    lon_col = next((c for c in row.index if c.lower() in ("longitude", "lon", "long")), None)
    if lat_col and lon_col and pd.notna(row[lat_col]) and pd.notna(row[lon_col]):
        try:
            return float(row[lon_col]), float(row[lat_col])
        except (TypeError, ValueError):
            pass
    try:
        geom = shapely_wkt.loads(wkt_str)
        point = geom if geom_type == "POINT" else geom.representative_point()
        return float(point.x), float(point.y)
    except Exception:
        return None, None


def site_label(row: pd.Series, idx) -> str:
    """Pick a human-readable site label, preferring a 'site_name' column."""
    for key in ("site_name", "name", "id"):
        if key in row.index and pd.notna(row[key]):
            return str(row[key])
    return str(idx)


def site_identifier(row: pd.Series, idx) -> str:
    """Pick a stable site identifier, preferring a 'site_id'/'id' column.

    Falls back to the row index when no identifier column is present.
    """
    for key in ("site_id", "id"):
        if key in row.index and pd.notna(row[key]):
            return str(row[key])
    return str(idx)


def detect_dominant_geometry(df: pd.DataFrame) -> str | None:
    for col in ("geometry", "wkt", "WKT", "geom"):
        if col in df.columns:
            sample = df[col].dropna()
            if not sample.empty:
                try:
                    return shapely_wkt.loads(str(sample.iloc[0])).geom_type.upper()
                except Exception:
                    pass
    if any(c.lower() in df.columns for c in ("latitude", "lat")):
        return "POINT"
    return None

# ─────────────────────────────────────────────────────────────────────────────
# Coordinate file loading
# ─────────────────────────────────────────────────────────────────────────────


def load_coordinate_file(filepath: str, fmt: str = None) -> pd.DataFrame:
    path = Path(filepath)
    suffix = (f".{fmt}" if fmt else path.suffix).lower()

    if suffix in {".shp", ".gpkg", ".geojson", ".json"}:
        gdf = gpd.read_file(filepath)
        log.info(
            "Loaded geo-file '%s': %d feature(s), geometry type(s): %s",
            path.name, len(gdf), gdf.geom_type.unique().tolist(),
        )
        return gdf

    if suffix == ".csv":
        df = pd.read_csv(filepath)
    elif suffix == ".tsv":
        df = pd.read_csv(filepath, sep="\t")
    elif suffix in (".xlsx", ".xls"):
        df = pd.read_excel(filepath)
    else:
        log.warning("Unknown extension '%s', trying auto-detect (sep=None).", suffix)
        df = pd.read_csv(filepath, sep=None, engine="python")

    log.info("Loaded tabular file '%s': %d row(s).", path.name, len(df))
    return df

# ─────────────────────────────────────────────────────────────────────────────
# GEOSAS OGC-EDR API call
# ─────────────────────────────────────────────────────────────────────────────


def fetch_safran_data(
    wkt_geometry: str,
    geom_type: str,
    epsg: int,
    start: date,
    end: date,
    parameters: list[str],
    output_format: str,
    timeout: int = 120,
) -> bytes:
    """
    Perform a GET request to the correct GEOSAS OGC-EDR endpoint and return
    the raw response bytes.
    """
    endpoint, geom_params = prepare_api_geometry(wkt_geometry, geom_type)
    url = f"{EDR_BASE_URL}/{endpoint}"

    params = {
        "parameter-name": ",".join(parameters),
        "crs": f"EPSG:{epsg}",
        "datetime": f"{start.strftime(DATE_FMT)}/{end.strftime(DATE_FMT)}",
        "f": output_format,
        **geom_params,
    }

    log.debug("GET %s  params=%s", url, params)

    try:
        response = requests.get(url, params=params, timeout=timeout)
        response.raise_for_status()
    except requests.exceptions.HTTPError as err:
        raise RuntimeError(
            f"GEOSAS API HTTP error [{response.status_code}] "
            f"for {geom_type} → /{endpoint}: {err}\n"
            f"Full URL: {response.url}\n"
            f"Response: {response.text[:500]}"
        ) from err
    except requests.exceptions.ConnectionError as err:
        raise RuntimeError(
            f"Could not connect to GEOSAS API at {url}. Check your network."
        ) from err
    except requests.exceptions.Timeout:
        raise RuntimeError(f"GEOSAS API request timed out after {timeout} s.")

    return response.content


def prepare_api_geometry(wkt_str: str, geom_type: str) -> tuple[str, dict]:
    """
    Map a geometry to the correct endpoint + query parameters.
      POINT / MULTIPOINT → /position, coords=WKT
      LINESTRING         → /position, vertices → MULTIPOINT
      POLYGON            → /cube,     bbox=minx,miny,maxx,maxy
    """
    geom = shapely_wkt.loads(wkt_str)

    if geom_type in ("POINT", "MULTIPOINT"):
        return "position", {"coords": wkt_str}

    if geom_type == "LINESTRING":
        coords = list(geom.coords)
        multipoint = MultiPoint(coords)
        log.info(
            "  LINESTRING → converting %d vertices to MULTIPOINT for /position",
            len(coords),
        )
        return "position", {"coords": multipoint.wkt}

    if geom_type in ("POLYGON", "MULTIPOLYGON"):
        minx, miny, maxx, maxy = geom.bounds
        bbox_str = f"{minx},{miny},{maxx},{maxy}"
        log.info("  %s → bounding box for /cube: %s", geom_type, bbox_str)
        return "cube", {"bbox": bbox_str}

    raise ValueError(
        f"Unsupported geometry type '{geom_type}'. "
        "Supported: POINT, MULTIPOINT, LINESTRING, POLYGON, MULTIPOLYGON."
    )

# ─────────────────────────────────────────────────────────────────────────────
# Reshape helper
# ─────────────────────────────────────────────────────────────────────────────


def standardize_table(
    df: pd.DataFrame,
    sid: str,
    label: str,
    lon: float | None,
    lat: float | None,
) -> pd.DataFrame:
    """
    Reshape a single API response table into the target layout:

        site_id, site_name, date, <variable columns…>, longitude, latitude

    - the time/datetime column is converted to a pure DATE (YYYY-MM-DD), no time;
    - any coordinate columns returned by the API are dropped and replaced by the
      exact longitude/latitude of the input point;
    - the site label and identifier are written to 'site_name' / 'site_id'.
    """
    df = df.copy()
    cols_lower = {c.lower(): c for c in df.columns}

    # 1. Time → date (date only, no time-of-day)
    time_col = next((cols_lower[k] for k in TIME_COL_CANDIDATES if k in cols_lower), None)
    if time_col is not None:
        df["date"] = pd.to_datetime(df[time_col], errors="coerce").dt.strftime("%Y-%m-%d")
        if time_col != "date":
            df = df.drop(columns=[time_col])
    else:
        df["date"] = pd.NA

    # 2. Drop API coordinate/housekeeping columns (kept from the input instead)
    drop_cols = [c for c in df.columns if c.lower() in DROP_COORD_CANDIDATES and c != "date"]
    df = df.drop(columns=drop_cols, errors="ignore")

    # 3. Remaining non-date columns are the climate variables
    variable_cols = [c for c in df.columns if c != "date"]

    # 4. Assemble final layout
    df.insert(0, "site_name", str(label))
    df.insert(0, "site_id", str(sid))
    df["longitude"] = lon
    df["latitude"] = lat

    ordered = ["site_id", "site_name", "date"] + variable_cols + ["longitude", "latitude"]
    return df[ordered]

# ─────────────────────────────────────────────────────────────────────────────
# Merge functions  (one per output format)
# ─────────────────────────────────────────────────────────────────────────────
# Each function receives a list of (site_label, lon, lat, raw_bytes) and returns
# the merged content as bytes ready to be written to a single file.
# ─────────────────────────────────────────────────────────────────────────────


def merge_csv(chunks: list[tuple[str, str, float | None, float | None, bytes]]) -> bytes:
    """
    Parse each CSV response and reshape it to:
        site_id, site_name, date, <variables…>, longitude, latitude
    then concatenate all sites into a single long-format table.
    """
    dfs = []
    for sid, label, lon, lat, raw in chunks:
        try:
            df = pd.read_csv(io.BytesIO(raw))
            df = standardize_table(df, sid, label, lon, lat)
            dfs.append(df)
            log.debug("  Parsed CSV for '%s': %d rows × %d cols", label, *df.shape)
        except Exception as exc:
            log.warning("Could not parse CSV response for site '%s': %s", label, exc)

    if not dfs:
        raise ValueError("No valid CSV responses could be parsed for merging.")

    merged = pd.concat(dfs, ignore_index=True)
    log.info(
        "Merged %d site(s) → %d total rows, columns: %s",
        len(dfs), len(merged), list(merged.columns),
    )
    return merged.to_csv(index=False).encode("utf-8")


def merge_parquet(chunks: list[tuple[str, str, float | None, float | None, bytes]]) -> bytes:
    """
    Same layout as merge_csv but written to a single parquet file.
    """
    dfs = []
    for sid, label, lon, lat, raw in chunks:
        try:
            df = pd.read_parquet(io.BytesIO(raw))
            df = standardize_table(df, sid, label, lon, lat)
            dfs.append(df)
        except Exception as exc:
            log.warning("Could not parse parquet response for site '%s': %s", label, exc)

    if not dfs:
        raise ValueError("No valid parquet responses could be parsed for merging.")

    merged = pd.concat(dfs, ignore_index=True)
    log.info("Merged %d site(s) → %d total rows.", len(dfs), len(merged))
    buf = io.BytesIO()
    merged.to_parquet(buf, index=False)
    return buf.getvalue()


def merge_coveragejson(chunks: list[tuple[str, str, float | None, float | None, bytes]]) -> bytes:
    """
    Wrap each CoverageJSON object in a CoverageCollection, adding
    'site_id' and 'site_name' properties to each coverage for identification.

    Output follows the OGC CoverageJSON spec for CoverageCollections:
    https://covjson.org/spec/#coverage-collections
    """
    coverages = []
    for sid, label, lon, lat, raw in chunks:
        try:
            cov = json.loads(raw.decode("utf-8"))
            # Inject site_id / site_name as top-level properties
            cov["properties"] = cov.get("properties", {})
            cov["properties"]["site_id"] = str(sid)
            cov["properties"]["site_name"] = str(label)
            coverages.append(cov)
        except Exception as exc:
            log.warning("Could not parse CoverageJSON for site '%s': %s", label, exc)

    if not coverages:
        raise ValueError("No valid CoverageJSON responses could be parsed for merging.")

    collection = {
        "type": "CoverageCollection",
        "coverages": coverages,
    }
    log.info("Merged %d coverage(s) into a CoverageCollection.", len(coverages))
    return json.dumps(collection, indent=2, ensure_ascii=False).encode("utf-8")


def merge_netcdf(chunks: list[tuple[str, str, float | None, float | None, bytes]]) -> bytes:
    """
    Concatenate netCDF responses along a new 'site' dimension using xarray.

    Each response is written to a temporary file (required by xarray/netCDF4),
    opened as a Dataset, and given scalar 'site' / 'site_id' coordinates equal
    to the row label / identifier before concatenation.

    Falls back gracefully if xarray is not installed.
    """
    try:
        import xarray as xr
    except ImportError:
        raise RuntimeError(
            "xarray is required to merge netCDF4 files. "
            "Install it with:  pip install xarray netCDF4"
        )

    tmp_paths: list[str] = []
    datasets: list = []

    try:
        for sid, label, lon, lat, raw in chunks:
            tmp = tempfile.NamedTemporaryFile(suffix=".nc", delete=False)
            tmp.write(raw)
            tmp.close()
            tmp_paths.append(tmp.name)
            try:
                ds = xr.open_dataset(tmp.name)
                # Add scalar 'site' / 'site_id' coords so concat creates the dimension
                ds = ds.assign_coords(site=str(label), site_id=str(sid)).expand_dims("site")
                datasets.append(ds)
                log.debug("  Opened netCDF for '%s': %s", label, list(ds.data_vars))
            except Exception as exc:
                log.warning("Could not open netCDF for site '%s': %s", label, exc)

        if not datasets:
            raise ValueError("No valid netCDF responses could be opened for merging.")

        merged = xr.concat(datasets, dim="site")
        log.info(
            "Merged %d dataset(s) along 'site' dimension → variables: %s",
            len(datasets), list(merged.data_vars),
        )

        out_tmp = tempfile.NamedTemporaryFile(suffix=".nc", delete=False)
        out_tmp.close()
        tmp_paths.append(out_tmp.name)
        merged.to_netcdf(out_tmp.name)

        with open(out_tmp.name, "rb") as fh:
            result = fh.read()

    finally:
        # Clean up all temporary files
        for path in tmp_paths:
            try:
                os.unlink(path)
            except OSError:
                pass

    return result


MERGE_FUNCTIONS = {
    "CSV": merge_csv,
    "parquet": merge_parquet,
    "CoverageJSON": merge_coveragejson,
    "netCDF4": merge_netcdf,
}

# ─────────────────────────────────────────────────────────────────────────────
# Output
# ─────────────────────────────────────────────────────────────────────────────


def build_output_path(base_output: str, fmt: str) -> str:
    """Return the output file path.

    Galaxy passes a fully managed path with an extension (e.g. dataset_uuid.dat)
    — write to it exactly as given so Galaxy can find the file afterwards.
    When called from the CLI without any extension, the correct format extension
    is appended automatically (e.g. safran_output → safran_output.csv).
    """
    base = Path(base_output)
    base.parent.mkdir(parents=True, exist_ok=True)
    if base.suffix:
        # Already has an extension (Galaxy .dat or explicit CLI path) → use as-is.
        return str(base)
    # No extension → CLI convenience: append the right one.
    ext = FORMAT_EXTENSIONS.get(fmt, fmt.lower())
    return str(base.parent / f"{base.stem}.{ext}")


def save_file(content: bytes, filepath: str) -> None:
    with open(filepath, "wb") as fh:
        fh.write(content)
    log.info("Saved merged output → '%s' (%d bytes).", filepath, len(content))

# ─────────────────────────────────────────────────────────────────────────────
# Argument parser
# ─────────────────────────────────────────────────────────────────────────────


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="safran_extract.py",
        description=(
            "Extract SAFRAN climate reanalysis data from the GEOSAS OGC-EDR API\n"
            "and produce a SINGLE merged output file for all sites.\n\n"
            "SAFRAN is a French meteorological gridded reanalysis (8 km) produced\n"
            "by Météo-France, covering metropolitan France from 1958-08-01 to\n"
            "near-present (updated monthly).\n\n"
            "API : https://api.geosas.fr/edr/collections/safran-isba/\n"
            "Docs: https://geosas.fr/web/?page_id=6345\n\n"
            "Geometry routing (automatic):\n"
            "  POINT / MULTIPOINT → /position  (coords=WKT)\n"
            "  LINESTRING         → /position  (vertices → MULTIPOINT)\n"
            "  POLYGON            → /cube      (bounding box extracted)\n\n"
            "Merge strategy per format:\n"
            "  CSV / parquet  → long-format table, 'site_name' column added\n"
            "  CoverageJSON   → OGC CoverageCollection wrapping all coverages\n"
            "  netCDF4        → xarray.concat along a new 'site' dimension"
        ),
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=(
            "EXAMPLES\n"
            "--------\n"
            "  # 4 WGS84 points → single merged CSV\n"
            "  python safran_extract.py \\\n"
            "      --parameters T_Q ETP_Q PRELIQ_Q \\\n"
            "      --coord-file sites.csv \\\n"
            "      --start-date 1990-01-01 --end-date 2000-12-31 \\\n"
            "      --output results/safran_all_sites\n\n"
            "OUTPUT STRUCTURE (CSV example)\n"
            "------------------------------\n"
            "  site_id,site_name,date,T_Q,longitude,latitude\n"
            "  1,Montpellier,1995-01-01,7.5,3.8711,43.6320\n"
            "  1,Montpellier,1995-01-02,2.7,3.8711,43.6320\n"
            "  ...\n\n"
            "CITATIONS\n"
            "---------\n"
            "  Vidal et al. (2010). Int. J. Climatol., 30(11), 1627-1644.\n"
            "  https://doi.org/10.1002/joc.2003\n\n"
            "  Habets et al. (2008). J. Geophys. Res. Atmos., 113(D6).\n"
            "  https://doi.org/10.1029/2007JD008548\n\n"
            "  INRAE / GEOSAS: https://geosas.fr/\n"
        ),
    )

    parser.add_argument(
        "--parameters", "-p",
        nargs="+", required=True, metavar="PARAM",
        choices=VALID_PARAMETERS,
        help=(
            "One or more SAFRAN variable codes (space-separated). "
            "Available: " + ", ".join(VALID_PARAMETERS) + ". "
            "NOTE: liquid rain = PRELIQ_Q; min/max temp = TINF_H_Q / TSUP_H_Q. "
            "Example: --parameters T_Q ETP_Q PRELIQ_Q"
        ),
    )

    parser.add_argument("--coord-format", default=None, metavar="EXT")

    parser.add_argument(
        "--coord-file", "-c",
        required=True, metavar="FILE",
        help=(
            "Coordinate / geometry file (CSV, TSV, Excel, Shapefile, GeoPackage, GeoJSON). "
            "For CSV/Excel: 'latitude'+'longitude' columns for POINTs, "
            "or 'geometry'/'wkt' column with WKT text for polygons/lines. "
            "A 'site_name' (or 'name'/'id') column is used to label each site. "
            "Geometry type is auto-detected. "
            "Optional 'begin_date'/'end_date' columns (YYYY-MM-DD) override dates per row."
        ),
    )

    parser.add_argument(
        "--start-date", "-s",
        default=None, metavar="YYYY-MM-DD",
        help=(
            f"Global start date. Cannot be before {SAFRAN_OLDEST_DATE}. "
            f"Default if omitted and no 'begin_date' column: {DEFAULT_START_DATE}."
        ),
    )

    parser.add_argument(
        "--end-date", "-e",
        default=None, metavar="YYYY-MM-DD",
        help=(
            "Global end date. SAFRAN is near real-time (updated monthly). "
            f"Default if omitted and no 'end_date' column: {DEFAULT_END_DATE}."
        ),
    )

    parser.add_argument(
        "--epsg",
        type=int, default=4326, metavar="CODE",
        help=(
            "EPSG code of the coordinate file CRS. "
            "Confirmed values: 4326 (WGS84, default), 2154 (Lambert-93). "
            "Example: --epsg 2154"
        ),
    )

    parser.add_argument(
        "--format", "-f",
        dest="output_format", default="CSV",
        choices=VALID_OUTPUT_FORMATS,
        help=(
            "Output format: CSV (default), CoverageJSON, netCDF4, parquet. "
            "CSV/parquet → long-format table with 'site_name' column. "
            "CoverageJSON → OGC CoverageCollection. "
            "netCDF4 → requires xarray (pip install xarray netCDF4)."
        ),
    )

    parser.add_argument(
        "--output", "-o",
        default="safran_output", metavar="PATH",
        help=(
            "Output file path (extension is added automatically if omitted). "
            "All sites are merged into this single file. "
            "The parent directory is created if it does not exist. "
            "Default: safran_output  →  safran_output.csv (or .json / .nc / .parquet)"
        ),
    )

    parser.add_argument(
        "--timeout",
        type=int, default=120, metavar="SECONDS",
        help="HTTP request timeout in seconds. Default: 120.",
    )

    return parser

# ─────────────────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────────────────


def main(argv=None):
    parser = build_parser()
    args = parser.parse_args(argv)

    # ── 1. Parse global dates ───────────────────────────────────────────────
    global_start = parse_date(args.start_date, "start-date") if args.start_date else None
    global_end = parse_date(args.end_date, "end-date") if args.end_date else None

    # ── 2. Load coordinate file ─────────────────────────────────────────────
    df = load_coordinate_file(args.coord_file)

    log.info("Dominant geometry: %s", detect_dominant_geometry(df) or "unknown")

    col_lower = {c.lower(): c for c in df.columns}
    col_begin = col_lower.get("begin_date") or col_lower.get("start_date") or col_lower.get("date_debut")
    col_end = col_lower.get("end_date") or col_lower.get("date_fin")

    # ── 3. Fetch data for every row, collect raw responses ──────────────────
    # Each chunk: (site_id, site_label, longitude, latitude, raw_bytes)
    chunks: list[tuple[str, str, float | None, float | None, bytes]] = []
    errors: list[tuple[int, str]] = []

    for idx, row in df.iterrows():
        row_label = site_label(row, idx)
        row_id = site_identifier(row, idx)
        log.info("── Row %s ──", row_label)

        # Resolve dates
        row_start = global_start
        row_end = global_end

        if col_begin and pd.notna(row.get(col_begin)):
            row_start = parse_date(str(row[col_begin]), f"begin_date row {idx}")
        if col_end and pd.notna(row.get(col_end)):
            row_end = parse_date(str(row[col_end]), f"end_date row {idx}")

        if row_start is None:
            if col_begin is None and global_start is None:
                parser.error(
                    "No --start-date provided and no 'begin_date' column in the file."
                )
            row_start = parse_date(DEFAULT_START_DATE)

        if row_end is None:
            if col_end is None and global_end is None:
                parser.error(
                    "No --end-date provided and no 'end_date' column in the file."
                )
            row_end = parse_date(DEFAULT_END_DATE)

        try:
            validate_date_range(row_start, row_end, context=f"row {idx}")
        except ValueError as exc:
            log.error("Skipping row %s: %s", idx, exc)
            errors.append((idx, str(exc)))
            continue

        try:
            wkt_str, geom_type = build_geometry_from_row(row)
        except ValueError as exc:
            log.error("Skipping row %s: %s", idx, exc)
            errors.append((idx, str(exc)))
            continue

        # Longitude / latitude of the input point (preserved in the output)
        lon, lat = extract_lonlat(row, wkt_str, geom_type)

        log.info(
            "  %s | EPSG:%d | %s → %s | vars: %s | fmt: %s",
            geom_type, args.epsg, row_start, row_end,
            ",".join(args.parameters), args.output_format,
        )

        try:
            raw = fetch_safran_data(
                wkt_geometry=wkt_str,
                geom_type=geom_type,
                epsg=args.epsg,
                start=row_start,
                end=row_end,
                parameters=args.parameters,
                output_format=args.output_format,
                timeout=args.timeout,
            )
            chunks.append((row_id, row_label, lon, lat, raw))
            log.info("  Fetched %d bytes for '%s'.", len(raw), row_label)
        except RuntimeError as exc:
            log.error("API error for row %s: %s", idx, exc)
            errors.append((idx, str(exc)))

    # ── 4. Merge all responses into a single file ───────────────────────────
    if not chunks:
        log.error("No data was successfully fetched. No output file written.")
        sys.exit(1)

    log.info("Merging %d site(s) as %s …", len(chunks), args.output_format)

    merge_fn = MERGE_FUNCTIONS[args.output_format]
    try:
        merged_bytes = merge_fn(chunks)
    except (ValueError, RuntimeError) as exc:
        log.error("Merge failed: %s", exc)
        sys.exit(1)

    out_path = build_output_path(args.output, args.output_format)
    save_file(merged_bytes, out_path)

    # ── 5. Summary ──────────────────────────────────────────────────────────
    success = len(chunks)
    total = len(df)
    log.info("Done. %d/%d site(s) merged into '%s'.", success, total, out_path)

    if errors:
        log.warning("%d site(s) failed and were excluded from the output:", len(errors))
        for row_id, msg in errors:
            log.warning("  Row %s — %s", row_id, msg)
        sys.exit(1)


if __name__ == "__main__":
    main()
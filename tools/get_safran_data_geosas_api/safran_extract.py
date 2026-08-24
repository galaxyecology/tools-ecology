#!/usr/bin/env python3
"""
safran_extract.py
-----------------
Extract SAFRAN climate data from the GEOSAS OGC-EDR API for one or more
locations defined in a coordinate file, and produce a SINGLE merged
output file.

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
import logging
import os
import sys
import tempfile
from datetime import date, datetime
from pathlib import Path

import geopandas as gpd

import pandas as pd

import requests

from shapely import wkt as shapely_wkt
from shapely.geometry import MultiPoint, Point

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
    "ETP_Q",         # Daily potential evapotranspiration – Penman-Monteith (mm)  # noqa: E501
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
TIME_COL_CANDIDATES = ("time", "date", "datetime", "valid_time", "t", "phenomenontime")  # noqa: E501
LON_COL_CANDIDATES = ("longitude", "lon", "long", "x")
LAT_COL_CANDIDATES = ("latitude", "lat", "y")
# Input columns that identify/label a site. Whichever of these is present is
# normalized to a single canonical 'site_name' output column, so the output
# schema is predictable regardless of which one the input file used.
LABEL_COL_CANDIDATES = ("site_name", "name", "id")
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
            f"End date {end}{ctx} is in the future ({SAFRAN_NEWEST_DATE} is today)."  # noqa: E501
        )
    if start > end:
        raise ValueError(f"Start date {start}{ctx} is after end date {end}{ctx}.")  # noqa: E501

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

    lat_col = next((c for c in row.index if c.lower() in ("latitude", "lat")), None)  # noqa: E501
    lon_col = next((c for c in row.index if c.lower() in ("longitude", "lon", "long")), None)  # noqa: E501
    if lat_col and lon_col:
        geom = Point(float(row[lon_col]), float(row[lat_col]))
        return geom.wkt, "POINT"

    raise ValueError(
        "Cannot determine geometry from row. "
        "Provide 'latitude'+'longitude' columns, or a 'geometry'/'wkt' column with WKT text."  # noqa: E501
    )


def extract_lonlat(row: pd.Series, wkt_str: str, geom_type: str) -> tuple[float | None, float | None]:  # noqa: E501
    """
    Return a representative (longitude, latitude) for the row, preferring the
    explicit lat/lon columns of the INPUT file (so the exact input coordinates
    are preserved in the output), and falling back to the geometry otherwise.
    """
    lat_col = next((c for c in row.index if c.lower() in ("latitude", "lat")), None)  # noqa: E501
    lon_col = next((c for c in row.index if c.lower() in ("longitude", "lon", "long")), None)  # noqa: E501
    if lat_col and lon_col and pd.notna(row[lat_col]) and pd.notna(row[lon_col]):  # noqa: E501
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


def detect_dominant_geometry(df: pd.DataFrame) -> str | None:
    for col in ("geometry", "wkt", "WKT", "geom"):
        if col in df.columns:
            sample = df[col].dropna()
            if not sample.empty:
                try:
                    return shapely_wkt.loads(str(sample.iloc[0])).geom_type.upper()  # noqa: E501
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
        log.warning("Unknown extension '%s', trying auto-detect (sep=None).", suffix)  # noqa: E501
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
            "  LINESTRING → converting %d vertices to MULTIPOINT for /position",  # noqa: E501
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
    sid: int,
    original_row: dict,
    lon: float | None,
    lat: float | None,
    label: str | None = None,
) -> pd.DataFrame:
    """
    Reshape a single API response table into the target layout:

        site_id, site_name, <other original input-file columns…>, date,
        <fetched climate variables…>[, longitude, latitude]

    - 'site_id' is always a 0-based sequential integer identifying the site
      (row processing order), regardless of any 'id'/'site_id' column that
      may exist in the input file.
    - 'site_name' is always present in the output. Whichever of
      'site_name' / 'name' / 'id' is found in the input file (in that
      priority order) is normalized/renamed to 'site_name', so the output
      schema is predictable no matter which one the input file used. If
      none of them is present, the computed row label (falling back to the
      row index) is used instead.
    - every OTHER column of the ORIGINAL coordinate/geometry file is
      preserved as-is and repeated across every date of that site's time
      series.
    - the time/datetime column returned by the API is converted to a pure
      DATE (YYYY-MM-DD, no time-of-day), named 'date'.
    - the remaining columns returned by the API are the requested climate
      variables.
    - 'longitude'/'latitude' convenience columns are appended only if the
      original file did not already provide them (e.g. for POLYGON /
      LINESTRING inputs described via a 'geometry'/'wkt' column instead of
      lat/lon).
    - if an original column's name collides with a reserved name ('site_id',
      'date') or with one of the fetched variable names, the original value
      is dropped for that column and a warning is logged (the fetched data
      takes precedence).
    """
    df = df.copy()
    cols_lower = {c.lower(): c for c in df.columns}

    # 1. Time → date (date only, no time-of-day)
    time_col = next((cols_lower[k] for k in TIME_COL_CANDIDATES if k in cols_lower), None)  # noqa: E501
    if time_col is not None:
        df["date"] = pd.to_datetime(df[time_col], errors="coerce").dt.strftime("%Y-%m-%d")  # noqa: E501
        if time_col != "date":
            df = df.drop(columns=[time_col])
    else:
        df["date"] = pd.NA

    # 2. Drop the API's own coordinate/housekeeping columns (the original
    #    file's columns, and/or the longitude/latitude below, are used instead)
    drop_cols = [c for c in df.columns if c.lower() in DROP_COORD_CANDIDATES and c != "date"]  # noqa: E501
    df = df.drop(columns=drop_cols, errors="ignore")

    # 3. Remaining non-date columns are the fetched climate variables
    variable_cols = [c for c in df.columns if c != "date"]

    # 4. Original input-file columns, minus anything colliding with a
    #    reserved/variable name (fetched data takes precedence on collision).
    #    The first column matching LABEL_COL_CANDIDATES is renamed to the
    #    canonical 'site_name' (not duplicated); everything else is kept as-is.  # noqa: E501
    reserved = {"site_id", "date", *[c.lower() for c in variable_cols]}
    orig_items = []
    site_name_seen = False
    for key, value in original_row.items():
        key_lower = key.lower()
        if key_lower in reserved:
            log.warning(
                "Input column '%s' collides with a reserved or fetched "
                "variable name; keeping the fetched value instead.", key,
            )
            continue
        if hasattr(value, "wkt"):  # Shapely geometry (GeoDataFrame input)
            value = value.wkt
        if not site_name_seen and key_lower in LABEL_COL_CANDIDATES:
            orig_items.append(("site_name", value))
            site_name_seen = True
            continue
        orig_items.append((key, value))

    # No 'site_name'/'name'/'id' column found in the input file → fall back
    # to the computed row label so 'site_name' is always present.
    if not site_name_seen:
        orig_items.insert(0, ("site_name", label if label is not None else str(sid)))  # noqa: E501

    # 5. Assemble: site_id, original columns, date, variables
    df["site_id"] = int(sid)
    for key, value in orig_items:
        df[key] = value

    ordered = ["site_id"] + [k for k, _ in orig_items] + ["date"] + variable_cols  # noqa: E501

    # 6. longitude/latitude convenience columns, only if not already present
    #    among the original file's own columns
    has_lon = any(k.lower() in LON_COL_CANDIDATES for k, _ in orig_items)
    has_lat = any(k.lower() in LAT_COL_CANDIDATES for k, _ in orig_items)
    if not has_lon:
        df["longitude"] = lon
        ordered.append("longitude")
    if not has_lat:
        df["latitude"] = lat
        ordered.append("latitude")

    return df[ordered]

# ─────────────────────────────────────────────────────────────────────────────
# Merge functions  (one per output format)
# ─────────────────────────────────────────────────────────────────────────────
# Each function receives a list of (site_id, site_label, lon, lat, raw_bytes,
# original_row) tuples and returns the merged content as bytes ready to be
# written to a single file.
# ─────────────────────────────────────────────────────────────────────────────


def merge_csv(chunks: list[tuple[int, str, float | None, float | None, bytes, dict]]) -> bytes:  # noqa: E501
    """
    Parse each CSV response and reshape it to:
        site_id, <original input columns…>, date, <variables…>[,
        longitude, latitude]
    then concatenate all sites into a single long-format table.
    """
    dfs = []
    for sid, label, lon, lat, raw, original_row in chunks:
        try:
            df = pd.read_csv(io.BytesIO(raw))
            df = standardize_table(df, sid, original_row, lon, lat, label=label)  # noqa: E501
            dfs.append(df)
            log.debug("  Parsed CSV for '%s': %d rows × %d cols", label, *df.shape)  # noqa: E501
        except Exception as exc:
            log.warning("Could not parse CSV response for site '%s': %s", label, exc)  # noqa: E501

    if not dfs:
        raise ValueError("No valid CSV responses could be parsed for merging.")

    merged = pd.concat(dfs, ignore_index=True)
    log.info(
        "Merged %d site(s) → %d total rows, columns: %s",
        len(dfs), len(merged), list(merged.columns),
    )
    return merged.to_csv(index=False).encode("utf-8")


def merge_parquet(chunks: list[tuple[int, str, float | None, float | None, bytes, dict]]) -> bytes:  # noqa: E501
    """
    Same layout as merge_csv but written to a single parquet file.
    """
    dfs = []
    for sid, label, lon, lat, raw, original_row in chunks:
        try:
            df = pd.read_parquet(io.BytesIO(raw))
            df = standardize_table(df, sid, original_row, lon, lat, label=label)  # noqa: E501
            dfs.append(df)
        except Exception as exc:
            log.warning("Could not parse parquet response for site '%s': %s", label, exc)  # noqa: E501

    if not dfs:
        raise ValueError("No valid parquet responses could be parsed for merging.")  # noqa: E501

    merged = pd.concat(dfs, ignore_index=True)
    log.info("Merged %d site(s) → %d total rows.", len(dfs), len(merged))
    buf = io.BytesIO()
    merged.to_parquet(buf, index=False)
    return buf.getvalue()


def merge_coveragejson(chunks: list[tuple[int, str, float | None, float | None, bytes, dict]]) -> bytes:  # noqa: E501
    """
    Wrap each CoverageJSON object in a CoverageCollection, adding
    'site_id' and 'site_name' properties, plus every column from the
    original input file, to each coverage for identification.

    Output follows the OGC CoverageJSON spec for CoverageCollections:
    https://covjson.org/spec/#coverage-collections
    """
    coverages = []
    for sid, label, lon, lat, raw, original_row in chunks:
        try:
            cov = json.loads(raw.decode("utf-8"))
            # Inject site_id / site_name as top-level properties
            cov["properties"] = cov.get("properties", {})
            cov["properties"]["site_id"] = int(sid)
            cov["properties"]["site_name"] = str(label)
            for key, value in original_row.items():
                if key in cov["properties"] or key.lower() in LABEL_COL_CANDIDATES:  # noqa: E501
                    continue
                if hasattr(value, "wkt"):  # Shapely geometry
                    value = value.wkt
                cov["properties"][key] = value
            coverages.append(cov)
        except Exception as exc:
            log.warning("Could not parse CoverageJSON for site '%s': %s", label, exc)  # noqa: E501

    if not coverages:
        raise ValueError("No valid CoverageJSON responses could be parsed for merging.")  # noqa: E501

    collection = {
        "type": "CoverageCollection",
        "coverages": coverages,
    }
    log.info("Merged %d coverage(s) into a CoverageCollection.", len(coverages))  # noqa: E501
    return json.dumps(collection, indent=2, ensure_ascii=False).encode("utf-8")


def merge_netcdf(chunks: list[tuple[int, str, float | None, float | None, bytes, dict]]) -> bytes:  # noqa: E501
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
        for sid, label, lon, lat, raw, original_row in chunks:
            tmp = tempfile.NamedTemporaryFile(suffix=".nc", delete=False)
            tmp.write(raw)
            tmp.close()
            tmp_paths.append(tmp.name)
            try:
                ds = xr.open_dataset(tmp.name)
                # Add scalar 'site' / 'site_id' coords so concat creates the dimension  # noqa: E501
                extra_coords = {}
                for key, value in original_row.items():
                    if key in ("site", "site_id") or key.lower() in LABEL_COL_CANDIDATES:  # noqa: E501
                        continue
                    if hasattr(value, "wkt"):  # Shapely geometry
                        value = value.wkt
                    extra_coords[key] = value
                ds = ds.assign_coords(
                    site=str(label), site_id=int(sid), **extra_coords
                ).expand_dims("site")
                datasets.append(ds)
                log.debug("  Opened netCDF for '%s': %s", label, list(ds.data_vars))  # noqa: E501
            except Exception as exc:
                log.warning("Could not open netCDF for site '%s': %s", label, exc)  # noqa: E501

        if not datasets:
            raise ValueError("No valid netCDF responses could be opened for merging.")  # noqa: E501

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

    Galaxy passes a fully managed path with an extension (e.g.
    dataset_uuid.dat) — write to it exactly as given so Galaxy can find
    the file afterwards. When called from the CLI without any
    extension, the correct format extension is appended automatically
    (e.g. safran_output → safran_output.csv).
    """
    base = Path(base_output)
    base.parent.mkdir(parents=True, exist_ok=True)
    if base.suffix:
        # Already has an extension (Galaxy .dat or explicit CLI path) → use as-is.  # noqa: E501
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
            "Extract SAFRAN climate reanalysis data from the GEOSAS OGC-EDR API\n"  # noqa: E501
            "and produce a SINGLE merged output file for all sites.\n\n"
            "SAFRAN is a French meteorological gridded reanalysis (8 km) produced\n"  # noqa: E501
            "by Météo-France, covering metropolitan France from 1958-08-01 to\n"  # noqa: E501
            "near-present (updated monthly).\n\n"
            "API : https://api.geosas.fr/edr/collections/safran-isba/\n"
            "Docs: https://geosas.fr/web/?page_id=6345\n\n"
            "Geometry routing (automatic):\n"
            "  POINT / MULTIPOINT → /position  (coords=WKT)\n"
            "  LINESTRING         → /position  (vertices → MULTIPOINT)\n"
            "  POLYGON            → /cube      (bounding box extracted)\n\n"
            "Merge strategy per format:\n"
            "  CSV / parquet  → long-format table, 'site_name' column added\n"
            "  CoverageJSON   → OGC CoverageCollection wrapping all coverages\n"  # noqa: E501
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
            "  Input file (sites.csv):\n"
            "    site_name,latitude,longitude,region\n"
            "    Montpellier,43.6047,3.8722,Occitanie\n"
            "    Paris,48.8566,2.3522,Ile-de-France\n\n"
            "  Merged output:\n"
            "    site_id,site_name,latitude,longitude,region,date,T_Q\n"
            "    0,Montpellier,43.6047,3.8722,Occitanie,1995-01-01,7.5\n"
            "    0,Montpellier,43.6047,3.8722,Occitanie,1995-01-02,2.7\n"
            "    1,Paris,48.8566,2.3522,Ile-de-France,1995-01-01,3.2\n"
            "    ...\n\n"
            "Every column from the input file is kept as-is (repeated for each\n"  # noqa: E501
            "date). 'site_id' is always a 0-based sequential integer assigned\n"  # noqa: E501
            "in row processing order — it ignores any 'id'/'site_id' column\n"
            "that may already be present in the input file. 'longitude' and\n"
            "'latitude' are added only if the input file didn't already have\n"
            "them (e.g. for POLYGON/LINESTRING inputs using a 'geometry'/'wkt'\n"  # noqa: E501
            "column instead of lat/lon).\n\n"
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
            "NOTE: liquid rain = PRELIQ_Q; min/max temp = TINF_H_Q / TSUP_H_Q. "  # noqa: E501
            "Example: --parameters T_Q ETP_Q PRELIQ_Q"
        ),
    )

    parser.add_argument("--coord-format", default=None, metavar="EXT")

    parser.add_argument(
        "--coord-file", "-c",
        required=True, metavar="FILE",
        help=(
            "Coordinate / geometry file (CSV, TSV, Excel, Shapefile, GeoPackage, GeoJSON). "  # noqa: E501
            "For CSV/Excel: 'latitude'+'longitude' columns for POINTs, "
            "or 'geometry'/'wkt' column with WKT text for polygons/lines. "
            "A 'site_name' (or 'name'/'id') column is used to label each site. "  # noqa: E501
            "Geometry type is auto-detected. "
            "Optional 'begin_date'/'end_date' columns (YYYY-MM-DD) override dates per row."  # noqa: E501
        ),
    )

    parser.add_argument(
        "--start-date", "-s",
        default=None, metavar="YYYY-MM-DD",
        help=(
            f"Global start date. Cannot be before {SAFRAN_OLDEST_DATE}. "
            f"Default if omitted and no 'begin_date' column: {DEFAULT_START_DATE}."  # noqa: E501
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
            "Default: safran_output  →  safran_output.csv (or .json / .nc / .parquet)"  # noqa: E501
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
    global_start = parse_date(args.start_date, "start-date") if args.start_date else None  # noqa: E501
    global_end = parse_date(args.end_date, "end-date") if args.end_date else None  # noqa: E501

    # ── 2. Load coordinate file ─────────────────────────────────────────────
    df = load_coordinate_file(args.coord_file)

    log.info("Dominant geometry: %s", detect_dominant_geometry(df) or "unknown")  # noqa: E501

    col_lower = {c.lower(): c for c in df.columns}
    col_begin = col_lower.get("begin_date") or col_lower.get("start_date") or col_lower.get("date_debut")  # noqa: E501
    col_end = col_lower.get("end_date") or col_lower.get("date_fin")

    # ── 3. Fetch data for every row, collect raw responses ──────────────────
    # Each chunk: (site_id, site_label, longitude, latitude, raw_bytes, original_row)  # noqa: E501
    # site_id is a 0-based sequential integer (row processing order).
    # original_row holds every column of the input coordinate/geometry file
    # for that row, so it can be preserved as-is in the merged output.
    chunks: list[tuple[int, str, float | None, float | None, bytes, dict]] = []
    errors: list[tuple[int, str]] = []

    for row_id, (idx, row) in enumerate(df.iterrows()):
        row_label = site_label(row, idx)
        log.info("── Row %s (site_id=%d) ──", row_label, row_id)

        # Resolve dates
        row_start = global_start
        row_end = global_end

        if col_begin and pd.notna(row.get(col_begin)):
            row_start = parse_date(str(row[col_begin]), f"begin_date row {idx}")  # noqa: E501
        if col_end and pd.notna(row.get(col_end)):
            row_end = parse_date(str(row[col_end]), f"end_date row {idx}")

        if row_start is None:
            if col_begin is None and global_start is None:
                parser.error(
                    "No --start-date provided and no 'begin_date' column in the file."  # noqa: E501
                )
            row_start = parse_date(DEFAULT_START_DATE)

        if row_end is None:
            if col_end is None and global_end is None:
                parser.error(
                    "No --end-date provided and no 'end_date' column in the file."  # noqa: E501
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

        # Every column of the original input file for this row, preserved as-is
        original_row = row.to_dict()

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
            chunks.append((row_id, row_label, lon, lat, raw, original_row))
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
        log.warning("%d site(s) failed and were excluded from the output:", len(errors))  # noqa: E501
        for row_id, msg in errors:
            log.warning("  Row %s — %s", row_id, msg)
        sys.exit(1)


if __name__ == "__main__":
    main()

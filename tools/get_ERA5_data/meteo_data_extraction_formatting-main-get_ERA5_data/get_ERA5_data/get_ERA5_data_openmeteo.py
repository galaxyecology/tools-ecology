#!/usr/bin/env python3
"""
era5_extractor.py — ERA5 / ERA5-Land climate data extractor
============================================================
Extract daily climate data from ERA5 and ERA5-Land reanalysis datasets
via the Open-Meteo Historical Archive API (https://archive-api.open-meteo.com).
No API key required for the free tier.  An optional --apikey enables the
commercial tier (customer-api.open-meteo.com) which has no daily quota.

Key features
------------
* Accepts any input projection (EPSG code) — auto-reprojects to WGS-84.
* Auto-detects geometry type (POINT, LINESTRING, POLYGON …) from a WKT
  'geometry' column; uses the centroid as the representative API point.
* Date range constrained to SAFRAN data availability: 1960-01-01 → 2020-12-31.
* Per-site date overrides via 'begin_date' / 'end_date' columns.
* Smart model routing: variables unavailable in era5_seamless are
  automatically requested against the correct sub-model (era5 / era5_land).
* Daily parameter always sent as a comma-joined string → correct URL encoding.
* Validates returned variables against requested ones and warns on silent drops.
* Configurable sleep between calls to respect Open-Meteo rate limits.

Dependencies
------------
  pip install requests pandas
  pip install geopandas pyproj shapely   # for WKT geometry & reprojection
  pip install pyarrow                    # for Parquet output

Author  : (fill in)
Version : 3.0.0
License : MIT
"""

from __future__ import annotations

import argparse
import sys
import time
import warnings
from datetime import date, datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import collections
import math
import numpy as np
import pandas as pd
import requests

# ── Shapely (WKT parsing + centroid) — does NOT need the PROJ database ───────
try:
    from shapely import wkt as shapely_wkt
    HAS_SHAPELY = True
except ImportError:
    HAS_SHAPELY = False

# ── geopandas + pyproj (only needed when reprojecting from a non-WGS84 CRS) ──
# These require a working PROJ database.  In some conda environments pyproj
# cannot locate it and raises CRSError even for EPSG:4326.  We guard every
# call with a has_proj check so the script still works for the common
# EPSG:4326 case even when pyproj's database is misconfigured.
try:
    import geopandas as gpd
    from pyproj import Transformer, CRS as ProjCRS
    # Smoke-test: instantiating EPSG:4326 costs nothing and catches the
    # "no database context specified" error early with a clear message.
    ProjCRS.from_epsg(4326)
    HAS_PROJ = True
except Exception:
    HAS_PROJ = False

# Backwards-compat alias used in guards below
HAS_SPATIAL = HAS_SHAPELY

# ── Optional Parquet ──────────────────────────────────────────────────────────
try:
    import pyarrow  # noqa: F401
    HAS_PARQUET = True
except ImportError:
    HAS_PARQUET = False


# ═══════════════════════════════════════════════════════════════════════════════
#  Constants
# ═══════════════════════════════════════════════════════════════════════════════

SAFRAN_START = date(1960, 1, 1)
SAFRAN_END = date(2020, 12, 31)

OPENMETEO_FREE_URL = "https://archive-api.open-meteo.com/v1/archive"
OPENMETEO_PAID_URL = "https://customer-api.open-meteo.com/v1/archive"

MAX_RETRIES = 4
RETRY_DELAYS = [5, 15, 30, 60]   # seconds per successive retry

VALID_FORMATS = ["csv", "tsv", "json", "parquet"]
_INTERNAL_MODEL = "era5_seamless"  # routing is automatic, not user-selectable

# Native grid resolutions (degrees)
ERA5_LAND_RESOLUTION = 0.1    # ~9 km — used for polygon/line grid sampling
ERA5_RESOLUTION = 0.25   # ~25 km

# How many consecutive total-failure sites trigger a "daily quota: pause until midnight UTC"
QUOTA_PAUSE_THRESHOLD = 2

# ── Complete variable catalogue ───────────────────────────────────────────────
# Each entry: internal_name → (description, best_model)
# best_model is used for smart routing when era5_seamless drops a variable.
PARAMETERS: Dict[str, Tuple[str, str]] = {
    # Temperature
    "temperature_2m_mean": ("Mean 2 m air temperature (°C)", "era5_land"),
    "temperature_2m_max": ("Max  2 m air temperature (°C)", "era5_seamless"),
    "temperature_2m_min": ("Min  2 m air temperature (°C)", "era5_seamless"),
    # Precipitation / snow
    "precipitation_sum": ("Total precipitation (mm/day)", "era5_seamless"),
    "rain_sum": ("Rainfall only (mm/day)", "era5_seamless"),
    "snowfall_sum": ("Snowfall cm/day (water-equivalent)", "era5_seamless"),
    "precipitation_hours": ("Duration of precipitation events (h/day)", "era5_seamless"),
    # Evapotranspiration
    "et0_fao_evapotranspiration": (
        "Reference ET₀ FAO-56 Penman-Monteith (mm/day)", "era5_seamless"
    ),
    # Wind
    "wind_speed_10m_mean": ("Mean 10 m wind speed (km/h)", "era5"),
    "wind_speed_10m_max": ("Max  10 m wind speed (km/h)", "era5"),
    "wind_gusts_10m_mean": ("Mean 10 m wind gusts (km/h)", "era5"),
    "wind_gusts_10m_max": ("Max  10 m wind gusts (km/h)", "era5"),
    "wind_direction_10m_dominant": ("Dominant 10 m wind direction (°, 0=N)", "era5"),
    # Radiation
    "shortwave_radiation_sum": ("Solar (shortwave) radiation sum (MJ/m²/day)", "era5"),
    "sunshine_duration": ("Sunshine duration (s/day)", "era5"),
    "daylight_duration": ("Astronomical daylight duration (s/day)", "era5"),
    # Humidity / pressure
    "relative_humidity_2m_mean": ("Mean 2 m relative humidity (%)", "era5"),
    "relative_humidity_2m_max": ("Max  2 m relative humidity (%)", "era5"),
    "relative_humidity_2m_min": ("Min  2 m relative humidity (%)", "era5"),
    "surface_pressure_mean": ("Mean surface pressure (hPa)", "era5"),
    "vapor_pressure_deficit_max": ("Max vapour pressure deficit (kPa)", "era5"),
    # Cloud cover
    "cloud_cover_mean": ("Mean total cloud cover (%)", "era5"),
    # Soil moisture — ERA5-Land only
    "soil_moisture_0_to_7cm_mean": ("Mean soil moisture   0–7 cm   (m³/m³)", "era5_land"),
    "soil_moisture_7_to_28cm_mean": ("Mean soil moisture   7–28 cm  (m³/m³)", "era5_land"),
    "soil_moisture_28_to_100cm_mean": ("Mean soil moisture  28–100 cm (m³/m³)", "era5_land"),
    "soil_moisture_100_to_255cm_mean": ("Mean soil moisture 100–255 cm (m³/m³)", "era5_land"),
    # Soil temperature — ERA5-Land only
    "soil_temperature_0_to_7cm_mean": ("Mean soil temperature   0–7 cm   (°C)", "era5_land"),
    "soil_temperature_7_to_28cm_mean": ("Mean soil temperature   7–28 cm  (°C)", "era5_land"),
    "soil_temperature_28_to_100cm_mean": ("Mean soil temperature  28–100 cm (°C)", "era5_land"),
    "soil_temperature_100_to_255cm_mean": ("Mean soil temperature 100–255 cm (°C)", "era5_land"),
}

# User-friendly aliases → canonical Open-Meteo variable names
ALIASES: Dict[str, str] = {
    # Common shorthand / SAFRAN-style names
    "2m_temperature": "temperature_2m_mean",
    "t2m": "temperature_2m_mean",
    "tmax": "temperature_2m_max",
    "tmin": "temperature_2m_min",
    "tmean": "temperature_2m_mean",
    "temp": "temperature_2m_mean",
    "precip": "precipitation_sum",
    "rr": "precipitation_sum",       # Météo-France convention
    "rain": "rain_sum",
    "snow": "snowfall_sum",
    "et0": "et0_fao_evapotranspiration",
    "evapotranspiration": "et0_fao_evapotranspiration",
    "wind": "wind_speed_10m_mean",
    "wind_speed": "wind_speed_10m_mean",
    "radiation": "shortwave_radiation_sum",
    "rg": "shortwave_radiation_sum",  # rayonnement global
    "humidity": "relative_humidity_2m_mean",
    "rh": "relative_humidity_2m_mean",
    "pressure": "surface_pressure_mean",
    "cloud": "cloud_cover_mean",
    "sm1": "soil_moisture_0_to_7cm_mean",
    "sm2": "soil_moisture_7_to_28cm_mean",
    "sm3": "soil_moisture_28_to_100cm_mean",
    "sm4": "soil_moisture_100_to_255cm_mean",
    "st1": "soil_temperature_0_to_7cm_mean",
    "st2": "soil_temperature_7_to_28cm_mean",
    "st3": "soil_temperature_28_to_100cm_mean",
    "st4": "soil_temperature_100_to_255cm_mean",
}

# Variables that are ERA5-Land only — cannot be requested via pure era5
ERA5_LAND_ONLY = {
    "soil_moisture_0_to_7cm_mean", "soil_moisture_7_to_28cm_mean",
    "soil_moisture_28_to_100cm_mean", "soil_moisture_100_to_255cm_mean",
    "soil_temperature_0_to_7cm_mean", "soil_temperature_7_to_28cm_mean",
    "soil_temperature_28_to_100cm_mean", "soil_temperature_100_to_255cm_mean",
}

# Variables best served by ERA5 (not available in era5_land)
ERA5_ONLY = {
    "wind_speed_10m_mean", "wind_speed_10m_max",
    "wind_gusts_10m_mean", "wind_gusts_10m_max",
    "wind_direction_10m_dominant",
    "shortwave_radiation_sum", "sunshine_duration", "daylight_duration",
    "relative_humidity_2m_mean", "relative_humidity_2m_max", "relative_humidity_2m_min",
    "surface_pressure_mean", "vapor_pressure_deficit_max", "cloud_cover_mean",
}


# ═══════════════════════════════════════════════════════════════════════════════
#  Logging helpers
# ═══════════════════════════════════════════════════════════════════════════════

def _ts() -> str:
    return datetime.now().strftime("%Y-%m-%d %H:%M:%S")


def log_info(msg: str) -> None:
    print(f"{_ts()} [INFO] {msg}", flush=True)


def log_warn(msg: str) -> None:
    print(f"{_ts()} [WARN] {msg}", flush=True)


def log_error(msg: str) -> None:
    print(f"{_ts()} [ERROR] {msg}", flush=True)


def log_debug(msg: str, verbose: bool = False) -> None:
    if verbose:
        print(f"{_ts()} [DEBUG] {msg}", flush=True)


# ═══════════════════════════════════════════════════════════════════════════════
#  ETA / progress helpers
# ═══════════════════════════════════════════════════════════════════════════════

def _fmt_dur(seconds: float) -> str:
    """Format a duration in seconds as h:mm:ss or m:ss."""
    seconds = max(0.0, seconds)
    h = int(seconds // 3600)
    m = int((seconds % 3600) // 60)
    s = int(seconds % 60)
    if h:
        return f"{h}h{m:02d}m{s:02d}s"
    if m:
        return f"{m}m{s:02d}s"
    return f"{s}s"


def _count_api_calls(n_sites: int, parameters: List[str]) -> int:
    """
    Return the total number of HTTP calls that will be made.

    Each site generates len(_group_by_model(parameters, model)) calls
    (one per sub-model group when smart routing splits ERA5 / ERA5-Land).
    """
    return n_sites * len(_group_by_model(parameters))


def _print_eta(
    call_idx: int,
    total_calls: int,
    run_start: float,
    recent_dur: "collections.deque[float]",
) -> None:
    """
    Print a one-line ETA summary after each completed API call.

      Appels : 3/12 (25%) | Écoulé : 0m04s | ETA : ~0m14s | Moy : 1.4s/appel
    """
    elapsed = time.perf_counter() - run_start
    pct = 100.0 * call_idx / total_calls if total_calls else 0
    avg = sum(recent_dur) / len(recent_dur) if recent_dur else 0.0
    rem = (total_calls - call_idx) * avg
    eta_str = f"~{_fmt_dur(rem)}" if recent_dur else "calcul en cours…"
    print(
        f"{_ts()} [ETA ] "
        f"Appels : {call_idx}/{total_calls} ({pct:.0f}%) | "
        f"Écoulé : {_fmt_dur(elapsed)} | "
        f"ETA : {eta_str} | "
        f"Moy : {avg:.1f}s/appel",
        flush=True,
    )


def _seconds_until_midnight_utc(buffer_minutes: int = 5) -> float:
    """
    Return the number of seconds until the next midnight UTC,
    plus *buffer_minutes* of safety margin.

    Open-Meteo resets the daily quota at 00:00 UTC.
    The buffer avoids hitting the API the instant it resets.
    """
    from datetime import datetime as _dt, timezone, timedelta
    now = _dt.now(timezone.utc)
    tonight = now.replace(hour=0, minute=0, second=0, microsecond=0)
    midnight = tonight + timedelta(days=1)
    return max(0.0, (midnight - now).total_seconds() + buffer_minutes * 60)


def _wait_for_quota_reset(verbose: bool = False) -> None:
    """
    Pause jusqu'au prochain minuit UTC (reset du quota Open-Meteo),
    en affichant un décompte toutes les 10 minutes.

    Le script NE s'arrête PAS — il reprend automatiquement dès que
    le quota est remis à zéro.
    """
    from datetime import datetime as _dt, timezone as _tz
    wait_s = _seconds_until_midnight_utc(buffer_minutes=5)
    wake_utc = _dt.now(_tz.utc).timestamp() + wait_s
    wake_str = _dt.fromtimestamp(wake_utc, tz=_tz.utc).strftime("%Y-%m-%d %H:%M UTC")

    log_warn(
        "⛔ Quota journalier Open-Meteo épuisé (IP bloquée).\n"
        "  Le quota se remet à zéro à minuit UTC.\n"
        f"  Reprise automatique à : {wake_str}  "
        f"(dans {_fmt_dur(wait_s)})"
    )

    # Décompte toutes les 10 minutes
    interval = 600   # 10 min
    elapsed = 0.0
    while elapsed < wait_s:
        chunk = min(interval, wait_s - elapsed)
        time.sleep(chunk)
        elapsed += chunk
        remaining = wait_s - elapsed
        if remaining > 0:
            log_info(
                f"  ⏳ Pause quota — reprise dans {_fmt_dur(remaining)} "
                f"({wake_str})"
            )

    log_info("  ✅ Quota remis à zéro — reprise de l'extraction.")


# ═══════════════════════════════════════════════════════════════════════════════
#  Date helpers
# ═══════════════════════════════════════════════════════════════════════════════

def parse_date(s: str, label: str = "date") -> date:
    """Parse a YYYY-MM-DD string, exit with a clear message on failure."""
    try:
        return datetime.strptime(s.strip(), "%Y-%m-%d").date()
    except ValueError:
        sys.exit(
            f"[ERROR] Invalid {label} '{s}'. "
            "Expected format: YYYY-MM-DD (e.g. 1990-06-15)."
        )


def validate_date_range(start: date, end: date, label: str = "") -> None:
    """
    Enforce SAFRAN bounds (1960-01-01 → 2020-12-31) and chronological order.
    """
    ctx = f"[{label}] " if label else ""
    if start < SAFRAN_START:
        sys.exit(
            f"[ERROR] {ctx}Start date {start} predates the oldest available "
            f"SAFRAN-compatible data ({SAFRAN_START})."
        )
    if end > SAFRAN_END:
        sys.exit(
            f"[ERROR] {ctx}End date {end} exceeds the latest available "
            f"SAFRAN-compatible data ({SAFRAN_END})."
        )
    if start > end:
        sys.exit(
            f"[ERROR] {ctx}Start date ({start}) is after end date ({end})."
        )


# ═══════════════════════════════════════════════════════════════════════════════
#  Alias / parameter resolution
# ═══════════════════════════════════════════════════════════════════════════════

def resolve_parameters(raw: List[str]) -> List[str]:
    """
    Expand aliases, de-duplicate, and validate parameter names.

    Accepts both space-separated tokens (CLI) and comma-separated tokens
    (Galaxy XML multi-select).
    """
    resolved: List[str] = []
    unknown: List[str] = []

    for tok in raw:
        for name in tok.split(","):
            name = name.strip()
            if not name:
                continue
            canonical = ALIASES.get(name.lower(), name)
            if canonical not in PARAMETERS:
                unknown.append(name)
            elif canonical not in resolved:
                resolved.append(canonical)

    if unknown:
        sys.exit(
            f"[ERROR] Unrecognised parameter(s): {', '.join(unknown)}\n"
            "Run  --list-parameters  to see the full catalogue."
        )
    return resolved


def _group_by_model(parameters: List[str], _model: str = _INTERNAL_MODEL) -> Dict[str, List[str]]:
    """
    Split *parameters* into sub-groups by their best sub-model
    (era5 for atmospheric variables, era5_land for soil variables).
    Routing is fully automatic — the user never needs to choose.
    """
    groups: Dict[str, List[str]] = {}
    for p in parameters:
        best = PARAMETERS[p][1]
        groups.setdefault(best, []).append(p)
    return groups


# ═══════════════════════════════════════════════════════════════════════════════
#  Coordinate / geometry helpers
# ═══════════════════════════════════════════════════════════════════════════════

def _geom_type_from_wkt(wkt_str: str) -> str:
    """Return a normalised geometry type tag from a WKT prefix."""
    upper = wkt_str.strip().upper()
    for tag in ("MULTIPOLYGON", "MULTILINESTRING", "MULTIPOINT",
                "POLYGON", "LINESTRING", "POINT", "GEOMETRYCOLLECTION"):
        if upper.startswith(tag):
            return tag
    return "GEOMETRY"


def _centroid_wgs84_from_wkt(wkt_str: str, src_epsg: int) -> Tuple[float, float]:
    """
    Parse WKT, reproject to WGS-84 if needed, return (lat, lon) of centroid.

    EPSG:4326 path  → pure shapely only (no PROJ database required).
    Other CRS paths → geopandas + pyproj (PROJ database must be available).

    This separation avoids the pyproj "no database context specified" crash
    that occurs in some conda environments even for the trivial 4326 case.
    """
    if not HAS_SHAPELY:
        sys.exit(
            "[ERROR] shapely is required for WKT geometry handling.\n"
            "        Install with:  pip install shapely"
        )
    geom = shapely_wkt.loads(wkt_str)

    if src_epsg == 4326:
        # Fast path: no reprojection needed — pure shapely, zero pyproj calls.
        c = geom.centroid
        return float(c.y), float(c.x)   # WKT in 4326: x=lon, y=lat

    # Reprojection path: needs a working PROJ database.
    if not HAS_PROJ:
        sys.exit(
            f"[ERROR] Reprojection from EPSG:{src_epsg} → EPSG:4326 requires "
            "pyproj with a working PROJ database, but pyproj could not locate "
            "its database in this environment.\n"
            "  Fix options:\n"
            "  1. Set the PROJ_DATA / PROJ_LIB environment variable to the "
            "directory that contains proj.db.\n"
            "     Example (conda):  export PROJ_DATA=$(python -c \"import pyproj; "
            "import os; print(os.path.dirname(pyproj.datadir.get_data_dir()))\")\n"
            "  2. Reinstall pyproj:  conda install -c conda-forge pyproj\n"
            "  3. If your coordinates are already in WGS-84, omit --epsg "
            "(default is 4326)."
        )
    gdf = gpd.GeoDataFrame(geometry=[geom], crs=f"EPSG:{src_epsg}")
    gdf = gdf.to_crs(epsg=4326)
    c = gdf.geometry.iloc[0].centroid
    return float(c.y), float(c.x)


def _reproject_point(lat: float, lon: float, src_epsg: int) -> Tuple[float, float]:
    """
    Reproject a (lat, lon) point from *src_epsg* to WGS-84 (EPSG:4326).

    EPSG:4326 → identity (no pyproj call at all).
    Other CRS → pyproj Transformer (PROJ database must be available).
    """
    if src_epsg == 4326:
        return lat, lon   # nothing to do; no PROJ database needed

    if not HAS_PROJ:
        sys.exit(
            f"[ERROR] Reprojection from EPSG:{src_epsg} → EPSG:4326 requires "
            "a working pyproj / PROJ installation.\n"
            "  Fix options:\n"
            "  1. export PROJ_DATA=<path-to-proj-data-dir>\n"
            "  2. conda install -c conda-forge pyproj\n"
            "  3. If coordinates are already WGS-84, use --epsg 4326 (default)."
        )
    transformer = Transformer.from_crs(
        f"EPSG:{src_epsg}", "EPSG:4326", always_xy=True
    )
    lon84, lat84 = transformer.transform(lon, lat)
    return float(lat84), float(lon84)


def _grid_points_in_polygon(
    wkt_str: str, src_epsg: int, resolution: float = ERA5_LAND_RESOLUTION
) -> List[Tuple[float, float]]:
    """
    Generate a regular grid of (lat, lon) points inside *wkt_str* at
    *resolution* degrees (default: ERA5-Land native grid of 0.1°).

    The grid is snapped to multiples of *resolution* so that returned
    coordinates align with the actual ERA5 / ERA5-Land grid cells.

    Requires shapely and numpy.
    """
    if not HAS_SHAPELY:
        sys.exit("[ERROR] shapely is required for polygon grid sampling.")

    # Reproject to WGS-84 first if needed
    geom_wgs84_wkt = wkt_str
    if src_epsg != 4326:
        if not HAS_PROJ:
            sys.exit(
                f"[ERROR] Reprojection from EPSG:{src_epsg} requires pyproj.\n"
                "        conda install -c conda-forge pyproj"
            )
        gdf = gpd.GeoDataFrame(
            geometry=[shapely_wkt.loads(wkt_str)], crs=f"EPSG:{src_epsg}"
        )
        gdf = gdf.to_crs(epsg=4326)
        geom_wgs84_wkt = gdf.geometry.iloc[0].wkt

    from shapely.geometry import Point as _Point
    poly = shapely_wkt.loads(geom_wgs84_wkt)
    minx, miny, maxx, maxy = poly.bounds

    # Snap grid origin to multiples of resolution
    lon_start = math.floor(minx / resolution) * resolution
    lat_start = math.floor(miny / resolution) * resolution

    lons = np.arange(lon_start, maxx + resolution, resolution)
    lats = np.arange(lat_start, maxy + resolution, resolution)

    points = [
        (round(float(lat), 6), round(float(lon), 6))
        for lat in lats
        for lon in lons
        if poly.contains(_Point(lon, lat))
    ]

    if not points:
        # Fallback: polygon smaller than one grid cell → use centroid
        c = poly.centroid
        points = [(round(float(c.y), 6), round(float(c.x), 6))]
        log_warn(
            "Polygon contains 0 grid points at resolution "
            f"{resolution}° — falling back to centroid "
            f"({points[0][0]}, {points[0][1]})."
        )

    return points


def resolve_geometry_to_points(
    row: pd.Series, epsg: int, verbose: bool = False
) -> Tuple[List[Tuple[float, float, str]], str]:
    """
    Return ([(lat, lon, point_label), …], geometry_type) for one row.

    Geometry routing — identical logic to safran_extract.py:
      POINT      → [(centroid_lat, centroid_lon, '')]           1 point
      MULTIPOINT → one (lat, lon) per component point           N points
      LINESTRING → one (lat, lon) per vertex                    N points
      POLYGON /
      MULTIPOLYGON → regular ERA5-Land grid (0.1°) inside poly  N points

    For POINT inputs without a 'geometry' column, the 'latitude'/'longitude'
    columns are used as before.
    """
    # ── 1. Resolve raw geometry ───────────────────────────────────────────────
    if "geometry" in row.index:
        raw = row["geometry"]
        if pd.notna(raw) and str(raw).strip():
            wkt_str = str(raw).strip()
            gtype = _geom_type_from_wkt(wkt_str)

            # ── POINT ─────────────────────────────────────────────────────────
            if gtype == "POINT":
                lat, lon = _centroid_wgs84_from_wkt(wkt_str, epsg)
                log_debug(f"POINT → lat={lat:.4f} lon={lon:.4f}", verbose)
                return [(lat, lon, "")], "POINT"

            # ── MULTIPOINT ────────────────────────────────────────────────────
            if gtype == "MULTIPOINT":
                if not HAS_SHAPELY:
                    sys.exit("[ERROR] shapely required for MULTIPOINT.")
                mp = shapely_wkt.loads(wkt_str)
                pts_raw = list(mp.geoms)
                if epsg != 4326:
                    gdf = gpd.GeoDataFrame(geometry=pts_raw, crs=f"EPSG:{epsg}")
                    gdf = gdf.to_crs(epsg=4326)
                    pts_raw = list(gdf.geometry)
                pts = [
                    (round(float(p.y), 6), round(float(p.x), 6), f"pt{i + 1}")
                    for i, p in enumerate(pts_raw)
                ]
                log_debug(f"MULTIPOINT → {len(pts)} point(s)", verbose)
                return pts, "MULTIPOINT"

            # ── LINESTRING — each vertex becomes a query point ────────────────
            if gtype == "LINESTRING":
                if not HAS_SHAPELY:
                    sys.exit("[ERROR] shapely required for LINESTRING.")
                line = shapely_wkt.loads(wkt_str)
                if epsg != 4326:
                    gdf = gpd.GeoDataFrame(
                        geometry=[line], crs=f"EPSG:{epsg}"
                    )
                    gdf = gdf.to_crs(epsg=4326)
                    line = gdf.geometry.iloc[0]
                pts = [
                    (round(float(lat), 6), round(float(lon), 6), f"v{i + 1}")
                    for i, (lon, lat) in enumerate(line.coords)
                ]
                log_debug(f"LINESTRING → {len(pts)} vertex/vertices", verbose)
                return pts, "LINESTRING"

            # ── POLYGON / MULTIPOLYGON — ERA5-Land grid inside polygon ─────────
            if gtype in ("POLYGON", "MULTIPOLYGON"):
                pts_ll = _grid_points_in_polygon(
                    wkt_str, epsg, ERA5_LAND_RESOLUTION
                )
                pts = [
                    (lat, lon, f"cell_{i + 1}")
                    for i, (lat, lon) in enumerate(pts_ll)
                ]
                log_info(
                    f"  {gtype} → grille ERA5-Land 0.1° : "
                    f"{len(pts)} point(s) dans la géométrie."
                )
                return pts, gtype

    # ── 2. Fallback: latitude + longitude columns (POINT) ────────────────────
    missing = [c for c in ("latitude", "longitude") if c not in row.index]
    if missing:
        sys.exit(
            f"[ERROR] Column(s) {missing} absent from the coordinate file.\n"
            "        Provide 'latitude'+'longitude' columns  OR  a 'geometry' "
            "column with WKT strings."
        )
    try:
        raw_lat = float(row["latitude"])
        raw_lon = float(row["longitude"])
    except (ValueError, TypeError) as exc:
        sys.exit(f"[ERROR] Cannot parse lat/lon from row — {exc}\n  {dict(row)}")

    lat, lon = _reproject_point(raw_lat, raw_lon, epsg)
    return [(lat, lon, "")], "POINT"


# ═══════════════════════════════════════════════════════════════════════════════
#  Open-Meteo API
# ═══════════════════════════════════════════════════════════════════════════════

def _build_request_params(
    lat: float,
    lon: float,
    start: date,
    end: date,
    variables: List[str],
    model: str,
    api_key: Optional[str] = None,
) -> Tuple[str, dict]:
    """
    Build the (url, params) pair for a single Open-Meteo archive request.

    api_key=None → free tier  (archive-api.open-meteo.com)
    api_key=str  → paid tier  (customer-api.open-meteo.com, no daily quota)

    The 'daily' parameter is always a comma-joined STRING (not a list) to
    ensure correct URL encoding: ?daily=v1,v2 rather than ?daily=v1&daily=v2.
    """
    url = OPENMETEO_PAID_URL if api_key else OPENMETEO_FREE_URL
    params: dict = {
        "latitude": round(lat, 6),
        "longitude": round(lon, 6),
        "start_date": start.isoformat(),
        "end_date": end.isoformat(),
        "daily": ",".join(variables),
        "timezone": "auto",
        "models": model,
    }
    if api_key:
        params["apikey"] = api_key
    return url, params


def _parse_daily(
    data: dict,
    requested: List[str],
    lat: float,
    lon: float,
    label: str,
    verbose: bool,
) -> pd.DataFrame:
    """
    Parse the 'daily' block of an Open-Meteo response.

    FIX #2 — validate that requested variables were actually returned and
              emit a warning for any that were silently dropped by the API.

    FIX #3 — handle the case where 'daily' exists at the top level but is
              an empty dict (variable not available for that model/range).
    """
    daily = data.get("daily")

    # ── FIX #3: distinguish three failure modes ───────────────────────────────
    if daily is None:
        # Key completely absent
        log_error(
            f"No 'daily' key in API response for {label} | "
            f"lat={lat:.4f} lon={lon:.4f}.\n"
            f"  Top-level keys received: {list(data.keys())}\n"
            "  This usually means the request was malformed."
        )
        return pd.DataFrame()

    if not isinstance(daily, dict) or not daily:
        # Key present but empty dict / null
        log_error(
            f"'daily' block is empty for {label} | lat={lat:.4f} lon={lon:.4f}.\n"
            f"  This means NONE of the requested variables [{', '.join(requested)}] "
            "are available for the chosen model and date range.\n"
            "  → Check that the variable is available for its assigned model, "
            "or run --list-parameters to review the catalogue."
        )
        return pd.DataFrame()

    if "time" not in daily or not daily["time"]:
        log_error(
            f"'daily' block has no 'time' series for {label} | "
            f"lat={lat:.4f} lon={lon:.4f}.\n"
            f"  daily keys returned: {list(daily.keys())}"
        )
        return pd.DataFrame()

    # ── FIX #2: warn on silently-dropped variables ────────────────────────────
    dropped = [v for v in requested if v not in daily]
    if dropped:
        log_warn(
            f"Variable(s) silently dropped by the API for {label}: {dropped}\n"
            "  Possible causes: variable not available for this model / "
            "land-only variable queried over ocean / invalid name.\n"
            "  Check https://open-meteo.com/en/docs/historical-weather-api"
        )

    df = pd.DataFrame(daily)
    df.rename(columns={"time": "date"}, inplace=True)
    df["date"] = pd.to_datetime(df["date"]).dt.date

    log_debug(
        f"  Parsed {len(df)} rows, {len(df.columns) - 1} variable(s): "
        f"{[c for c in df.columns if c != 'date']}",
        verbose,
    )
    return df


def fetch_openmeteo(
    lat: float,
    lon: float,
    start: date,
    end: date,
    parameters: List[str],
    sleep_s: float,
    api_key: Optional[str] = None,
    verbose: bool = False,
) -> Tuple[pd.DataFrame, float, bool]:
    """
    Query the Open-Meteo Historical Archive API for one location.

    Handles smart model routing (era5_seamless → era5 / era5_land grouping),
    retries on 429 / 5xx, and merges multi-model results on the 'date' column.

    Parameters
    ----------
    api_key : None → free tier (10 000 req/day, per-IP quota).
              str  → paid tier (customer-api.open-meteo.com, no daily quota).

    Returns
    -------
    (DataFrame, elapsed_seconds, all_429)
        all_429 signals daily-quota exhaustion on the free tier.
        Always False when api_key is provided (paid tier has no quota).
    """
    # ── Group variables by sub-model ──────────────────────────────────────────
    model_groups = _group_by_model(parameters)

    if verbose and len(model_groups) > 1:
        log_info(
            f"  Smart routing: splitting {len(parameters)} variable(s) across "
            f"{len(model_groups)} model call(s): "
            + "  |  ".join(f"{m}: {vs}" for m, vs in model_groups.items())
        )

    frames: List[pd.DataFrame] = []
    total_elapsed: float = 0.0
    site_all_429: bool = True   # True until at least one call succeeds or non-429 fails

    for model, variables in model_groups.items():
        url, params = _build_request_params(
            lat, lon, start, end, variables, model, api_key=api_key
        )

        log_debug(f"  API URL → {url}?{_params_to_qs(params)}", verbose)

        df, elapsed, was_all_429 = _call_with_retry(
            url=url,
            params=params,
            requested=variables,
            lat=lat, lon=lon,
            label=f"{model}",
            sleep_s=sleep_s,
            verbose=verbose,
        )
        total_elapsed += elapsed
        site_all_429 = site_all_429 and was_all_429
        if not df.empty:
            frames.append(df)

    if not frames:
        return pd.DataFrame(), total_elapsed, site_all_429

    if len(frames) == 1:
        return frames[0], total_elapsed, False   # at least one call succeeded

    # Merge multiple sub-model results on date
    merged = frames[0]
    for other in frames[1:]:
        merged = pd.merge(merged, other, on="date", how="outer")
    merged.sort_values("date", inplace=True)
    return merged, total_elapsed, False


def _params_to_qs(params: dict) -> str:
    """Format a params dict as a readable query string (for debug logs)."""
    return "&".join(f"{k}={v}" for k, v in params.items())


def _call_with_retry(
    url: str,
    params: dict,
    requested: List[str],
    lat: float,
    lon: float,
    label: str,
    sleep_s: float,
    verbose: bool,
) -> Tuple[pd.DataFrame, float, bool]:
    """
    HTTP GET with exponential back-off on 429 / 5xx.

    Parameters
    ----------
    url     : API endpoint (free or paid tier).
    params  : query parameters dict (already includes apikey if paid).

    Returns
    -------
    (DataFrame, elapsed_seconds, all_429)
        all_429 is True when every single attempt received HTTP 429 — used
        to detect daily quota exhaustion on the free tier.
        On the paid tier this signal is ignored by the caller.
    """
    t0 = time.perf_counter()
    last_exc: Optional[Exception] = None
    all_429 = True   # flips to False on any non-429 outcome

    for attempt in range(MAX_RETRIES):
        wait = RETRY_DELAYS[min(attempt, len(RETRY_DELAYS) - 1)]
        time.sleep(sleep_s)
        try:
            resp = requests.get(url, params=params, timeout=90)
            resp.raise_for_status()
            data = resp.json()
            elapsed = time.perf_counter() - t0
            all_429 = False
            return _parse_daily(data, requested, lat, lon, label, verbose), elapsed, False

        except requests.exceptions.HTTPError as exc:
            status = exc.response.status_code if exc.response is not None else 0
            body = exc.response.text[:400] if exc.response is not None else ""
            last_exc = exc

            try:
                err_json = exc.response.json()
                api_msg = err_json.get("reason", body)
            except Exception:
                api_msg = body

            if status == 429 or status >= 500:
                if status != 429:
                    all_429 = False
                if status == 429:
                    log_warn(
                        "⚠ Quota API atteint (HTTP 429) — tentative "
                        f"{attempt + 1}/{MAX_RETRIES} pour '{label}'.\n"
                        f"  Pause de {wait}s avant la reprise "
                        "(l'ETA sera révisée après cette attente)."
                    )
                else:
                    log_warn(
                        f"HTTP {status} — tentative {attempt + 1}/{MAX_RETRIES} "
                        f"pour '{label}'. Pause {wait}s… ({api_msg})"
                    )
                time.sleep(wait)
                continue

            all_429 = False
            if status == 400:
                log_error(
                    f"HTTP 400 pour '{label}': {api_msg}\n"
                    "  → Cause probable : nom de variable invalide pour ce modèle.\n"
                    "  → Lancez --list-parameters pour voir les noms valides."
                )
                return pd.DataFrame(), time.perf_counter() - t0, False

            log_error(f"HTTP {status} pour '{label}': {api_msg}")
            return pd.DataFrame(), time.perf_counter() - t0, False

        except requests.exceptions.RequestException as exc:
            all_429 = False
            last_exc = exc
            log_warn(
                f"Erreur réseau — tentative {attempt + 1}/{MAX_RETRIES} : {exc}.\n"
                f"  Nouvelle tentative dans {wait}s…"
            )
            time.sleep(wait)

    log_error(
        f"Abandon après {MAX_RETRIES} tentatives pour '{label}'. "
        f"Dernière erreur : {last_exc}"
    )
    # all_429=True here means every single attempt got a 429 → daily quota signal
    return pd.DataFrame(), time.perf_counter() - t0, all_429


# ═══════════════════════════════════════════════════════════════════════════════
#  Coordinate-file reader
# ═══════════════════════════════════════════════════════════════════════════════

def read_coord_file(path: str) -> pd.DataFrame:
    """
    Read a CSV or TSV coordinate file.

    Required (one of):
      • 'latitude' + 'longitude'  — decimal degrees in the projection given
        by --epsg; POINT geometry assumed.
      • 'geometry'                — WKT string; any geometry; projection
        given by --epsg.

    Optional:
      • 'site_name'  — label used in output and logs (default: row_N).
      • 'begin_date' — per-site start (YYYY-MM-DD); overrides --start-date.
      • 'end_date'   — per-site end   (YYYY-MM-DD); overrides --end-date.
    """
    p = Path(path)
    if not p.exists():
        sys.exit(f"[ERROR] Coordinate file not found: '{path}'")

    sep = "\t" if p.suffix.lower() in {".tsv", ".txt"} else ","
    try:
        df = pd.read_csv(p, sep=sep, dtype=str)
    except Exception as exc:
        sys.exit(f"[ERROR] Cannot read coordinate file '{path}': {exc}")

    df.columns = df.columns.str.strip()

    has_latlon = {"latitude", "longitude"}.issubset(df.columns)
    has_geom = "geometry" in df.columns

    if not has_latlon and not has_geom:
        sys.exit(
            "[ERROR] The coordinate file must contain either:\n"
            "  • 'latitude' AND 'longitude' columns, OR\n"
            "  • a 'geometry' column with WKT strings.\n"
            f"  Columns found: {list(df.columns)}"
        )
    if df.empty:
        sys.exit("[ERROR] Coordinate file has no data rows.")

    return df


# ═══════════════════════════════════════════════════════════════════════════════
#  Output writer
# ═══════════════════════════════════════════════════════════════════════════════

def write_output(df: pd.DataFrame, path: str, fmt: str) -> None:
    """Write *df* to *path* in the requested format."""
    fmt_l = fmt.lower()
    out = Path(path)
    out.parent.mkdir(parents=True, exist_ok=True)

    if fmt_l == "csv":
        df.to_csv(out, index=False)
    elif fmt_l == "tsv":
        df.to_csv(out, index=False, sep="\t")
    elif fmt_l == "json":
        df.to_json(out, orient="records", date_format="iso", indent=2,
                   force_ascii=False)
    elif fmt_l == "parquet":
        if not HAS_PARQUET:
            sys.exit(
                "[ERROR] pyarrow is required for Parquet output.\n"
                "        Install with:  pip install pyarrow"
            )
        df = df.copy()
        df["date"] = pd.to_datetime(df["date"])
        df.to_parquet(out, index=False)
    else:
        sys.exit(
            f"[ERROR] Unsupported output format '{fmt}'. "
            f"Choices: {VALID_FORMATS}."
        )

    log_info(f"Output → {out.resolve()}  ({len(df):,} rows × {len(df.columns)} cols)")


# ═══════════════════════════════════════════════════════════════════════════════
#  Argument parser
# ═══════════════════════════════════════════════════════════════════════════════

_PARAM_TABLE = "\n".join(
    f"  {k:<46} {v[0]}"
    for k, v in PARAMETERS.items()
)
_ALIAS_TABLE = "\n".join(
    f"  {a:<28} → {c}"
    for a, c in ALIASES.items()
)

_EPILOG = f"""\
AVAILABLE CLIMATE PARAMETERS
─────────────────────────────────────────────────────────────────────────────
{_PARAM_TABLE}

SHORTHAND ALIASES  (case-insensitive)
─────────────────────────────────────────────────────────────────────────────
{_ALIAS_TABLE}

BUGS FIXED IN v2.0.0
─────────────────────────────────────────────────────────────────────────────
  #1  'daily' parameter now always sent as a comma-joined string, not a list.
      (requests encodes list as ?daily=v1&daily=v2 which Open-Meteo ignores)
  #2  Silent variable drops are now detected and reported with guidance.
  #3  Empty 'daily' block is handled with a descriptive error instead of a
      cryptic crash.
  #4  Smart model routing: era5_seamless requests are automatically split so
      ERA5-Land-only (soil moisture/temperature) and ERA5-only (wind, radiation)
      variables each go to the correct sub-model endpoint, then merged on date.

EXAMPLES
─────────────────────────────────────────────────────────────────────────────
  # 1. Basic — temperature + precip for POINT sites in WGS-84
  python era5_extractor.py \\
      --coordinates stations_simple.csv \\
      --parameters temperature_2m_mean precipitation_sum et0_fao_evapotranspiration \\
      --start-date 1980-01-01 --end-date 1980-03-31 \\
      --output outputs/climate.csv

  # 2. Use aliases (compatible with Météo-France naming)
  python era5_extractor.py \\
      --coordinates stations_simple.csv \\
      --parameters tmean rr et0 \\
      --start-date 1990-01-01 --end-date 2000-12-31

  # 3. Soil layers — automatically routed to the ERA5-Land sub-model
  python era5_extractor.py \\
      --coordinates stations_simple.csv \\
      --parameters sm1 sm2 st1 st2 \\
      --start-date 1980-01-01 --end-date 2020-12-31 \\
      --output outputs/soil.csv --verbose

  # 4. Lambert-93 input coordinates (EPSG:2154)
  python era5_extractor.py \\
      --coordinates lambert93_sites.csv --epsg 2154 \\
      --parameters temperature_2m_mean precipitation_sum \\
      --start-date 2000-01-01 --end-date 2010-12-31 \\
      --output-format tsv --output climate.tsv

  # 5. Polygon / watershed file (geometry auto-detected, centroid used)
  python era5_extractor.py \\
      --coordinates watersheds.csv \\
      --parameters precipitation_sum shortwave_radiation_sum \\
      --start-date 1960-01-01 --end-date 2020-12-31 \\
      --output outputs/watersheds.json --output-format json --verbose

  # 6. Per-site dates in the CSV (no --start-date / --end-date needed)
  python era5_extractor.py \\
      --coordinates sites_with_dates.csv \\
      --parameters temperature_2m_mean precipitation_sum

  # 7. Print all available parameters and exit
  python era5_extractor.py --list-parameters

CITATIONS
─────────────────────────────────────────────────────────────────────────────
  Open-Meteo Historical Weather API
    Zippenfenig, P. (2023). Open-Meteo.com Weather API [Computer software].
    Zenodo. https://doi.org/10.5281/zenodo.7970649

  ERA5 global reanalysis
    Hersbach, H., Bell, B., Berrisford, P., et al. (2020). The ERA5 global
    reanalysis. Q. J. R. Meteorol. Soc., 146(730), 1999-2049.
    https://doi.org/10.1002/qj.3803

  ERA5-Land reanalysis
    Munoz-Sabater, J., Dutra, E., Agusti-Panareda, A., et al. (2021).
    ERA5-Land: A state-of-the-art global reanalysis dataset for land
    applications. Earth Syst. Sci. Data, 13(9), 4349-4383.
    https://doi.org/10.5194/essd-13-4349-2021
"""


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="era5_extractor.py",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description=__doc__,
        epilog=_EPILOG,
    )

    # ── Core inputs ───────────────────────────────────────────────────────────
    parser.add_argument(
        "--coordinates", "--coord-file", "-c",
        required=True,
        metavar="FILE",
        dest="coord_file",
        help=(
            "Path to the coordinate file (CSV or TSV). "
            "REQUIRED CONTENT — one of: "
            "(a) columns 'latitude' + 'longitude' (decimal degrees in the CRS "
            "given by --epsg; POINT geometry assumed), or "
            "(b) column 'geometry' (WKT string; any geometry type; CRS given "
            "by --epsg). "
            "OPTIONAL COLUMNS — "
            "'site_name': human-readable row label (default: row_N); "
            "'begin_date' (YYYY-MM-DD): per-site start, overrides --start-date; "
            "'end_date'   (YYYY-MM-DD): per-site end,   overrides --end-date. "
            "The script fails if a date is missing both here and on the command line."
        ),
    )

    parser.add_argument(
        "--parameters", "-p",
        required=True,
        nargs="+",
        metavar="PARAM",
        help=(
            "One or more climate variables to download (space- or "
            "comma-separated). Accepts both full variable names "
            "(e.g. temperature_2m_mean) and shorthand aliases (e.g. tmean, rr). "
            "Run --list-parameters for the complete catalogue and alias table."
        ),
    )

    # ── Dates ─────────────────────────────────────────────────────────────────
    parser.add_argument(
        "--start-date", "-s",
        default=None,
        metavar="YYYY-MM-DD",
        help=(
            "Global extraction start date. "
            "Minimum: 1960-01-01 (oldest SAFRAN-compatible data). "
            "Overridden per row by a 'begin_date' column in the coordinate file. "
            "REQUIRED if 'begin_date' is absent from the coordinate file."
        ),
    )

    parser.add_argument(
        "--end-date", "-e",
        default=None,
        metavar="YYYY-MM-DD",
        help=(
            "Global extraction end date. "
            "Maximum: 2020-12-31 (latest SAFRAN-compatible data). "
            "Overridden per row by an 'end_date' column in the coordinate file. "
            "REQUIRED if 'end_date' is absent from the coordinate file."
        ),
    )

    # ── Coordinate system ─────────────────────────────────────────────────────
    parser.add_argument(
        "--epsg",
        type=int,
        default=4326,
        metavar="CODE",
        help=(
            "EPSG code of the coordinate reference system used in the input "
            "file (default: 4326 = WGS-84 geographic, decimal degrees). "
            "Coordinates are automatically reprojected to WGS-84 before the "
            "API call; the output always stores WGS-84 lat/lon. "
            "Examples: 2154 (RGF93/Lambert-93, France), "
            "32631 (WGS84/UTM zone 31N), 27563 (NTF/Lambert zone III), "
            "3035 (ETRS89/LAEA Europe). "
            "Reprojection requires geopandas + pyproj."
        ),
    )

    # ── Output ────────────────────────────────────────────────────────────────
    parser.add_argument(
        "--output-format", "-f",
        default="csv",
        choices=VALID_FORMATS,
        metavar="FORMAT",
        help=(
            "Output file format (default: csv). "
            "csv     — comma-separated, UTF-8. "
            "tsv     — tab-separated. "
            "json    — JSON array of records. "
            "parquet — Apache Parquet binary (requires pyarrow). "
            f"Choices: {VALID_FORMATS}."
        ),
    )

    parser.add_argument(
        "--output", "-o",
        default=None,
        metavar="FILE",
        help=(
            "Output file path (default: era5_data.<format>). "
            "Parent directories are created automatically."
        ),
    )

    # ── Misc ──────────────────────────────────────────────────────────────────
    parser.add_argument(
        "--apikey",
        default=None,
        metavar="KEY",
        help=(
            "Open-Meteo API key for the commercial tier (optional). "
            "Without a key the free tier is used (archive-api.open-meteo.com), "
            "limited to 10 000 requests per day per IP address — "
            "unsuitable for shared compute environments such as Galaxy. "
            "With a key the paid endpoint is used (customer-api.open-meteo.com): "
            "no daily quota, dedicated servers, 99.9%% uptime SLA. "
            "Plans start at ~30 EUR/month (cancellable). "
            "Get a key at https://open-meteo.com/en/pricing. "
            "When deployed on Galaxy, the admin can inject this via job_conf.xml "
            "so individual users do not need to handle it."
        ),
    )

    parser.add_argument(
        "--sleep",
        type=float,
        default=1.0,
        metavar="SECONDS",
        help=(
            "Seconds to sleep between API calls (default: 1.0). "
            "Increase to ≥0.1 s to stay within Open-Meteo free-tier limits "
            "(≤600 req/min, ≤5 000/hour, ≤10 000/day). "
            "Set to 0 only on private / commercial API plans."
        ),
    )

    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help=(
            "Print per-site progress details, API URLs, and model-routing "
            "information to standard output."
        ),
    )

    parser.add_argument(
        "--list-parameters",
        action="store_true",
        help="Print all available parameters (with aliases) and exit.",
    )

    return parser


# ═══════════════════════════════════════════════════════════════════════════════
#  Main
# ═══════════════════════════════════════════════════════════════════════════════

def main() -> None:
    # ── --list-parameters works without any other argument ────────────────────
    if "--list-parameters" in sys.argv:
        print("Available climate parameters:\n")
        for k, (desc, model) in PARAMETERS.items():
            print(f"  {k:<46} {desc}  [best: {model}]")
        print("\nShorthand aliases:")
        for a, c in ALIASES.items():
            print(f"  {a:<28} → {c}")
        sys.exit(0)

    parser = build_parser()
    args = parser.parse_args()

    # ── Environment smoke-test ────────────────────────────────────────────────
    if not HAS_SHAPELY:
        sys.exit(
            "[ERROR] shapely is required but not installed.\n"
            "        pip install shapely"
        )
    if not HAS_PROJ:
        warnings.warn(
            "pyproj cannot locate its PROJ database in this environment.\n"
            "  WKT geometry and lat/lon reprojection work normally for EPSG:4326 (default).\n"
            "  Any other --epsg value will fail at runtime.  To fix:\n"
            "    conda install -c conda-forge pyproj\n"
            "  or: export PROJ_DATA=$(python -c \"import pyproj, os; "
            "print(os.path.dirname(pyproj.datadir.get_data_dir()))\")",
            stacklevel=2,
        )

    # ── Resolve & validate parameters ─────────────────────────────────────────
    parameters = resolve_parameters(args.parameters)
    api_key: Optional[str] = args.apikey if args.apikey and args.apikey.strip() else None

    # ── Log which API tier will be used ───────────────────────────────────────
    if api_key:
        log_info(
            f"Tier API     : COMMERCIAL (customer-api.open-meteo.com) "
            f"— clé …{api_key[-4:]}  [aucun quota journalier]"
        )
    else:
        log_info(
            "Tier API     : GRATUIT (archive-api.open-meteo.com) "
            "— 10 000 req/jour par IP.  "
            "Sur un serveur partagé (Galaxy), préférez --apikey."
        )
    log_info(f"Variables : {parameters}")

    # ── Output path ───────────────────────────────────────────────────────────
    fmt = args.output_format
    output_path = args.output or f"era5_data.{fmt}"

    # ── Parse global dates ────────────────────────────────────────────────────
    global_start: Optional[date] = None
    global_end: Optional[date] = None

    if args.start_date:
        global_start = parse_date(args.start_date, "--start-date")
    if args.end_date:
        global_end = parse_date(args.end_date, "--end-date")

    if global_start and global_end:
        validate_date_range(global_start, global_end, "global")

    # ── Read coordinate file ──────────────────────────────────────────────────
    log_info(f"Reading coordinate file: {args.coord_file}")
    coords = read_coord_file(args.coord_file)
    log_info(f"Loaded {len(coords)} site(s).")

    if args.verbose:
        log_info(f"Output         : {output_path} ({fmt})")
        log_info(f"Sleep per call : {args.sleep} s")

    # ── Quota-detection state ────────────────────────────────────────────────
    consecutive_429_failures: int = 0

    # ── ETA setup ─────────────────────────────────────────────────────────────
    # Note: the entire site loop is wrapped in try/except QuotaExhausted below.
    n_sites = len(coords)
    total_calls = _count_api_calls(n_sites, parameters)
    n_groups = total_calls // n_sites  # calls per site
    calls_done = 0
    run_start = time.perf_counter()
    recent_dur: collections.deque = collections.deque(maxlen=5)  # rolling window

    # Upfront estimate (floor = pure sleep time, no network latency)
    floor_s = total_calls * args.sleep
    log_info(
        f"Estimation : {n_sites} site(s) × {n_groups} appel(s)/site "
        f"= {total_calls} appel(s) API total | "
        f"Durée minimale : {_fmt_dur(floor_s)} "
        f"(sleep={args.sleep}s × {total_calls} appels, hors latence réseau)"
    )

    # ── Iterate over sites ────────────────────────────────────────────────────
    results: List[pd.DataFrame] = []
    n_skipped = 0
    next_site_id = 0   # numeric identifier, unique per extracted point (1, 2, 3, …)

    for idx, row in coords.iterrows():
        site_label = (
            str(row["site_name"]).strip()
            if "site_name" in row.index and pd.notna(row.get("site_name"))
            else f"row_{idx + 1}"
        )

        # Resolve per-row dates
        row_start: Optional[date] = None
        row_end: Optional[date] = None

        if "begin_date" in row.index and pd.notna(row["begin_date"]):
            raw = str(row["begin_date"]).strip()
            if raw:
                row_start = parse_date(raw, f"begin_date[{site_label}]")

        if "end_date" in row.index and pd.notna(row["end_date"]):
            raw = str(row["end_date"]).strip()
            if raw:
                row_end = parse_date(raw, f"end_date[{site_label}]")

        effective_start = row_start or global_start
        effective_end = row_end or global_end

        if effective_start is None:
            sys.exit(
                f"[ERROR] Pas de date de début pour le site '{site_label}'. "
                "Utilisez --start-date ou ajoutez une colonne 'begin_date'."
            )
        if effective_end is None:
            sys.exit(
                f"[ERROR] Pas de date de fin pour le site '{site_label}'. "
                "Utilisez --end-date ou ajoutez une colonne 'end_date'."
            )

        validate_date_range(effective_start, effective_end, site_label)

        # Resolve geometry → list of (lat, lon, point_label)
        point_list, geom_type = resolve_geometry_to_points(
            row, args.epsg, args.verbose
        )
        n_points = len(point_list)

        log_info(
            f"[{idx + 1}/{n_sites}] Site '{site_label}' | "
            f"géométrie : {geom_type} ({n_points} point(s)) | "
            f"{effective_start} → {effective_end}"
        )

        site_ok = False   # becomes True if at least one point succeeds

        for pt_idx, (lat, lon, pt_label) in enumerate(point_list):
            # Build a unique label for this point
            full_label = f"{site_label}__{pt_label}" if pt_label else site_label

            if n_points > 1 and args.verbose:
                log_info(
                    f"    [{pt_idx + 1}/{n_points}] "
                    f"lat={lat:.4f} lon={lon:.4f}  ({full_label})"
                )

            # API call
            df, elapsed, was_all_429 = fetch_openmeteo(
                lat=lat,
                lon=lon,
                start=effective_start,
                end=effective_end,
                parameters=parameters,
                sleep_s=args.sleep,
                api_key=api_key,
                verbose=args.verbose,
            )

            # Update ETA tracker
            calls_done += n_groups
            per_call = elapsed / n_groups if n_groups else elapsed
            recent_dur.append(per_call)
            _print_eta(calls_done, total_calls, run_start, recent_dur)

            if df.empty:
                if was_all_429 and not api_key:
                    consecutive_429_failures += 1
                    log_warn(
                        f"'{full_label}' : toutes les tentatives ont reçu HTTP 429 "
                        f"({consecutive_429_failures}/{QUOTA_PAUSE_THRESHOLD} "
                        "appel(s) consécutif(s) de ce type)."
                    )
                    if consecutive_429_failures >= QUOTA_PAUSE_THRESHOLD:
                        _wait_for_quota_reset(verbose=args.verbose)
                        consecutive_429_failures = 0
                else:
                    consecutive_429_failures = 0
                log_warn(f"Aucune donnée pour '{full_label}'. Point ignoré.")
                n_skipped += 1
                continue

            consecutive_429_failures = 0
            site_ok = True

            df.insert(0, "site_name", site_label)
            df.insert(1, "site_id", next_site_id)
            df.insert(2, "latitude", round(lat, 6))
            df.insert(3, "longitude", round(lon, 6))
            results.append(df)
            next_site_id += 1

        if not site_ok:
            log_warn(f"Aucun point extrait pour '{site_label}'.")

    # ── Combine & write ────────────────────────────────────────────────────
    if not results:
        sys.exit(
            "[ERROR] Aucune donnée récupérée.\n"
            "  • Lancez --verbose pour voir les URLs et le routing.\n"
            "  • Vérifiez la plage de dates (1960-01-01 → 2020-12-31).\n"
            "  • Vérifiez la connectivité vers archive-api.open-meteo.com."
        )

    combined = pd.concat(results, ignore_index=True)
    write_output(combined, output_path, fmt)

    total_wall = time.perf_counter() - run_start
    n_ok = combined["site_name"].nunique()
    log_info(
        f"Terminé en {_fmt_dur(total_wall)}. "
        f"{n_ok} site(s) extraits, {n_skipped} ignoré(s). "
        f"{len(combined):,} lignes journalières au total."
    )


# ─── Entry point ──────────────────────────────────────────────────────────────
if __name__ == "__main__":
    main()
# Extract ERA5 Climate Data — Open-Meteo API

## Overview

This tool extracts and formats **daily ERA5 and ERA5-Land climate and soil data** for long-term experiments (LTEs) using the [Open-Meteo Historical Weather API](https://open-meteo.com). It is composed of two files:

| File | Role |
|---|---|
| `get_ERA5_data_openmeteo.R` | Core R script performing data extraction and processing |
| `get_ERA5_data_openmeteo.xml` | Galaxy tool wrapper that exposes the R script as a Galaxy tool |

---

## Coordinate System

> ⚠️ **Coordinates must be provided in WGS84 (EPSG:4326) — decimal degrees.**

The tool expects site coordinates as **decimal longitude and latitude in the WGS84 geographic reference system (EPSG:4326)**. These are read directly from the `longitude` and `latitude` columns of the input metadata CSV and passed as-is to:

- `lutz::tz_lookup_coords(latitude, longitude)` — to automatically derive the timezone of each site.
- `openmeteo::weather_history(c(latitude, longitude), ...)` — to query the Open-Meteo API.

**No coordinate reprojection is performed by this tool.** If your coordinates are in a different system (e.g., Lambert 93 / EPSG:2154, RGF93, etc.), you must reproject them to WGS84 **before** using this tool. This can be done with:

- R: `sf::st_transform(your_data, crs = 4326)`
- Python: `pyproj.Transformer.from_crs("EPSG:2154", "EPSG:4326")`

---

## Input

### Site Metadata CSV

The main input is a CSV file with one row per site. Required columns:

| Column | Type | Description |
|---|---|---|
| `site_name` | character | Unique site identifier |
| `country` | character | Used to distinguish French vs. non-French sites |
| `longitude` | numeric | **Decimal degrees, WGS84** |
| `latitude` | numeric | **Decimal degrees, WGS84** |
| `start_date` | date (YYYY or YYYY-MM-DD) | Date of first soil C stock measurement |
| `last_soc_date` | date (YYYY or YYYY-MM-DD) | Date of last soil C stock measurement |

### Optional Parameters

| Parameter | Type | Default | Description |
|---|---|---|---|
| `--site_name` / `-s` | character | `"all"` | Name of a single site to process |
| `--start_date_` / `-d` | integer (YYYY) | derived from metadata | Override extraction start year |
| `--end_date_` / `-e` | integer (YYYY) | derived from metadata | Override extraction end year |

---

## Extracted Climate Variables

### Climate variables — non-French sites only

Extracted for: `ultuna`, `askov_ltbf`, `bad_lauchstadt_ltbf`, `pergamino`, `lanna`, `lonnstorp`, `kbs`, `nelitcse`, `waite`.

| Variable | Open-Meteo parameter | Unit |
|---|---|---|
| Daily precipitation | `precipitation_sum` | mm/day |
| Daily mean temperature at 2 m | `temperature_2m_mean` | °C |
| Reference ET0 (FAO-56 Penman-Monteith) | `et0_fao_evapotranspiration` | mm/day |

### Soil climate variables — all sites

| Variable | Open-Meteo parameter | Unit |
|---|---|---|
| Volumetric soil moisture, 0–7 cm | `soil_moisture_0_to_7cm_mean` | m³/m³ |
| Volumetric soil moisture, 7–28 cm | `soil_moisture_7_to_28cm_mean` | m³/m³ |
| Soil temperature, 0–7 cm | `soil_temperature_0_to_7cm_mean` | °C |
| Soil temperature, 7–28 cm | `soil_temperature_7_to_28cm_mean` | °C |

ERA5 data are available from **1940-01-01** onwards.

---

## Outputs

Each site × variable combination is first saved as an individual `.rds` file (resumable runs — existing files are automatically skipped).

| File | Format | Description |
|---|---|---|
| `outputs/ERA5/single_outputs_fw/*.rds` | RDS | One file per site × variable — forward simulation period |
| `outputs/ERA5/single_outputs_su/*.rds` | RDS | One file per site × variable — spin-up period |
| `outputs/ERA5/era5_nonfr_sites_fw.rds` | RDS | Merged dataset — forward simulation period (all variables, all sites) |
| `outputs/ERA5/era5_nonfr_sites_su.rds` | RDS | Merged dataset — spin-up period (all variables, all sites) |

### Output columns (merged RDS files)

| Column | Description |
|---|---|
| `site_name` | Site identifier |
| `date` | Date (daily) |
| `precip` | Total precipitation (mm) — `NA` for French sites |
| `temp` | Daily mean temperature at 2 m (°C) — `NA` for French sites |
| `et0` | Reference ET0 (mm) — `NA` for French sites |
| `soil_moisture_0_7` | Volumetric soil moisture, 0–7 cm (m³/m³) |
| `soil_moisture_7_28` | Volumetric soil moisture, 7–28 cm (m³/m³) |
| `soil_temperature_0_7` | Soil temperature, 0–7 cm (°C) |
| `soil_temperature_7_28` | Soil temperature, 7–28 cm (°C) |

Isolated missing values (`NA`) in `precip`, `temp`, and `et0` are filled by linear interpolation between the preceding and following days.

---

## Temporal Logic

Two time periods are produced per site:

- **Forward simulation period**: from `start_date` to `last_soc_date` (inclusive), with a floor at 1940-01-01.
- **Spin-up period** (model initialization): ideally the 30 years preceding `start_date`, with a floor at 1940-01-01. If fewer than 15 years of pre-experiment data are available, the first 30 years of the experiment are used as spin-up instead.

---

## Requirements

An **active internet connection** is required to query the Open-Meteo API.

To comply with Open-Meteo rate limits (600 calls/min, 5 000/h, 10 000/day), a **40-second pause** is applied between each API call. Extracting data for all sites and all variables can therefore take **several hours**. Re-running the tool on a partially completed dataset is safe: already-downloaded `.rds` files are detected and skipped automatically.

### R packages

| Package | Version |
|---|---|
| `r-base` | ≥ 4.5.1 |
| `r-sf` | ≥ 1.0.21 |
| `r-tidyverse` | ≥ 2.0.0 |
| `r-lutz` | ≥ 0.3.2 |
| `r-optparse` | ≥ 1.7.5 |
| `openmeteo` | 0.2.4 (installed locally from `.tar.gz`) |
| `tibblify` | 0.3.1 (installed locally from `.tar.gz`) |
| `testthat` | 3.3.2 (installed locally from `.tar.gz`) |

The three packages above are bundled in `R_packages_folder/` and installed locally by the Galaxy wrapper at runtime.

---

## Usage

### Command line

```bash
Rscript get_ERA5_data_openmeteo_MG.R \
  -m path/to/site_metadata.csv \
  -s all \
  -d 1990 \
  -e 2020
```

### Galaxy

Open the tool **"Extract ERA5 climate data"** in your Galaxy instance, upload your metadata CSV, and fill in the optional parameters as needed.

---

## References

- Open-Meteo Historical Weather API: https://open-meteo.com
- Fujisaki et al. (2026). *Data from long-term experiments in temperate croplands to evaluate soil organic carbon models.* https://www.nature.com/articles/s41597-026-06863-7

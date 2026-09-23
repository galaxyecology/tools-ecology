# Build HETEROFOR Forcing Files

Builds hourly meteorological forcing files for the **HETEROFOR** forest growth model at the 26 plots of the **RENECOFOR** network, from **SAFRAN** reanalysis data (via the ORCHIDEE land surface model output format) already regridded onto the RENECOFOR plot locations.

For each station, the tool writes an hourly forcing file with radiation, air temperature, soil surface temperature, rainfall, relative humidity (derived from SAFRAN specific humidity, temperature and surface pressure), wind speed and direction, and a diffuse-to-global radiation ratio.

## Modes

- **SAFRAN-only** (default): forcing files built entirely from the regridded SAFRAN/ORCHIDEE data, for every station with complete records.
- **RENECOFOR + SAFRAN**: additionally accepts observed RENECOFOR meteorological data (RDS file). Observations are used first and gaps filled from SAFRAN (radiation always comes from SAFRAN). Produces two extra reports: a SAFRAN-filling summary and a missing-value summary per station.

## Input

- **SAFRAN/ORCHIDEE forcing file** and **PSurf file** — NetCDF, CSV, Parquet, or CoverageJSON, auto-detected independently per file. NetCDF must already be regridded onto the plot locations (e.g. with `cdo remapbil` / `remapnn`); stations are matched by position for NetCDF, by name for the other formats.
- **RENECOFOR station metadata** — tab-delimited file or RDS data.frame with a `Station` column.
- Optional: observed RENECOFOR data (RDS) for the combined mode.

## Output

Galaxy **list collections** (one element per station, since the number of stations passing the completeness check varies): SAFRAN-only forcing files, and — in combined mode — RENECOFOR+SAFRAN forcing files plus the two summary reports.

## Requirements

R (`r-base`, `r-optparse`, `r-dplyr`, `r-tidyr`, `r-purrr`, `r-stringr`, `r-lubridate`, `r-ncdf4`, `r-nanoparquet`, `r-jsonlite`)

## Notes

Wraps a cleaned-up, parameterised version of an R script originally written by Annemiek I Stegehuis to prepare SAFRAN/RENECOFOR forcing data for HETEROFOR runs at the ALAMOD project plots. The underlying meteorological calculations (unit conversions, humidity conversion, gap-filling logic) are unchanged from the original script.

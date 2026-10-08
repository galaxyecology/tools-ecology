# Generate Spin-up Climate Metadata

This tool computes **spin-up period metadata** for long-term soil organic carbon (SOC) model simulations.

Given a table of experimental sites with a first and last measurement date, it works out — for each site — the optimal date range to use for the model's spin-up (initialization) phase, i.e. the period of climate data used to bring the carbon pools to a steady state before the forward simulation begins.

The output metadata table is designed to be passed directly to a downstream climate data extraction tool (e.g. [GEOSAS/SAFRAN](https://geosas.fr/web/?page_id=6345) or [Open-Meteo/ERA5](https://open-meteo.com/en/docs/era5-api)) to drive the actual data retrieval for each site.

## Input

A CSV file with one row per site, containing at least two date columns (year `YYYY` or full date `YYYYMMDD`):

- First SOC measurement date (column name configurable, default `start_date`)
- Last SOC measurement date (column name configurable, default `end_date`)

Any other columns (e.g. `site_name`, `latitude`, `longitude`) are preserved but not required.

## Parameters

| Parameter | Description |
|---|---|
| Site metadata file | Input CSV described above |
| First / Last measurement date column | Names of the two date columns to read |
| Target climate data source | `geosas` (earliest date 1959-01-01) or `openmeteo` (earliest date 1940-01-01) — sets the clipping bound for the spin-up start |
| Minimum spin-up duration (years) | Below this, the spin-up window is shifted to overlap the first years of the experiment instead |
| Target spin-up duration (years) | Ideal spin-up length; may be shortened by the reanalysis start-date bound |

## Spin-up logic

For each site:

1. Target start = first measurement date − target spin-up duration.
2. Clip to the earliest date available for the chosen reanalysis (1959-01-01 for SAFRAN, 1940-01-01 for ERA5).
3. Effective duration = first measurement date − clipped start.
4. If effective duration ≥ minimum duration → spin-up covers `[clipped start → first measurement date]`.
5. Otherwise → spin-up covers `[clipped start → clipped start + target duration]` (first N years of the experiment used instead).

## Output

A CSV with one row per site: `site_name`, `begin_date`, `end_date`, `start_spinup`, `end_spinup`, `spinup_duration_yr`, `method_spinup`, `api_source`.

## Requirements

- R (`r-base`, `r-optparse`, `r-tidyverse`, `r-lubridate`)

## References

- Durand, Y. et al. (1993). A meteorological estimation of relevant parameters for snow models. *Annals of Glaciology*, 18, 65–71. https://doi.org/10.3189/S0260305500011277
- Hersbach, H. et al. (2020). The ERA5 global reanalysis. *Quarterly Journal of the Royal Meteorological Society*, 146(730), 1999–2049. https://doi.org/10.1002/qj.3803
- Zippenfenig, P. (2023). Open-Meteo.com Weather API. Zenodo. https://doi.org/10.5281/zenodo.7970649
- GEOSAS API documentation: https://geosas.fr/web/?page_id=6345

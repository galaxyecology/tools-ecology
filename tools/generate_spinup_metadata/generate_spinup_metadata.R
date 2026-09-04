#!/usr/bin/env Rscript
# =============================================================================
# generate_spinup_metadata.R
# Galaxy tool — Generate Spin-up Climate Metadata
# -----------------------------------------------------------------------------
# Reads a site metadata table and computes, for each site, the optimal
# spin-up period (start date, end date, duration) to be used for SOC model
# initialization. The result is exported as a CSV metadata table that can
# subsequently be passed to a climate data extraction tool (GEOSAS/SAFRAN
# or Open-Meteo/ERA5).
# =============================================================================

library(optparse)
library(tidyverse)
library(lubridate)
# =============================================================================
# 1. Parse command-line arguments
# =============================================================================
option_list <- list(
  make_option("--input_metadata",      type = "character",
              help = "Path to site metadata CSV [required]"),
  make_option("--api_source",          type = "character", default = "geosas",
              help = "Target API: 'geosas' or 'openmeteo' [default: geosas]"),
  make_option("--spinup_min_years",    type = "integer",   default = 15L,
              help = "Minimum spin-up duration in years [default: 15]"),
  make_option("--spinup_target_years", type = "integer",   default = 30L,
              help = "Target spin-up duration in years [default: 30]"),
  make_option("--begin_date_col",      type = "character", default = "begin_date",
              help = "Name of the column holding the first SOC measurement date [default: begin_date]"),
  make_option("--end_date_col",        type = "character", default = "end_date",
              help = "Name of the column holding the last SOC measurement date [default: end_date]"),
  make_option("--earliest_date",       type = "character", default = "1959-01-01",
              help = "Earliest date available in the reanalysis [default: 1959-01-01]"),
  make_option("--output_metadata",     type = "character",
              help = "Output path for the spin-up metadata CSV [required]")
)

opt <- parse_args(OptionParser(option_list = option_list))

if (is.null(opt$input_metadata))  stop("--input_metadata is required.")
if (is.null(opt$output_metadata)) stop("--output_metadata is required.")

api_source          <- tolower(trimws(opt$api_source))
spinup_min_years    <- as.integer(opt$spinup_min_years)
spinup_target_years <- as.integer(opt$spinup_target_years)
earliest_date       <- ymd(opt$earliest_date)
begin_date_col      <- opt$begin_date_col
end_date_col        <- opt$end_date_col

message("=== Generate Spin-up Climate Metadata ===")
message("API source           : ", api_source)
message("Earliest date        : ", earliest_date)
message("Target spinup (yr)   : ", spinup_target_years)
message("Minimum spinup (yr)  : ", spinup_min_years)
message("Begin date column    : ", begin_date_col)
message("End date column      : ", end_date_col)

# =============================================================================
# 2. Read and validate site metadata
# =============================================================================
message("\n[1/3] Reading site metadata ...")

metadata_raw <- read_csv(opt$input_metadata, show_col_types = FALSE)

required_cols <- c("site_name", begin_date_col, end_date_col)
missing_cols  <- setdiff(required_cols, colnames(metadata_raw))
if (length(missing_cols) > 0) {
  stop("Missing required column(s) in input metadata: ",
       paste(missing_cols, collapse = ", "))
}

# Clean dates (longitude/latitude, if present, are simply not used by this
# tool and are dropped further down — they are optional, as documented).
# truncated = 2 allows bare years (e.g. 1985 → 1985-01-01)
locations <- metadata_raw %>%
  rename(begin_date = all_of(begin_date_col),
         end_date   = all_of(end_date_col)) %>%
  mutate(
    begin_date    = ymd(begin_date,    truncated = 2L),
    end_date = ymd(end_date, truncated = 2L),
    end_date      = ceiling_date(end_date, unit = "year") - days(1)
  )

message("  Sites loaded: ", nrow(locations))

# =============================================================================
# 3. Compute spin-up periods
# =============================================================================
message("\n[2/3] Computing spin-up periods ...")

su_info <- locations %>%
  mutate(
    # Ideal start: N target years before first SOC measurement
    start_spinup_theo = begin_date - years(spinup_target_years),
    # Clip to earliest date available in the reanalysis
    start_spinup = case_when(
      start_spinup_theo < earliest_date ~ earliest_date,
      TRUE                              ~ start_spinup_theo
    ),
    # Effective pre-experiment duration
    spinup_duration_yr = as.integer(
      round(interval(start_spinup, begin_date) / years(1))
    )
  ) %>%
  mutate(
    # If enough pre-experiment data are available, spin-up runs up to begin_date.
    # Otherwise, spin-up is shifted into the first N years of the experiment.
    end_spinup = case_when(
      spinup_duration_yr >= spinup_min_years ~ begin_date,
      TRUE ~ start_spinup %m+% years(spinup_target_years)
    ),
    # Plain-language description of the strategy
    method_spinup = case_when(
      spinup_duration_yr >= spinup_min_years ~
        paste0(
          "Climate normals computed from ", year(start_spinup),
          " to ", year(begin_date),
          " (", spinup_duration_yr, " yr before the experiment)"
        ),
      TRUE ~
        paste0(
          "Insufficient pre-experiment data (", spinup_duration_yr,
          " yr available before the reanalysis starts). ",
          "Climate normals computed from ", year(start_spinup),
          " to ", year(start_spinup %m+% years(spinup_target_years)),
          " (first ", spinup_target_years, " yr of the experiment used instead)"
        )
    )
  )

# Build the final metadata table
spinup_metadata <- su_info %>%
  dplyr::select(
    site_name,
    begin_date,
    end_date,
    start_spinup,
    end_spinup,
    spinup_duration_yr,
    method_spinup
  ) %>%
  mutate(api_source = api_source)

message("  Spin-up periods computed for ", nrow(spinup_metadata), " site(s).")
spinup_metadata %>%
  dplyr::select(site_name, start_spinup, end_spinup, spinup_duration_yr) %>%
  print(n = Inf)

# =============================================================================
# 4. Write output
# =============================================================================
message("\n[3/3] Writing spin-up metadata ...")
write_csv(spinup_metadata, opt$output_metadata)
message("  Saved: ", opt$output_metadata)
message("\nDone.")

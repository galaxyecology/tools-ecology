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
  make_option("--begin_date_col",      type = "integer",   default = 1L,
              help = "1-based index of the column holding the first SOC measurement date [default: 1]"),
  make_option("--end_date_col",        type = "integer",   default = 2L,
              help = "1-based index of the column holding the last SOC measurement date [default: 2]"),
  make_option("--longitude_col",       type = "integer",   default = 3L,
              help = "1-based index of the column holding the site longitude (decimal degrees) [default: 3]"),
  make_option("--latitude_col",        type = "integer",   default = 4L,
              help = "1-based index of the column holding the site latitude (decimal degrees) [default: 4]"),
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
begin_date_col      <- as.integer(opt$begin_date_col)
end_date_col        <- as.integer(opt$end_date_col)
longitude_col       <- as.integer(opt$longitude_col)
latitude_col        <- as.integer(opt$latitude_col)

message("=== Generate Spin-up Climate Metadata ===")
message("API source           : ", api_source)
message("Earliest date        : ", earliest_date)
message("Target spinup (yr)   : ", spinup_target_years)
message("Minimum spinup (yr)  : ", spinup_min_years)
message("Begin date column    : column #", begin_date_col)
message("End date column      : column #", end_date_col)
message("Longitude column     : column #", longitude_col)
message("Latitude column      : column #", latitude_col)

# =============================================================================
# 2. Read and validate site metadata
# =============================================================================
message("\n[1/3] Reading site metadata ...")

# All columns are read as text so that the coordinates are written to the
# output exactly as they appear in the input file.
metadata_raw <- read_csv(opt$input_metadata,
                         col_types = cols(.default = col_character()))
n_cols <- ncol(metadata_raw)

if (!"site_name" %in% colnames(metadata_raw)) {
  stop(
    "Missing required column 'site_name' in input metadata.\n",
    "Columns found in the file: ", paste(colnames(metadata_raw), collapse = ", ")
  )
}

selected_cols <- c(
  "first measurement date" = begin_date_col,
  "last measurement date"  = end_date_col,
  "longitude"              = longitude_col,
  "latitude"               = latitude_col
)

if (any(is.na(selected_cols) | selected_cols < 1 | selected_cols > n_cols)) {
  stop(
    "Column numbers must be between 1 and ", n_cols,
    " (the input file has ", n_cols, " column(s))."
  )
}

if (anyDuplicated(selected_cols) > 0) {
  stop(
    "The same column was selected more than once: ",
    paste0(names(selected_cols), " = column ", selected_cols,
           " (", colnames(metadata_raw)[selected_cols], ")", collapse = "; "),
    ". Please select four different columns."
  )
}

# Check that coordinates are numeric decimal degrees within valid ranges
check_coordinate <- function(values, col_index, label, limit) {
  col_name <- colnames(metadata_raw)[col_index]
  numeric_values <- suppressWarnings(as.numeric(values))
  not_numeric <- is.na(numeric_values) & !is.na(values) & trimws(values) != ""
  if (any(not_numeric)) {
    stop(
      call. = FALSE,
      "The ", label, " column (column ", col_index, ", '", col_name,
      "') contains non-numeric values for site(s): ",
      paste(metadata_raw$site_name[not_numeric], collapse = ", "),
      ". Coordinates must be decimal degrees (e.g. 1.506059)."
    )
  }
  missing_values <- is.na(numeric_values)
  if (any(missing_values)) {
    stop(
      call. = FALSE,
      "The ", label, " column (column ", col_index, ", '", col_name,
      "') is empty for site(s): ",
      paste(metadata_raw$site_name[missing_values], collapse = ", "), "."
    )
  }
  out_of_range <- abs(numeric_values) > limit
  if (any(out_of_range)) {
    stop(
      call. = FALSE,
      "The ", label, " column (column ", col_index, ", '", col_name,
      "') has values outside [-", limit, ", ", limit, "] for site(s): ",
      paste(metadata_raw$site_name[out_of_range], collapse = ", "),
      ". Check that the longitude and latitude columns are not swapped."
    )
  }
}

check_coordinate(metadata_raw[[longitude_col]], longitude_col, "longitude", 180)
check_coordinate(metadata_raw[[latitude_col]],  latitude_col,  "latitude",   90)

# Galaxy's column-selector parameters pass a 1-based column index, not a
# column name: the selected columns are extracted by position.
# truncated = 2 allows bare years (e.g. 1985 -> 1985-01-01)
locations <- tibble(
  site_name  = metadata_raw$site_name,
  longitude  = trimws(metadata_raw[[longitude_col]]),
  latitude   = trimws(metadata_raw[[latitude_col]]),
  begin_date = ymd(metadata_raw[[begin_date_col]], truncated = 2L),
  end_date   = ymd(metadata_raw[[end_date_col]],   truncated = 2L)
) %>%
  mutate(end_date = ceiling_date(end_date, unit = "year") - days(1))

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
    longitude,
    latitude,
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
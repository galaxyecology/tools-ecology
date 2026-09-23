#!/usr/bin/env Rscript
#
# build_heterofor_forcing.R
#
# Build hourly meteorological forcing files for the HETEROFOR forest growth
# model at the 26 RENECOFOR plots, starting from SAFRAN reanalysis data that
# has already been regridded onto the RENECOFOR plot locations. Optionally,
# the SAFRAN data can be completed with observed RENECOFOR station data,
# using RENECOFOR observations first and filling remaining gaps with SAFRAN.
#
# Original author: Annemiek I Stegehuis
# Galaxy wrapper / refactor of 260410_ALAMOD_forcing_files_heterofor.R
#
# One tab-delimited *_HETEROFOR_meteo.txt file is written per RENECOFOR
# station. Stations for which the output time series is not fully complete
# (any missing value in any column) are skipped, and reported on stderr.

suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(ncdf4)
})

heterofor_column_order <- c(
  "Station", "Year", "Month", "Day", "Hour",
  "Radiation", "Air_temperature", "Soil_surface_temperature",
  "Rainfall", "Relative_humidity", "Wind_speed", "Wind_direction",
  "Diffuse_to_global_ratio"
)

## ---------------------------------------------------------------------
## 1. Command line interface
## ---------------------------------------------------------------------

build_option_list <- function() {
  list(
    make_option(
      "--safran_data",
      type = "character",
      help = paste(
        "SAFRAN/ORCHIDEE file with variables Tair, Qair, Wind, Rainf,",
        "Snowf, SWdown, LWdown, regridded onto the RENECOFOR stations.",
        "NetCDF, CSV or Parquet (auto-detected) [required]"
      )
    ),
    make_option(
      "--psurf_data",
      type = "character",
      help = paste(
        "SAFRAN/ORCHIDEE PSurf file, regridded onto the RENECOFOR",
        "stations. NetCDF, CSV or Parquet (auto-detected) [required]"
      )
    ),
    make_option(
      "--stations",
      type = "character",
      help = paste(
        "Tab-delimited or RDS station metadata file with a Station",
        "column. If the SAFRAN/PSurf files are NetCDF, station order",
        "must match the order used when they were regridded; with",
        "CSV/Parquet, stations are matched by name instead [required]"
      )
    ),
    make_option(
      "--reneco_rds",
      type = "character",
      default = NULL,
      help = paste(
        "Optional RDS file with observed RENECOFOR meteorological data",
        "(same column layout as the SAFRAN-only output). When given,",
        "a second set of forcing files is produced using observed",
        "RENECOFOR data first, filled with SAFRAN where missing",
        "[default: not used, SAFRAN-only output only]"
      )
    ),
    make_option(
      "--safran_end_date",
      type = "character",
      default = "2024-01-01 00:00:00",
      help = paste(
        "SAFRAN records at or after this date (UTC, 'YYYY-MM-DD HH:MM:SS')",
        "are dropped before combining with RENECOFOR data, to protect",
        "against known artefacts near the end of the SAFRAN record",
        "[default: %default]"
      )
    ),
    make_option(
      "--wind_height_safran",
      type = "double",
      default = 10.0,
      help = paste(
        "Wind speed measurement height (m) written in the header of the",
        "SAFRAN-only forcing files [default: %default]"
      )
    ),
    make_option(
      "--wind_height_combined",
      type = "double",
      default = 2.0,
      help = paste(
        "Wind speed measurement height (m) written in the header of the",
        "RENECOFOR + SAFRAN forcing files [default: %default]"
      )
    ),
    make_option(
      "--wind_atten_coef",
      type = "double",
      default = 2,
      help = paste(
        "Wind speed attenuation coefficient written in the forcing file",
        "header [default: %default]"
      )
    ),
    make_option(
      "--out_safran_dir",
      type = "character",
      default = "heterofor_safran_only",
      help = paste(
        "Output directory for SAFRAN-only forcing files",
        "[default: %default]"
      )
    ),
    make_option(
      "--out_combined_dir",
      type = "character",
      default = "heterofor_combined",
      help = paste(
        "Output directory for RENECOFOR + SAFRAN forcing files, only",
        "used when --reneco_rds is given [default: %default]"
      )
    ),
    make_option(
      "--filling_summary",
      type = "character",
      default = NULL,
      help = paste(
        "Output tabular file summarising, per station, how much of the",
        "combined record was filled from SAFRAN. Only written when",
        "--reneco_rds is given"
      )
    ),
    make_option(
      "--missing_summary",
      type = "character",
      default = NULL,
      help = paste(
        "Output tabular file summarising, per station, remaining missing",
        "values after combining RENECOFOR and SAFRAN. Only written when",
        "--reneco_rds is given"
      )
    )
  )
}

parse_cli_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  parser <- OptionParser(option_list = build_option_list())
  opt <- parse_args(parser, args = args)

  required <- c("safran_data", "psurf_data", "stations")
  missing_req <- required[vapply(opt[required], is.null, logical(1))]
  if (length(missing_req) > 0) {
    print_help(parser)
    stop(
      "Missing required argument(s): ", paste(missing_req, collapse = ", "),
      call. = FALSE
    )
  }
  opt
}

## ---------------------------------------------------------------------
## 2. Reading SAFRAN data (NetCDF, CSV or Parquet) and station metadata
## ---------------------------------------------------------------------

safran_main_vars <- c(
  "Tair", "Qair", "Wind", "Rainf", "Snowf", "SWdown", "LWdown"
)
safran_psurf_vars <- "PSurf"

# Identify a SAFRAN input file as netcdf, parquet or csv from its first
# bytes, rather than from its file extension: Galaxy does not always pass
# datasets to tools with a meaningful extension.
detect_file_format <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  header <- readBin(con, "raw", n = 8)

  has_prefix <- function(magic) {
    length(header) >= length(magic) &&
      all(header[seq_along(magic)] == magic)
  }

  netcdf4_magic <- as.raw(c(0x89, 0x48, 0x44, 0x46, 0x0d, 0x0a, 0x1a, 0x0a))
  if (has_prefix(charToRaw("PAR1"))) {
    "parquet"
  } else if (has_prefix(charToRaw("CDF")) || has_prefix(netcdf4_magic)) {
    "netcdf"
  } else {
    "text"
  }
}

# Convert a NetCDF "<unit> since <origin>" time axis to POSIXct, rounded to
# the nearest full hour.
read_time_axis <- function(nc, time_var = "tstep") {
  time_vals <- ncvar_get(nc, time_var)
  time_units <- ncatt_get(nc, time_var, "units")$value

  parts <- strsplit(time_units, " since ")[[1]]
  unit <- parts[1]
  origin <- as.POSIXct(parts[2], tz = "UTC")

  dates <- switch(
    unit,
    "seconds" = origin + seconds(time_vals),
    "minutes" = origin + minutes(time_vals),
    "hours"   = origin + hours(time_vals),
    "days"    = origin + days(time_vals),
    stop("Unsupported time unit in NetCDF file: ", unit)
  )
  round_date(dates, unit = "hour")
}

# Reshape a [time, station] matrix into a long data frame with one row per
# station-hour.
matrix_to_long <- function(mat, varname, dates, station_names) {
  df <- as.data.frame(t(mat))
  df$date <- dates
  colnames(df)[seq_along(station_names)] <- station_names
  pivot_longer(
    df,
    cols = all_of(station_names),
    names_to = "Station",
    values_to = varname
  )
}

# Read one or more variables from a SAFRAN NetCDF file into a long table
# with Station, date and one column per variable. Stations are matched to
# NetCDF columns by position: station_names must be given in the same
# order as the file's "ncells" dimension.
read_netcdf_long <- function(path, varnames, station_names) {
  nc <- nc_open(path)
  on.exit(nc_close(nc), add = TRUE)
  dates <- read_time_axis(nc)

  long_tables <- lapply(varnames, function(varname) {
    matrix_to_long(ncvar_get(nc, varname), varname, dates, station_names)
  })
  reduce(long_tables, full_join, by = c("Station", "date"))
}

# Read one or more variables from a CSV or Parquet SAFRAN file into a long
# table with Station, date and one column per variable. The file must have
# a "Station" column and "Year"/"Month"/"Day"/"Hour" columns; stations are
# matched to the metadata file by name rather than by position.
read_tabular_long <- function(path, format, varnames) {
  df <- switch(
    format,
    csv = read.csv(path, stringsAsFactors = FALSE),
    parquet = as.data.frame(nanoparquet::read_parquet(path)),
    stop("Unsupported tabular SAFRAN format: ", format)
  )

  required_cols <- c("Station", "Year", "Month", "Day", "Hour", varnames)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "SAFRAN file is missing required column(s): ",
      paste(missing_cols, collapse = ", "), " (file: ", path, ")"
    )
  }

  df %>%
    mutate(
      date = make_datetime(.data$Year, .data$Month, .data$Day, .data$Hour)
    ) %>%
    select("Station", "date", all_of(varnames))
}

# Parse a CoverageJSON "t" axis into POSIXct. Standard ISO 8601
# ("%Y-%m-%dT%H:%M:%SZ") is tried first; GeoSAS-style CoverageJSON encodes
# time with hyphens instead of colons ("%Y-%m-%dT%H-%M-%SZ"), tried next.
parse_coveragejson_dates <- function(values) {
  dates <- as.POSIXct(values, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  still_na <- is.na(dates)
  if (any(still_na)) {
    dates[still_na] <- as.POSIXct(
      values[still_na], format = "%Y-%m-%dT%H-%M-%SZ", tz = "UTC"
    )
  }
  if (any(is.na(dates))) {
    stop("Could not parse one or more CoverageJSON time values")
  }
  dates
}

# Read one CoverageJSON Coverage object (one station) into a long
# data.frame with Station, date and one column per requested variable. The
# Coverage must have an "id" field giving the station name, a
# domain.axes.t.values time axis, and one ranges.<varname>.values array per
# requested variable, parallel to the time axis.
read_one_coverage <- function(coverage, varnames, path) {
  station_name <- coverage$id
  if (is.null(station_name)) {
    stop(
      "Each Coverage in a CoverageJSON SAFRAN file must have an 'id' ",
      "field giving the station name: ", path
    )
  }

  dates <- parse_coveragejson_dates(
    unlist(coverage$domain$axes$t$values, use.names = FALSE)
  )

  missing_vars <- setdiff(varnames, names(coverage$ranges))
  if (length(missing_vars) > 0) {
    stop(
      "CoverageJSON Coverage '", station_name, "' is missing required ",
      "parameter(s): ", paste(missing_vars, collapse = ", "),
      " (file: ", path, ")"
    )
  }

  df <- data.frame(Station = station_name, date = dates)
  for (varname in varnames) {
    df[[varname]] <- unlist(
      coverage$ranges[[varname]]$values, use.names = FALSE
    )
  }
  df
}

# Read a CoverageJSON SAFRAN file into a long table with Station, date and
# one column per requested variable. Accepts either a single Coverage (one
# station) or a CoverageCollection (one Coverage per station).
read_coveragejson_long <- function(json_doc, varnames, path) {
  coverages <- if (identical(json_doc$type, "CoverageCollection")) {
    json_doc$coverages
  } else {
    list(json_doc)
  }
  long_tables <- lapply(
    coverages, read_one_coverage, varnames = varnames, path = path
  )
  bind_rows(long_tables)
}

# Read a SAFRAN input file (NetCDF, Parquet, CoverageJSON or CSV,
# auto-detected) into a long table with Station, date and one column per
# requested variable.
read_safran_source <- function(path, varnames, station_names) {
  format <- detect_file_format(path)
  if (format == "netcdf") {
    return(read_netcdf_long(path, varnames, station_names))
  }
  if (format == "parquet") {
    return(read_tabular_long(path, "parquet", varnames))
  }

  json_doc <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.null(json_doc)) {
    read_coveragejson_long(json_doc, varnames, path)
  } else {
    read_tabular_long(path, "csv", varnames)
  }
}

# Station metadata: a "Station" column is required; row order must match the
# station order in the NetCDF ncells dimension. Accepts either a
# tab-delimited text file or an RDS file containing a data.frame; the file
# is first tried as RDS, and treated as tab-delimited text if that fails.
read_station_metadata <- function(path) {
  rds_obj <- tryCatch(readRDS(path), error = function(e) NULL)

  if (!is.null(rds_obj)) {
    if (!is.data.frame(rds_obj)) {
      stop("Station metadata RDS file must contain a data.frame: ", path)
    }
    stationdat <- rds_obj
  } else {
    stationdat <- read.table(
      path, sep = "\t", header = TRUE, dec = ".", stringsAsFactors = FALSE
    )
  }

  if (!"Station" %in% names(stationdat)) {
    stop("Station metadata file must contain a 'Station' column: ", path)
  }
  stationdat
}

## ---------------------------------------------------------------------
## 3. Relative humidity calculation
## ---------------------------------------------------------------------

# Saturation vapour pressure following the ECMWF water/ice formula.
# temp_k: air temperature in Kelvin. Returns saturation pressure in Pa.
esat_ecmwf <- function(temp_k) {
  t0 <- 273.16
  es_water <- 611.21 * exp(17.502 * (temp_k - t0) / (temp_k - 32.19))
  es_ice <- 611.21 * exp(22.587 * (temp_k - t0) / (temp_k + 0.7))

  # Linear interpolation between the ice formula (<= -23 degC) and the
  # water formula (>= 0 degC).
  weight <- pmin(1, pmax(0, (temp_k - 250.16) / (273.16 - 250.16)))
  weight * es_water + (1 - weight) * es_ice
}

# Convert specific humidity to relative humidity.
# q: specific humidity (kg/kg); tair_c: air temperature (degC);
# psurf: surface pressure (Pa). Returns relative humidity in percent,
# clipped to the physical range [0, 100].
qair_to_rh <- function(q, tair_c, psurf) {
  eps <- 0.621981
  temp_k <- tair_c + 273.15

  vapour_pressure <- q * psurf / (eps + (1 - eps) * q)
  sat_vapour_pressure <- esat_ecmwf(temp_k)

  rh <- 100 * vapour_pressure / sat_vapour_pressure
  pmin(100, pmax(0, rh))
}

## ---------------------------------------------------------------------
## 4. Building the HETEROFOR forcing table from SAFRAN
## ---------------------------------------------------------------------

# From the joined long SAFRAN table (Station, date, Tair, Qair, Wind,
# Rainf, Snowf, SWdown, LWdown, PSurf), derive air temperature in degC,
# total precipitation and relative humidity.
assemble_safran_table <- function(safran_long) {
  safran_long %>%
    mutate(
      Tair = .data$Tair - 273.15,
      precip = .data$Rainf + .data$Snowf,
      RH = qair_to_rh(
        q = .data$Qair, tair_c = .data$Tair, psurf = .data$PSurf
      )
    )
}

# Map the assembled SAFRAN table onto the HETEROFOR column layout.
to_heterofor_table <- function(meteo) {
  meteo %>%
    mutate(
      Year = year(.data$date),
      Month = month(.data$date),
      Day = day(.data$date),
      Hour = hour(.data$date), # hour of day, time zone follows nc input
      Radiation = .data$SWdown,
      Air_temperature = .data$Tair,
      Soil_surface_temperature = .data$Tair,
      Rainfall = .data$precip * 3600, # kilogram per m2 per s to mm per hour
      Relative_humidity = .data$RH,
      Wind_speed = .data$Wind,
      Wind_direction = 0,
      Diffuse_to_global_ratio = 0.5
    ) %>%
    select(all_of(heterofor_column_order))
}

# Log a short relative-humidity sanity check to stderr (range and number of
# missing values), instead of relying on implicit console auto-printing.
log_rh_diagnostics <- function(meteo_heterofor) {
  rh <- meteo_heterofor$Relative_humidity
  rh_range <- suppressWarnings(range(rh, na.rm = TRUE))
  message(sprintf(
    "Relative humidity range: [%.1f, %.1f] percent; missing values: %d",
    rh_range[1], rh_range[2], sum(is.na(rh))
  ))
}

## ---------------------------------------------------------------------
## 5. Filtering and writing per-station forcing files
## ---------------------------------------------------------------------

# Drop stations for which any value, in any column, is missing, and report
# which stations were dropped.
drop_incomplete_stations <- function(df, label) {
  station_missing <- df %>%
    group_by(.data$Station) %>%
    summarise(n_missing = sum(is.na(across(everything()))), .groups = "drop")

  stations_ok <- station_missing %>%
    filter(.data$n_missing == 0) %>%
    pull("Station")

  stations_dropped <- setdiff(unique(df$Station), stations_ok)
  if (length(stations_dropped) > 0) {
    message(sprintf(
      "%s: dropping %d station(s) with missing values: %s",
      label, length(stations_dropped), paste(stations_dropped, collapse = ", ")
    ))
  }
  message(sprintf(
    "%s: %d of %d station(s) kept", label, length(stations_ok),
    length(unique(df$Station))
  ))

  df %>% filter(.data$Station %in% stations_ok)
}

# Build the multi-line HETEROFOR forcing file header.
build_heterofor_header <- function(wind_height, wind_atten_coef, source_lines) {
  column_header <- paste(
    "# Year", "Month", "Day", "Hour(GMT+1)",
    "Radiation (W/m2)",
    "Air temperature (degC)",
    "Soil surface temperature (degC)",
    "Rainfall (l/m2 or mm)",
    "Relative humidity (%)",
    "Wind speed (m/s)",
    "Wind direction (deg)",
    "Diffuse to global ratio",
    sep = "\t"
  )
  c(
    paste0("# Generated on ", format(Sys.time(), "%d-%m-%Y %H:%M")),
    paste0("windSpeedMeasurementHeight = ", wind_height),
    paste0("windSpeedAttenuationCoefficient = ", wind_atten_coef),
    source_lines,
    "",
    column_header
  )
}

# Turn a station name into a filesystem-safe file name component.
safe_station_name <- function(station_name) {
  str_replace_all(station_name, "[^A-Za-z0-9_-]", "_")
}

# Write one tab-delimited forcing file per station found in df.
write_station_files <- function(df, out_dir, header, suffix) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  write_one <- function(df_station, station_name) {
    file_name <- file.path(
      out_dir, paste0(safe_station_name(station_name), suffix)
    )
    writeLines(header, file_name)
    write.table(
      df_station, file = file_name, sep = "\t", row.names = FALSE,
      col.names = FALSE, quote = FALSE, append = TRUE, na = ""
    )
    file_name
  }

  written <- df %>%
    group_by(.data$Station) %>%
    group_map(function(df_station, keys) write_one(df_station, keys$Station))

  message(sprintf("Wrote %d forcing file(s) to %s", length(written), out_dir))
  invisible(written)
}

## ---------------------------------------------------------------------
## 6. Combining SAFRAN with observed RENECOFOR data
## ---------------------------------------------------------------------

# Build a complete hourly date grid per station, from 1 January of the first
# available RENECOFOR year through the last available RENECOFOR record.
build_station_hourly_grid <- function(reneco) {
  reneco %>%
    group_by(.data$Station) %>%
    summarise(
      first_year = year(min(.data$date, na.rm = TRUE)),
      last_date = max(.data$date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      start_date = make_datetime(.data$first_year, 1, 1, 0),
      end_date = floor_date(.data$last_date, unit = "hour")
    ) %>%
    rowwise() %>%
    mutate(date = list(seq(.data$start_date, .data$end_date, by = "hour"))) %>%
    unnest(date) %>%
    ungroup() %>%
    select("Station", "date")
}

# Combine a complete hourly grid with RENECOFOR and SAFRAN data: radiation
# always comes from SAFRAN, other variables use RENECOFOR first and fall
# back to SAFRAN where RENECOFOR is missing.
fill_with_safran <- function(complete_grid, reneco_fill, safran_fill) {
  complete_grid %>%
    left_join(reneco_fill, by = c("Station", "date")) %>%
    left_join(safran_fill, by = c("Station", "date")) %>%
    mutate(
      filled_Radiation = !is.na(.data$Radiation_safran),
      Radiation = .data$Radiation_safran,

      filled_Tair = is.na(.data$Air_temperature_reneco) &
        !is.na(.data$Air_temperature_safran),
      Air_temperature = coalesce(
        .data$Air_temperature_reneco, .data$Air_temperature_safran
      ),

      filled_RH = is.na(.data$Relative_humidity_reneco) &
        !is.na(.data$Relative_humidity_safran),
      Relative_humidity = coalesce(
        .data$Relative_humidity_reneco, .data$Relative_humidity_safran
      ),

      filled_Rainfall = is.na(.data$Rainfall_reneco) &
        !is.na(.data$Rainfall_safran),
      Rainfall = coalesce(.data$Rainfall_reneco, .data$Rainfall_safran),

      filled_Wind = is.na(.data$Wind_speed_reneco) &
        !is.na(.data$Wind_speed_safran),
      Wind_speed = coalesce(
        .data$Wind_speed_reneco, .data$Wind_speed_safran
      ),

      Soil_surface_temperature = .data$Air_temperature,
      Wind_direction = coalesce(
        .data$Wind_direction_reneco, .data$Wind_direction_safran, 0
      ),
      Diffuse_to_global_ratio = 0.5,

      Year = year(.data$date),
      Month = month(.data$date),
      Day = day(.data$date),
      Hour = hour(.data$date)
    ) %>%
    select(all_of(c(
      heterofor_column_order,
      "filled_Radiation", "filled_Tair", "filled_RH",
      "filled_Rainfall", "filled_Wind"
    )))
}

# Per-station summary of how many hours were filled from SAFRAN.
summarise_filling <- function(meteo_filled) {
  meteo_filled %>%
    group_by(.data$Station) %>%
    summarise(
      start = make_datetime(
        first(.data$Year), first(.data$Month), first(.data$Day),
        first(.data$Hour)
      ),
      end = make_datetime(
        last(.data$Year), last(.data$Month), last(.data$Day),
        last(.data$Hour)
      ),
      n_hours = n(),
      n_filled_Radiation = sum(.data$filled_Radiation, na.rm = TRUE),
      pct_filled_Radiation = 100 * .data$n_filled_Radiation / .data$n_hours,
      n_filled_Tair = sum(.data$filled_Tair, na.rm = TRUE),
      pct_filled_Tair = 100 * .data$n_filled_Tair / .data$n_hours,
      n_filled_RH = sum(.data$filled_RH, na.rm = TRUE),
      pct_filled_RH = 100 * .data$n_filled_RH / .data$n_hours,
      n_filled_Rainfall = sum(.data$filled_Rainfall, na.rm = TRUE),
      pct_filled_Rainfall = 100 * .data$n_filled_Rainfall / .data$n_hours,
      n_filled_Wind = sum(.data$filled_Wind, na.rm = TRUE),
      pct_filled_Wind = 100 * .data$n_filled_Wind / .data$n_hours,
      .groups = "drop"
    )
}

# Per-station summary of remaining missing values after filling.
summarise_missing <- function(meteo_filled) {
  meteo_filled %>%
    select(all_of(heterofor_column_order)) %>%
    group_by(.data$Station) %>%
    summarise(
      n_hours = n(),
      n_missing = sum(is.na(across(everything()))),
      missing_Radiation = sum(is.na(.data$Radiation)),
      missing_Tair = sum(is.na(.data$Air_temperature)),
      missing_RH = sum(is.na(.data$Relative_humidity)),
      missing_Rainfall = sum(is.na(.data$Rainfall)),
      missing_Wind = sum(is.na(.data$Wind_speed)),
      .groups = "drop"
    )
}

# Select the columns needed from the RENECOFOR / SAFRAN tables before the
# join, renaming them with a source-specific suffix.
select_with_suffix <- function(df, suffix) {
  renamed <- setNames(
    heterofor_column_order[-(1:5)],
    paste0(heterofor_column_order[-(1:5)], suffix)
  )
  df %>% select("Station", "date", all_of(renamed))
}

# Combine the SAFRAN-derived table with observed RENECOFOR data: returns the
# filled table together with the filling and missing-value summaries.
combine_with_renecofor <- function(meteo_heterofor, reneco_rds_path,
                                   safran_end_date) {
  reneco_raw <- readRDS(reneco_rds_path)
  if (!"Station" %in% names(reneco_raw)) {
    stop(
      "RENECOFOR RDS file must contain a 'Station' column: ", reneco_rds_path
    )
  }

  meteo_heterofor_clean <- meteo_heterofor %>%
    mutate(
      date = make_datetime(.data$Year, .data$Month, .data$Day, .data$Hour)
    ) %>%
    filter(.data$date < as.POSIXct(safran_end_date, tz = "UTC"))

  reneco <- reneco_raw %>%
    mutate(date = make_datetime(.data$Year, .data$Month, .data$Day, .data$Hour))

  complete_grid <- build_station_hourly_grid(reneco)
  safran_fill <- select_with_suffix(meteo_heterofor_clean, "_safran")
  reneco_fill <- select_with_suffix(reneco, "_reneco")

  meteo_filled <- fill_with_safran(complete_grid, reneco_fill, safran_fill)

  list(
    meteo_filled = meteo_filled,
    filling_summary = summarise_filling(meteo_filled),
    missing_summary = summarise_missing(meteo_filled)
  )
}

## ---------------------------------------------------------------------
## 7. Main
## ---------------------------------------------------------------------

main <- function() {
  opt <- parse_cli_args()

  message("Reading SAFRAN data...")
  stationdat <- read_station_metadata(opt$stations)
  safran_main <- read_safran_source(
    opt$safran_data, safran_main_vars, stationdat$Station
  )
  safran_psurf <- read_safran_source(
    opt$psurf_data, safran_psurf_vars, stationdat$Station
  )
  safran_long <- full_join(safran_main, safran_psurf, by = c("Station", "date"))

  message("Building HETEROFOR forcing table from SAFRAN...")
  meteo <- assemble_safran_table(safran_long)
  meteo_heterofor <- to_heterofor_table(meteo)
  log_rh_diagnostics(meteo_heterofor)

  safran_complete <- drop_incomplete_stations(meteo_heterofor, "SAFRAN-only")
  write_station_files(
    safran_complete,
    out_dir = opt$out_safran_dir,
    header = build_heterofor_header(
      opt$wind_height_safran, opt$wind_atten_coef,
      "# Radiation and meteorological data from SAFRAN / ORCHIDEE France 8 km"
    ),
    suffix = "_HETEROFOR_meteo.txt"
  )

  if (!is.null(opt$reneco_rds)) {
    message("Combining SAFRAN with observed RENECOFOR data...")
    combined <- combine_with_renecofor(
      meteo_heterofor, opt$reneco_rds, opt$safran_end_date
    )

    if (!is.null(opt$filling_summary)) {
      write.table(
        combined$filling_summary, file = opt$filling_summary, sep = "\t",
        row.names = FALSE, na = "NA", dec = "."
      )
    }
    if (!is.null(opt$missing_summary)) {
      write.table(
        combined$missing_summary, file = opt$missing_summary, sep = "\t",
        row.names = FALSE, na = "NA", dec = "."
      )
    }

    combined_complete <- drop_incomplete_stations(
      combined$meteo_filled %>% select(all_of(heterofor_column_order)),
      "RENECOFOR + SAFRAN"
    )
    write_station_files(
      combined_complete,
      out_dir = opt$out_combined_dir,
      header = build_heterofor_header(
        opt$wind_height_combined, opt$wind_atten_coef,
        c(
          "# RENECOFOR meteorological observations, completed with SAFRAN",
          "# where needed. Radiation is always taken from SAFRAN."
        )
      ),
      suffix = "_HETEROFOR_meteo_RENECOFOR.txt"
    )
  }

  message("Done.")
}

if (identical(environment(), globalenv()) && sys.nframe() == 0L) {
  main()
}
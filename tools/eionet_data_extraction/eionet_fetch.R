# Fetch species data from the EIONET Article 12 (birds) and Article 17 (other
# groups) web tools by querying the public pages and parsing the HTML tables.
#
# Two input modes:
#   - manual : one taxon, one or several species, one or several periods
#   - table  : an input table (columns Taxon, Period, Species, Country)
#              describing one query per row; several taxa can be mixed
# The EIONET article is derived from the taxon: Birds -> Article 12,
# every other taxonomic group -> Article 17.

# Note: parts of this code were written with the assistance of an AI model (Claude, developed by Anthropic).

#Load packages
suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(xml2)
  library(optparse)
  library(dplyr)
})


# Load arguments
# Fallback operator: use `b` when `a` is NULL, empty or NA
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

option_list <- list(
  make_option("--mode",          type="character", default="manual",
              help="Input mode: 'manual' (form values) or 'table' (queries read from --table)."),
  make_option("--taxon",         type="character", default=NULL,
              help="Manual mode: taxon (Birds, Mammals, Reptiles, ...). Birds use Article 12, other groups Article 17."),
  make_option("--include_eu",    action="store_true", default=FALSE,
              help="Table mode only: also retrieve the EU-level assessment for every query."),
  make_option("--table",         type="character", default=NULL,
              help="Table mode only: path to the query table (Taxon, Period, Species, Country)."),
  make_option("--subject",       type="character",
              help="Species latin name(s), comma-separated (manual mode)."),
  make_option("--period",        type="character",
              help="Reporting period(s), comma-separated (manual mode)."),
  make_option("--country",       type="character", default=NULL,
              help="Filter by Member State code(s), comma-separated. 'EU' selects the EU-level assessment table (Article 17: not available for 2019-2024)."),
  make_option("--region",        type="character", default=NULL,
              help="Article 17 taxa only: filter by bio-geographical region code (ALP, ATL, BLS, BOR, CON, MED, PAN). Empty = all regions.")
)

opt <- parse_args(OptionParser(option_list=option_list))

# Browser-like headers: the EIONET servers may reject the default httr user agent
UA_HEADERS <- add_headers(
  "User-Agent"      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
  "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language" = "en-US,en;q=0.9"
)

# ==================
# ARTICLE 12 (BIRDS)
# ==================
run_art12 <- function(opt) {

  BASE_URL <- "https://nature-art12.eionet.europa.eu/article12/summary" #URL for the EIONET article on 12 birds

  message("Recovery : ", opt$subject, " (", opt$period, ")")

  # Step 1: the site uses internal numeric codes for periods. Rather than
  # hardcoding them (they change with each new reporting cycle), read them
  # from the <select name="period"> dropdown of the live page.

  discover_resp <- GET(
    BASE_URL,
    query = list(subject = opt$subject),
    UA_HEADERS,
    add_headers("Referer" = "https://nature-art12.eionet.europa.eu/article12/"),
    timeout(30)
  )
  if (status_code(discover_resp) == 404) {
    stop("EIONET returned 404 (not found) for species '", opt$subject, "'. ",
         "This almost always means the species latin name is misspelled or does not match ",
         "the exact spelling used on the EIONET Article 12 portal -- please check --subject.")
  }
  if (http_error(discover_resp)) stop("HTTP ERROR (period discovery) ", status_code(discover_resp))
  discover_html <- read_html(content(discover_resp, as="text", encoding="UTF-8"))

  period_select <- html_node(discover_html, "select[name='period']")
  if (is.na(period_select)) {
    stop("Could not locate the period dropdown on the EIONET Article 12 page. The site's HTML structure may have changed; this script will need to be updated.")
  }

  period_opts   <- html_nodes(period_select, "option")
  period_labels <- trimws(html_text(period_opts))
  period_values <- html_attr(period_opts, "value")
  message("Periods available on the site : ", paste(period_labels, collapse=", "))

  period_idx <- which(period_labels == opt$period)
  if (length(period_idx) == 0) {
    stop("Period '", opt$period, "' was not found among the site's available periods (",
         paste(period_labels, collapse=", "), "). Check --period.")
  }
  period_val <- period_values[period_idx[1]]
  message("Resolved period '", opt$period, "' -> internal code '", period_val, "'")

  # Step 2: request the page for the chosen species and period
  response <- GET(
    BASE_URL,
    query = list(
      period        = period_val,
      subject       = opt$subject
    ),
    UA_HEADERS,
    add_headers("Referer" = "https://nature-art12.eionet.europa.eu/article12/"),
    timeout(30)
  )
  if (http_error(response)) stop("HTTP ERROR ", status_code(response))

  html <- read_html(content(response, as="text", encoding="UTF-8")) #extract html content

  # All results are in one <table class="complex_datatable"> holding the Member
  # State (MS) rows, followed (when available) by the EU assessment rows.

  table_node <- html_node(html, "table.complex_datatable") #search the first element (main table)
  if (is.na(table_node)) stop("Table not found. Check the species name.")
  ##if table doesn't find, species name incorrect or empty page

  normalize_row <- function(row_vals, n_cols) {
    if (length(row_vals) < n_cols) {
      row_vals <- c(row_vals, rep("", n_cols - length(row_vals)))
    } else if (length(row_vals) > n_cols) {
      row_vals <- row_vals[seq_len(n_cols)]
    }
    row_vals
  }

  # The table layout changed with the 2019-2024 cycle:
  #   - 2008-2012 / 2013-2018: MS rows contain an <a data-title="Factsheet"> link
  #   - 2019-2024: MS rows start with a cell flagged data-tooltip="html"
  # The Factsheet link is checked first because data-tooltip cells can also
  # appear elsewhere in older pages, so it is the more specific marker.


  is_old_format <- length(html_nodes(table_node, xpath=".//a[@data-title='Factsheet']")) > 0
  is_new_format <- !is_old_format && length(html_nodes(table_node, xpath=".//tr[td[@data-tooltip='html']][td/span]")) > 0
  message("Table layout detected : ", if (is_old_format) "pre-2019-style (Factsheet link)" else if (is_new_format) "2019-2024-style (data-tooltip)" else "UNKNOWN")

  if (is_new_format) {

    # ---- 2019-2024 layout: 45 cells per MS row (country code + 44 values) ----

    data_row_nodes <- html_nodes(table_node, xpath=".//tr[td[@data-tooltip='html']]")
    message("Extracted MS lines : ", length(data_row_nodes))
    if (length(data_row_nodes) == 0) stop("No MS data found. Please check the species and time period.")

    # Column names, in the order of the table cells
    # (ST/LT = short-/long-term trend; "Info" columns hold the site's info marker)

    ms_col_names <- c(
      "MS",
      # Breeding population - Population size (8 cols)
      "BreedPop_Min", "BreedPop_Max", "BreedPop_BestValue", "BreedPop_Unit",
      "BreedPop_TypeEst", "BreedPop_Change", "BreedPop_Info", "BreedPop_PctEU",
      # Breeding population - Population trend (7 cols)
      "BreedPop_ST_Period", "BreedPop_ST_Direction", "BreedPop_ST_Magnitude",
      "BreedPop_LT_Period", "BreedPop_LT_Direction", "BreedPop_LT_Magnitude",
      "BreedPop_TrendInfo",
      # Breeding distribution - Distribution size (3 cols)
      "BreedDist_Area_km2", "BreedDist_Info", "BreedDist_PctEU",
      # Breeding distribution - Distribution trend (7 cols)
      "BreedDist_ST_Period", "BreedDist_ST_Direction", "BreedDist_ST_Magnitude",
      "BreedDist_LT_Period", "BreedDist_LT_Direction", "BreedDist_LT_Magnitude",
      "BreedDist_TrendInfo",
      # Winter population - Population size (8 cols)
      "WintPop_Min", "WintPop_Max", "WintPop_BestValue", "WintPop_Unit",
      "WintPop_TypeEst", "WintPop_Change", "WintPop_Info", "WintPop_PctEU",
      # Winter population - Population trend (8 cols, includes a trailing Status)
      "WintPop_ST_Period", "WintPop_ST_Direction", "WintPop_ST_Magnitude",
      "WintPop_LT_Period", "WintPop_LT_Direction", "WintPop_LT_Magnitude",
      "WintPop_TrendInfo", "WintPop_Status",
      # Breeding area from gridded maps (2 cols)
      "GriddedMap_BreedArea_km2", "GriddedMap_BreedArea_PctEU",
      # Subspecific unit (1 col)
      "Ssp_unit"
    )

    extract_row_new <- function(row) {
      cells <- html_nodes(row, "td")
      ms_span <- xml2::xml_find_first(cells[[1]], "./span")
      ms_code <- if (!inherits(ms_span, "xml_missing")) {
        trimws(xml2::xml_text(ms_span))
      } else {
        trimws(html_text(cells[[1]]))
      }
      rest <- trimws(html_text(cells[-1]))
      c(ms_code, rest)
    }

    ms_rows_list <- lapply(data_row_nodes, extract_row_new)
    row_lengths   <- sapply(ms_rows_list, length)
    message("Cell counts across MS rows : ", paste(sort(unique(row_lengths)), collapse=", "),
            " (expected ", length(ms_col_names), ")")

    ms_mat <- do.call(rbind, lapply(ms_rows_list, normalize_row, n_cols=length(ms_col_names)))
    df_ms  <- as.data.frame(ms_mat, stringsAsFactors=FALSE)
    colnames(df_ms) <- ms_col_names
    df_ms <- cbind(Species=opt$subject, df_ms)

    # EU rows: same 20-column skeleton as the older layout. Not verified for
    # 2019-2024, as no EU section was present in the pages tested so far.

    eu_col_names <- c(
      "EU",
      "EU_BreedPop_Min", "EU_BreedPop_Max", "EU_BreedPop_Unit",
      "EU_BreedPop_ShortTrend_Dir", "EU_BreedPop_LongTrend_Dir",
      "EU_BreedDist_Area",
      "EU_BreedDist_ShortTrend_Dir", "EU_BreedDist_LongTrend_Dir",
      "EU_WintPop_Min", "EU_WintPop_Max", "EU_WintPop_Unit",
      "EU_WintPop_ShortTrend_Dir", "EU_WintPop_LongTrend_Dir",
      "EU_GriddedMap_Breeding", "EU_GriddedMap_Wintering",
      "EU_PopStatus", "EU_ContribTarget1",
      "EU_Season", "EU_PreviousStatus"
    )

    eu_section_rows <- html_nodes(table_node, xpath=".//tr[td[starts-with(normalize-space(text()),'EU')]]")
    df_eu <- NULL
    if (length(eu_section_rows) > 0) {
      message("Extracted EU lines : ", length(eu_section_rows))
      eu_rows_list <- lapply(eu_section_rows, function(row) trimws(html_text(html_nodes(row, "td"))))
      eu_mat <- do.call(rbind, lapply(eu_rows_list, normalize_row, n_cols=length(eu_col_names)))
      df_eu  <- as.data.frame(eu_mat, stringsAsFactors=FALSE)
      colnames(df_eu) <- eu_col_names[seq_len(ncol(df_eu))]
      df_eu <- cbind(Species=opt$subject, df_eu)
    } else {
      message("Extracted EU lines : 0 (no EU assessment section found on the page for this selection)")
    }

  } else if (is_old_format) {

    # ---- pre-2019-style layout (2008-2012 / 2013-2018) ----
    all_rows <- html_nodes(table_node, "tr") #Retrieves all <tr> tags from the array and places them in a list.
    message("Rows in HTML : ", length(all_rows))

    # ---- Function : extract the values from a row of data ----
    extract_row_values <- function(row) {
      cells <- html_nodes(row, css="td, th")
      vals  <- character(length(cells))
      for (i in seq_along(cells)) {
        cell <- cells[[i]]
        # Link factsheet = country code
        a_fs <- tryCatch(xml_find_first(cell, ".//a[@data-title='Factsheet']"), error=function(e) NULL) #country code
        if (!is.null(a_fs) && !inherits(a_fs, "xml_missing")) {
          vals[i] <- trimws(xml_text(a_fs))
          next
        }
        # div.first-subcolumn = numeric value or main text
        div_fs <- tryCatch(xml_find_first(cell, ".//div[contains(@class,'first-subcolumn')]"), error=function(e) NULL) #value
        if (!is.null(div_fs) && !inherits(div_fs, "xml_missing")) {
          vals[i] <- trimws(xml_text(div_fs))
          next
        }
        # Otherwise, plain text (trends, status updates, etc.)
        vals[i] <- trimws(xml_text(cell))
      }
      vals
    }

    # ---- Separation of MS rows and EU rows ----
    ms_rows <- list() #rows with countries
    eu_rows <- list() #rows of EU (EU28 or EU27)
    in_eu_section <- FALSE #becomes TRUE when we found "EU population status assessments" line

    for (row in all_rows) {
      cls <- xml_attr(row, "class") %||% ""

      # Detect a change in section
      if (grepl("section-header", cls)) {
        txt <- trimws(xml_text(row))
        if (grepl("EU population status", txt)) in_eu_section <- TRUE
        next
      }

      # MS data rows: contain a link to the factsheet
      has_factsheet <- length(xml_find_all(row, ".//a[@data-title='Factsheet']")) > 0
      if (!in_eu_section && has_factsheet) {
        ms_rows <- c(ms_rows, list(extract_row_values(row)))
        next
      }

      # EU rows : "EU28" or"EU27"
      if (in_eu_section) {
        txt <- trimws(xml_text(xml_find_first(row, ".//td")))
        if (!is.na(txt) && grepl("^EU", txt)) {
          eu_rows <- c(eu_rows, list(extract_row_values(row)))
        }
      }
    }

    message("Extracted MS lines : ", length(ms_rows))
    message("Extracted EU lines : ", length(eu_rows))

    if (length(ms_rows) == 0) stop("No MS data found. Please check the species and time period.")

    # ---- MS columns (45 fixed columns) ----
    ms_col_names <- c(
      "MS",
      # Breeding population - Population size (8 cols)
      "BreedPop_Min", "BreedPop_Max", "BreedPop_BestEst",
      "BreedPop_Unit", "BreedPop_SizeMethod", "BreedPop_TrendMethod",
      "BreedPop_PctEU",
      # Breeding population - Population trend short-term (7 cols)
      "BreedPop_ShortTrend_Period", "BreedPop_ShortTrend_Dir",
      "BreedPop_ShortTrend_Magnitude",
      # Breeding population - Population trend long-term (4 cols)
      "BreedPop_LongTrend_Period", "BreedPop_LongTrend_Dir",
      "BreedPop_LongTrend_Magnitude",
      "BreedPop_TrendQuality",
      # Breeding distribution - Distribution size (3 cols)
      "BreedDist_Area", "BreedDist_Unit", "BreedDist_PctEU",
      # Breeding distribution - Distribution trend short-term (4 cols)
      "BreedDist_ShortTrend_Period", "BreedDist_ShortTrend_Dir",
      "BreedDist_ShortTrend_Magnitude",
      # Breeding distribution - Distribution trend long-term (3 cols)
      "BreedDist_LongTrend_Period", "BreedDist_LongTrend_Dir",
      "BreedDist_LongTrend_Magnitude",
      # Winter population - Population size (8 cols)
      "WintPop_Min", "WintPop_Max", "WintPop_BestEst",
      "WintPop_Unit", "WintPop_SizeMethod", "WintPop_TrendMethod",
      "WintPop_PctEU",
      # Winter population - Population trend short-term (4 cols)
      "WintPop_ShortTrend_Period", "WintPop_ShortTrend_Dir",
      "WintPop_ShortTrend_Magnitude",
      # Winter population - Population trend long-term (4 cols)
      "WintPop_LongTrend_Period", "WintPop_LongTrend_Dir",
      "WintPop_LongTrend_Magnitude",
      "WintPop_TrendQuality",
      # Breeding area from gridded maps (2 cols)
      "GriddedMap_BreedArea_km2", "GriddedMap_BreedArea_PctEU",
      # Subspecific unit (1 col)
      "Ssp_unit"
    )

    ms_mat <- do.call(rbind, lapply(ms_rows, normalize_row, n_cols=45))
    df_ms  <- as.data.frame(ms_mat, stringsAsFactors=FALSE)
    colnames(df_ms) <- ms_col_names
    df_ms <- cbind(Species=opt$subject, df_ms)

    # ---- EU columns (20 fixed columns) ----
    eu_col_names <- c(
      "EU",
      # Breeding population
      "EU_BreedPop_Min", "EU_BreedPop_Max", "EU_BreedPop_Unit",
      "EU_BreedPop_ShortTrend_Dir", "EU_BreedPop_LongTrend_Dir",
      # Breeding distribution
      "EU_BreedDist_Area",
      "EU_BreedDist_ShortTrend_Dir", "EU_BreedDist_LongTrend_Dir",
      # Winter population
      "EU_WintPop_Min", "EU_WintPop_Max", "EU_WintPop_Unit",
      "EU_WintPop_ShortTrend_Dir", "EU_WintPop_LongTrend_Dir",
      # Status
      "EU_GriddedMap_Breeding", "EU_GriddedMap_Wintering",
      "EU_PopStatus", "EU_ContribTarget1",
      "EU_Season", "EU_PreviousStatus"
    )

    df_eu <- NULL
    if (length(eu_rows) > 0) {
      eu_mat <- do.call(rbind, lapply(eu_rows, normalize_row, n_cols=20))
      df_eu  <- as.data.frame(eu_mat, stringsAsFactors=FALSE)
      colnames(df_eu) <- eu_col_names[seq_len(ncol(df_eu))]
      df_eu <- cbind(Species=opt$subject, df_eu)
    }

  } else {
    stop("Could not detect a known table layout (neither the pre-2019 Factsheet-link marker nor the 2019-2024 data-tooltip marker was found). The site's HTML may have changed again; this script will need updating.")
  }

  # ---- Filter by country (multiple selections allowed) ----
  # opt$country may contain several codes separated by commas
  # "EU" is a special case -> EU population status assessments section
  show_ms <- TRUE
  show_eu <- FALSE
  cf_ms   <- NULL  # selected MS country codes
  cf_all  <- NULL  # all selected codes (for metadata)

  if (!is.null(opt$country) && nchar(trimws(opt$country)) > 0) {
    cf_all <- toupper(trimws(unlist(strsplit(opt$country, ","))))
    cf_all <- cf_all[nchar(cf_all) > 0]

    # Separate EU from MS codes
    show_eu        <- "EU" %in% cf_all
    has_all_countries <- "ALL_COUNTRIES" %in% cf_all

    if (has_all_countries) {
      show_ms <- TRUE
    } else {
      cf_ms <- cf_all[cf_all != "EU"]
      if (length(cf_ms) > 0) {
        # Filter MS lines
        df_ms$MS <- trimws(df_ms$MS)
        df_ms     <- df_ms[toupper(df_ms$MS) %in% cf_ms, , drop=FALSE]
        if (nrow(df_ms) == 0) message("WARNINGS : no data for : ", paste(cf_ms, collapse=", "))
        show_ms <- TRUE
      } else {
        # EU only: no MS section
        show_ms <- FALSE
      }
    }
  } else {
    # No filter: show all (MS + EU)
    show_eu <- TRUE
    show_ms <- TRUE
  }

  message("Final MS lines : ", nrow(df_ms))

  # Return the tables (header-only when there is nothing to report);
  # the caller aggregates the results of all queries and writes the files.
  if (!(show_eu && !is.null(df_eu) && nrow(df_eu) > 0)) {
    df_eu <- data.frame(matrix(ncol=length(eu_col_names)+1, nrow=0))
    colnames(df_eu) <- c("Species", eu_col_names)
  }
  list(ms = if (show_ms) df_ms else df_ms[0, , drop=FALSE], eu = df_eu)
}

# ============================================================
# ARTICLE 17 (ALL GROUPS EXCEPT BIRDS)
# ============================================================
run_art17 <- function(opt) {
 
  if (is.null(opt$group) || nchar(trimws(opt$group)) == 0) {
    stop("--group is required for --source art17 (e.g. Mammals, Reptiles, Vascular plants...).")
  }
 
  BASE_URL17 <- "https://nature-art17.eionet.europa.eu/article17/species/summary/"
 
  message("Fetching Article 17 data : ", opt$subject, " (group: ", opt$group, ", period: ", opt$period, ")")
 
  # Step 1: translate the period label into the site's internal code by reading
  # the <select name="period"> dropdown (codes are not a simple sequence).
  discover_resp <- GET(
    BASE_URL17,
    query = list(group = opt$group),
    UA_HEADERS,
    add_headers("Referer" = "https://nature-art17.eionet.europa.eu/article17/"),
    timeout(30)
  )
  if (status_code(discover_resp) == 404) { # unknown group name -> 404
    stop("EIONET returned 404 (not found) for group '", opt$group, "'. ",
         "Check --group matches one of the site's taxonomic groups exactly.")
  }
  if (http_error(discover_resp)) stop("HTTP ERROR (period discovery) ", status_code(discover_resp))
  discover_html <- read_html(content(discover_resp, as="text", encoding="UTF-8"))
 
  period_select <- html_node(discover_html, "select[name='period']")
  if (is.na(period_select)) {
    stop("Could not locate the period dropdown on the EIONET Article 17 page. The site's HTML structure may have changed; this script will need to be updated.")
  }
 
  period_opts   <- html_nodes(period_select, "option")
  period_labels <- trimws(html_text(period_opts))      # e.g. "2019-2024"
  period_values <- html_attr(period_opts, "value")     # internal code
  message("Periods available on the site : ", paste(period_labels, collapse=", "))
 
  period_idx <- which(period_labels == opt$period)     # match the label chosen by the user
  if (length(period_idx) == 0) {
    stop("Period '", opt$period, "' was not found among the site's available periods (",
         paste(period_labels, collapse=", "), "). Check --period.")
  }
  period_code <- period_values[period_idx[1]]
  message("Resolved period '", opt$period, "' -> internal code '", period_code, "'")
 
  # Log the available bio-regions (information only, useful for debugging)
  region_select <- html_node(discover_html, "select[name='region']")
  if (!is.na(region_select)) {
    region_labels <- trimws(html_text(html_nodes(region_select, "option")))
    message("Bio-regions available on the site : ", paste(region_labels, collapse=", "))
  }
 
  # Step 2: request the data (the optional bio-region filter is applied server-side)
  query_list <- list(group = opt$group, period = period_code, subject = opt$subject)
  if (!is.null(opt$region) && nchar(trimws(opt$region)) > 0 && toupper(trimws(opt$region)) != "ALL") {
    query_list$region <- toupper(trimws(opt$region))
  }
 
  response <- GET(
    BASE_URL17,
    query = query_list,
    UA_HEADERS,
    add_headers("Referer" = "https://nature-art17.eionet.europa.eu/article17/"),
    timeout(30)
  )
  if (http_error(response)) stop("HTTP ERROR ", status_code(response))
 
  html <- read_html(content(response, as="text", encoding="UTF-8"))
 
  # The page has one <table class="complex_datatable"> with several <tbody>:
  #   - MS rows: their first cell is flagged data-tooltip="html"
  #   - EU biogeographical assessment rows: the <tbody> right after
  #     <tbody id="manual-thead"> (not published for 2019-2024)
  # Small "popout" tooltip tables are nested in the header cells; selecting
  # rows through the markers above avoids picking them up.
  table_node <- html_node(html, "table.complex_datatable")
 
  if (is.na(table_node)) {
    stop("No table.complex_datatable found for this species/group/period/region combination. ",
         "This can mean either that the species has no data for this selection, ",
         "or that the page structure differs from what this script expects (check the species Latin name).")
  }
 
  # Trim and collapse whitespace/newlines inside cell text
  collapse_ws <- function(x) trimws(gsub("\\s+", " ", x))
 
  # Pad (with NA) or truncate a row to exactly n values
  normalize_row <- function(vals, n) {
    if (length(vals) < n) vals <- c(vals, rep(NA_character_, n - length(vals)))
    vals[seq_len(n)]
  }
 
  # ---- Member State rows ----
  data_row_nodes <- html_nodes(table_node, xpath = ".//tr[td[@data-tooltip='html']]")
  message("Genuine MS data rows found : ", length(data_row_nodes))
 
  if (length(data_row_nodes) == 0) {
    stop("No data rows could be parsed for this species/group/period/region combination. ",
         "Check the species Latin name and group, or inspect the raw HTML if this persists -- ",
         "the site's markup may have changed since this script was written.")
  }
 
  # Column names per layout, in the order of the table cells.
  # Each MS row ends with one empty spacer cell, which is dropped.
  # 2019-2024 layout: 38 columns
  col_names_2019 <- c(
    "MS", "Region",
    # Range (5)
    "Range_Surface_km2", "Range_Status_pctMS", "Range_Trend",
    "Range_FRR", "Range_vs_FRR_predefined",
    # Population (13)
    "Population_Min", "Population_Max", "Population_BestValue",
    "Population_Unit_QualityExtrapolation", "Population_SizeClass",
    "Population_TypeEst", "Population_Method", "Population_Status_pctMS",
    "Population_Trend", "Population_TrendMagnitude_MinMax",
    "Population_FRP", "Population_vs_FRP_predefined", "Population_Unit",
    # Habitat for the species (5)
    "Habitat_AreaOccupiedSufficient", "Habitat_QualityOccupiedSufficient",
    "Habitat_UnoccupiedSufficient", "Habitat_Status", "Habitat_Trend",
    # Future prospects (4)
    "FutureProspects_Range", "FutureProspects_Population",
    "FutureProspects_Habitat", "FutureProspects_Status",
    # Overall assessment (6)
    "OverallAssessment_CurrentCS", "OverallAssessment_CurrentCS_Trend",
    "OverallAssessment_PreviousCS", "OverallAssessment_PreviousCS_Trend",
    "OverallAssessment_Status_NatureOfChange", "OverallAssessment_CSTrend_NatureOfChange",
    # Distribution area (3)
    "DistributionArea_km2", "DistributionArea_Method", "DistributionArea_pctMS"
  )
 
  # 2007-2012 layout: 24 columns
  col_names_2007 <- c(
    "MS", "Region",
    # Range (4)
    "Range_Area_km2", "Range_pctMS", "Range_Trend", "Range_Ref",
    # Population (4)
    "Population_SizeUnit", "Population_pctMS", "Population_Trend", "Population_Ref",
    # Habitat for the species (5)
    "Habitat_Area_km2", "Habitat_pctMS", "Habitat_Quality", "Habitat_Trend", "Habitat_Suitable",
    # Future prospects (1)
    "FutureProspects",
    # Overall assessment (4)
    "OverallAssessment_CurrentCS", "OverallAssessment_Qualifier",
    "OverallAssessment_PreviousCS", "OverallAssessment_NatureOfChange",
    # Areas from gridded maps (4)
    "GriddedMap_Range_km2", "GriddedMap_Range_pctMS",
    "GriddedMap_Distrib_km2", "GriddedMap_Distrib_pctMS"
  )
 
  # 2013-2018 layout: 33 columns
  col_names_2013 <- c(
    "MS", "Region",
    # Range (4)
    "Range_Surface_km2", "Range_Status_pctMS", "Range_Trend", "Range_FRR",
    # Population (10)
    "Population_Min", "Population_Max", "Population_BestValue", "Population_Unit",
    "Population_TypeEst", "Population_Method", "Population_Status_pctMS",
    "Population_Trend", "Population_FRP", "Population_FRP_Unit",
    # Habitat for the species (4)
    "Habitat_OccupiedSufficient", "Habitat_UnoccupiedSufficient",
    "Habitat_Status", "Habitat_Trend",
    # Future prospects (4)
    "FutureProspects_Range", "FutureProspects_Population",
    "FutureProspects_Habitat", "FutureProspects_Status",
    # Overall assessment (6)
    "OverallAssessment_CurrentCS", "OverallAssessment_CurrentCS_Trend",
    "OverallAssessment_PreviousCS", "OverallAssessment_PreviousCS_Trend",
    "OverallAssessment_Status_NatureOfChange", "OverallAssessment_CSTrend_NatureOfChange",
    # Distribution area (3)
    "DistributionArea_km2", "DistributionArea_Method", "DistributionArea_pctMS"
  )
 
  # Extract the text of every cell of one MS row
  extract_row <- function(row) {
    cells <- html_nodes(row, "td")
    # The first cell hides a long tooltip (with its own <span>s). The country
    # code is a direct child: a <span> in 2019-2024, an <a> in 2007-2012.
    ms_node <- xml2::xml_find_first(cells[[1]], "./span|./a")
    ms_code <- if (!inherits(ms_node, "xml_missing")) {
      collapse_ws(xml2::xml_text(ms_node))
    } else {
      collapse_ws(html_text(cells[[1]]))
    }
    c(ms_code, collapse_ws(html_text(cells[-1])))
  }
 
  row_list    <- lapply(data_row_nodes, extract_row)
  row_lengths <- sapply(row_list, length)
  n_cells     <- max(row_lengths)
  message("Cell counts across MS rows : ", paste(sort(unique(row_lengths)), collapse=", "))
 
  # Pick the column names matching the number of cells per row
  if (n_cells == length(col_names_2019) + 1) {
    col_names <- col_names_2019
    message("MS table layout detected : 2019-2024-style (", length(col_names), " columns)")
  } else if (n_cells == length(col_names_2013) + 1) {
    col_names <- col_names_2013
    message("MS table layout detected : 2013-2018-style (", length(col_names), " columns)")
  } else if (n_cells == length(col_names_2007) + 1) {
    col_names <- col_names_2007
    message("MS table layout detected : 2007-2012-style (", length(col_names), " columns)")
  } else {
    n_keep <- max(n_cells - 1, 2)
    col_names <- c("MS", "Region", if (n_keep > 2) paste0("col_", 3:n_keep))
    message("WARNING : unrecognised MS table layout (", n_cells, " cells per row). ",
            "Using generic column names -- please report this period/group so the layout can be added.")
  }
 
  data_mat <- do.call(rbind, lapply(row_list, normalize_row, n = length(col_names)))
  df <- as.data.frame(data_mat, stringsAsFactors = FALSE)
  colnames(df) <- col_names
  df <- cbind(Species = opt$subject, Group = opt$group, Period = opt$period, df)
 
  # ---- EU biogeographical assessment (not available for 2019-2024) ----
  # Column names follow the table header; "Extra_1"/"Extra_2" have no label on
  # the site. The last column holds the date and institution of the assessment.
  # 2013-2018 layout: 34 columns
  eu_col_names_2013 <- c(
    "EU", "Region",
    "Range_Surface_km2", "Range_Status", "Range_Trend", "Range_FRR",
    "Population_Min", "Population_Max", "Population_BestValue", "Population_Unit",
    "Population_TypeEst", "Population_Method", "Population_Status",
    "Population_Trend", "Population_FRP", "Population_FRP_Unit",
    "Habitat_OccupiedSufficient", "Habitat_UnoccupiedSufficient",
    "Habitat_Status", "Habitat_Trend",
    "FutureProspects_Range", "FutureProspects_Population",
    "FutureProspects_Habitat", "FutureProspects_Status",
    "OverallAssessment_CurrentCS", "OverallAssessment_CurrentCS_Trend",
    "OverallAssessment_CS2012", "OverallAssessment_CS2012_Trend",
    "OverallAssessment_Status_NatureOfChange", "OverallAssessment_CSTrend_NatureOfChange",
    "CS_2001_2006_Backcasting", "Target1",
    "Extra_1", "Assessment_Info"
  )
 
  # 2007-2012 layout: 25 columns
  eu_col_names_2007 <- c(
    "EU", "Region",
    "Range_Area_km2", "Range_Conclusion", "Range_Trend", "Range_Ref",
    "Population_SizeUnit", "Population_Conclusion", "Population_Trend", "Population_Ref",
    "Habitat_Area_km2", "Habitat_Conclusion", "Habitat_Quality", "Habitat_Trend", "Habitat_Suitable",
    "FutureProspects_Conclusion",
    "OverallAssessment_CurrentCS_Conclusion", "OverallAssessment_Qualifier",
    "OverallAssessment_PreviousCS_Conclusion", "OverallAssessment_NatureOfChange",
    "Target1_Contrib", "Target1_Type",
    "Extra_1", "Extra_2",
    "Assessment_Info"
  )
 
  eu_section_rows <- html_nodes(
    table_node,
    xpath = ".//tbody[@id='manual-thead']/following-sibling::tbody[1]/tr[td]"
  )
  df_eu <- NULL
  if (length(eu_section_rows) > 0) {
    eu_rows_list <- lapply(eu_section_rows, function(row) collapse_ws(html_text(html_nodes(row, "td"))))
    # Keep EU-level rows only ("EU27", "EU28"...): the section also lists
    # corrections proposed by individual Member States
    eu_rows_list <- Filter(function(v) length(v) > 0 && grepl("^EU", v[1]), eu_rows_list)
  } else {
    eu_rows_list <- list()
  }
  message("EU biogeographical assessment rows found : ", length(eu_rows_list))
 
  if (length(eu_rows_list) > 0) {
    n_eu <- max(sapply(eu_rows_list, length))
    eu_names <- if (n_eu == length(eu_col_names_2013)) {
      eu_col_names_2013
    } else if (n_eu == length(eu_col_names_2007)) {
      eu_col_names_2007
    } else {
      message("WARNING : unrecognised EU table layout (", n_eu, " cells per row). Using generic column names.")
      c("EU", "Region", if (n_eu > 2) paste0("col_", 3:n_eu))
    }
    eu_mat <- do.call(rbind, lapply(eu_rows_list, normalize_row, n = length(eu_names)))
    df_eu  <- as.data.frame(eu_mat, stringsAsFactors = FALSE)
    colnames(df_eu) <- eu_names
    df_eu <- cbind(Species = opt$subject, Group = opt$group, Period = opt$period, df_eu)
  } else {
    message("No EU assessment available on the page for this selection (expected for 2019-2024).")
    df_eu <- data.frame(matrix(ncol = length(eu_col_names_2013) + 3, nrow = 0))
    colnames(df_eu) <- c("Species", "Group", "Period", eu_col_names_2013)
  }
 
  # ---- Country filter (same convention as Article 12) ----
  # "EU" selects the EU table, other codes select MS rows, "all_countries"
  # keeps every MS, no value returns both tables. Applied after fetching,
  # since the site has no country parameter.
  show_ms <- TRUE
  show_eu <- TRUE
  if (!is.null(opt$country) && nchar(trimws(opt$country)) > 0) {
    cf_all <- toupper(trimws(unlist(strsplit(opt$country, ","))))
    cf_all <- cf_all[nchar(cf_all) > 0]
    show_eu <- "EU" %in% cf_all
    if (!("ALL_COUNTRIES" %in% cf_all)) {
      cf_ms <- cf_all[cf_all != "EU"]
      # Greece is "GR" in older cycles but "EL" in 2019-2024: accept either
      if ("GR" %in% cf_ms || "EL" %in% cf_ms) cf_ms <- union(cf_ms, c("GR", "EL"))
      if (length(cf_ms) > 0) {
        df <- df[toupper(trimws(df$MS)) %in% cf_ms, , drop = FALSE]
        if (nrow(df) == 0) message("WARNINGS : no data for : ", paste(cf_ms, collapse = ", "))
      } else {
        show_ms <- FALSE # EU only: no MS rows
      }
    }
  }
 
  message("Final MS rows : ", nrow(df), " | EU rows : ", nrow(df_eu))
 
  # Return the tables; the caller aggregates the results of all queries and writes the files.
  list(ms = if (show_ms) df else df[0, , drop = FALSE],
       eu = if (show_eu) df_eu else df_eu[0, , drop = FALSE])
}
 
# ============================================================
# QUERY BUILDING (manual form or input table)
# ============================================================
OUT_DIR <- "output_files"

# Lower-case a string and drop everything but letters and digits (used to match
# column names and category labels leniently)
norm_key <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))

# Trim values, strip surrounding quotes, turn empty strings into NA
clean_cells <- function(x) {
  x <- trimws(gsub('^"|"$', "", x))
  x[!is.na(x) & !nzchar(x)] <- NA_character_
  x
}

# Split a delimited string into trimmed, unique, non-empty values.
# "None" is what Galaxy writes for an unselected optional parameter.
split_values <- function(x, pattern = "[,;]+") {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(character(0))
  vals <- trimws(unlist(strsplit(as.character(x), pattern)))
  unique(vals[nchar(vals) > 0 & vals != "None"])
}

# Birds are reported under Article 12; every other group under Article 17
BIRD_ALIASES <- c("birds", "bird", "aves")

GROUP_ALIASES <- list(
  "Amphibians"          = c("amphibians", "amphibian"),
  "Arthropods"          = c("arthropods", "arthropod"),
  "Fish"                = c("fish", "fishes"),
  "Mammals"             = c("mammals", "mammal"),
  "Molluscs"            = c("molluscs", "mollusc", "mollusks", "mollusk"),
  "Non-vascular plants" = c("nonvascularplants", "nonvascularplant"),
  "Other invertebrates" = c("otherinvertebrates"),
  "Reptiles"            = c("reptiles", "reptile"),
  "Vascular plants"     = c("vascularplants", "vascularplant")
)

# Return the canonical name whose alias list contains x (NA when unknown)
lookup_alias <- function(x, alias_list) {
  key <- norm_key(x)
  if (is.na(key)) return(NA_character_)
  for (nm in names(alias_list)) {
    if (key %in% alias_list[[nm]]) return(nm)
  }
  NA_character_
}

# Derive the EIONET article from a taxon label.
# Returns list(article, group) or NULL when the taxon is not recognised.
resolve_taxon <- function(x) {
  key <- norm_key(x)
  if (is.na(key)) return(NULL)
  if (key %in% BIRD_ALIASES) return(list(article = "art12", group = ""))
  grp <- lookup_alias(x, GROUP_ALIASES)
  if (!is.na(grp)) return(list(article = "art17", group = grp))
  NULL
}

ACCEPTED_TAXA <- paste(c("Birds", names(GROUP_ALIASES)), collapse = ", ")

# Merge the country cells of several table rows into one country filter string.
# Empty cell / "all" -> every Member State; "EU" is kept next to it when requested.
merge_countries <- function(cells) {
  has_blank <- any(is.na(cells))
  tokens    <- toupper(split_values(cells, "[,;|[:space:]]+"))
  has_all   <- has_blank || any(tokens %in% c("ALL", "ALL_COUNTRIES"))
  tokens    <- setdiff(tokens, c("ALL", "ALL_COUNTRIES"))
  if (has_all || length(tokens) == 0) {
    return(paste(c("all_countries", intersect(tokens, "EU")), collapse = ","))
  }
  paste(tokens, collapse = ",")
}

# ---- Manual mode: species x periods for the chosen article ----
build_manual_queries <- function(opt) {
  taxon <- resolve_taxon(opt$taxon)
  if (is.null(taxon)) {
    stop("Unrecognised --taxon: '", opt$taxon %||% "", "'. Accepted: ", ACCEPTED_TAXA, ".")
  }
  article <- taxon$article
  message("Taxon '", opt$taxon, "' -> ", article)
  species <- split_values(opt$subject)
  periods <- split_values(opt$period)
  if (length(species) == 0) stop("No species name provided (--subject).")
  if (length(periods) == 0) stop("No reporting period provided (--period).")

  q <- expand.grid(period = periods, species = species, stringsAsFactors = FALSE)
  q$article <- article
  q$group   <- taxon$group
  q$country <- paste(split_values(opt$country), collapse = ",")
  q$region  <- if (article == "art17") paste(split_values(opt$region), collapse = ",") else ""
  message("Manual mode: ", length(species), " species x ", length(periods), " period(s) = ", nrow(q), " query(ies)")
  list(queries = q[, c("article", "group", "period", "species", "country", "region")],
       skipped = NULL)
}

# ---- Table mode ----
# Read a tab-, comma- or semicolon-separated table (delimiter sniffed from the header)
read_query_table <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop("Query table not found: '", path %||% "", "'.")
  }
  header <- readLines(path, n = 1, warn = FALSE)
  if (length(header) == 0 || !nzchar(header)) stop("The query table is empty.")
  seps   <- c("\t", ",", ";")
  counts <- vapply(seps, function(s) nchar(header) - nchar(gsub(s, "", header, fixed = TRUE)), numeric(1))
  sep    <- if (max(counts) == 0) "\t" else seps[which.max(counts)]
  message("Query table delimiter detected: ", if (sep == "\t") "tab" else sep)

  tbl <- read.table(path, header = TRUE, sep = sep, quote = "", comment.char = "",
                    check.names = FALSE, colClasses = "character", strip.white = TRUE,
                    fill = TRUE, na.strings = c("NA", ""), fileEncoding = "UTF-8-BOM",
                    stringsAsFactors = FALSE)
  if (nrow(tbl) == 0) stop("The query table has a header but no data rows.")
  tbl
}

# Index of the first column whose normalised name is in `aliases` (NA when absent)
find_column <- function(tbl, aliases, label, required = TRUE) {
  hit <- which(norm_key(colnames(tbl)) %in% aliases)
  if (length(hit) == 0) {
    if (required) {
      stop("Column '", label, "' not found in the query table. Columns found: ",
           paste(colnames(tbl), collapse = ", "))
    }
    return(NA_integer_)
  }
  hit[1]
}

build_table_queries <- function(path) {
  tbl   <- read_query_table(path)
  i_tax <- find_column(tbl, c("taxon", "taxa", "taxonomicgroup", "group"), "Taxon")
  i_per <- find_column(tbl, c("period", "reportingperiod"), "Period")
  i_spe <- find_column(tbl, c("species", "subject", "speciesname", "latinname"), "Species")
  i_cty <- find_column(tbl, c("country", "countries", "ms", "memberstate"), "Country", required = FALSE)
  if (is.na(i_cty)) message("WARNING : no Country column found; all countries will be requested.")
  message("Query table: ", nrow(tbl), " row(s); other columns are ignored")

  col_or_na <- function(i) if (is.na(i)) rep(NA_character_, nrow(tbl)) else clean_cells(tbl[[i]])
  raw <- data.frame(
    row_id      = seq_len(nrow(tbl)),
    taxon_raw   = col_or_na(i_tax),
    period      = gsub("[\u2010-\u2015]", "-", col_or_na(i_per)),
    species     = col_or_na(i_spe),
    country     = col_or_na(i_cty),
    stringsAsFactors = FALSE
  )
  # The article (and Article 17 group) is derived from the taxon
  resolved    <- lapply(raw$taxon_raw, resolve_taxon)
  raw$article <- vapply(resolved, function(r) if (is.null(r)) NA_character_ else r$article, character(1))
  raw$group   <- vapply(resolved, function(r) if (is.null(r)) NA_character_ else r$group,   character(1))

  # Row-level validation: invalid rows are reported in the log and skipped
  reason <- rep(NA_character_, nrow(raw))
  reason[is.na(raw$article)] <- paste0("Unrecognised Taxon (accepted: ", ACCEPTED_TAXA, ")")
  reason[is.na(raw$taxon_raw)] <- "Missing Taxon"
  reason[is.na(raw$period)]  <- "Missing Period"
  reason[is.na(raw$species)] <- "Missing Species"
  bad <- !is.na(reason)

  skipped <- NULL
  if (any(bad)) {
    skipped <- data.frame(
      article = ifelse(is.na(raw$article[bad]), "", raw$article[bad]),
      group   = ifelse(is.na(raw$taxon_raw[bad]), "", raw$taxon_raw[bad]),
      period  = ifelse(is.na(raw$period[bad]),  "", raw$period[bad]),
      species = ifelse(is.na(raw$species[bad]), "", raw$species[bad]),
      country = ifelse(is.na(raw$country[bad]), "", raw$country[bad]), region = "",
      message = paste0("Table row ", raw$row_id[bad] + 1, ": ", reason[bad]),
      stringsAsFactors = FALSE
    )
    message("WARNING : ", sum(bad), " row(s) skipped (see the query log)")
  }
  raw <- raw[!bad, , drop = FALSE]
  if (nrow(raw) == 0) return(list(queries = raw[0, c("article", "group", "period", "species", "country")], skipped = skipped))

  # One query per unique (article, group, period, species); countries of the
  # matching rows are merged so that each page is fetched only once
  key <- paste(raw$article, raw$group, raw$period, raw$species, sep = "\r")
  keep_order <- unique(key)
  queries <- do.call(rbind, lapply(keep_order, function(k) {
    sub <- raw[key == k, , drop = FALSE]
    data.frame(article = sub$article[1], group = sub$group[1], period = sub$period[1],
               species = sub$species[1], country = merge_countries(sub$country),
               region = "", stringsAsFactors = FALSE)
  }))
  message("Table mode: ", nrow(raw), " valid row(s) -> ", nrow(queries), " unique query(ies)")
  list(queries = queries, skipped = skipped)
}

# ============================================================
# RUNNING THE QUERIES
# ============================================================

# Give a name to every unnamed column. The pre-2019 Article 12 layout has more
# cells per row than column names, which leaves trailing columns named NA.
fix_column_names <- function(df) {
  nm  <- colnames(df)
  bad <- is.na(nm) | !nzchar(nm)
  if (any(bad)) {
    nm[bad] <- paste0("Unnamed_", which(bad))
    colnames(df) <- nm
    message("WARNING : ", sum(bad), " unnamed column(s) renamed to ", paste(nm[bad], collapse = ", "))
  }
  df
}

# Insert a Period column right after Species (Article 12 tables do not carry one).
# Columns are moved by position, never by name.
insert_period <- function(df, period) {
  if ("Period" %in% colnames(df)) return(df)
  df$Period <- rep(period, nrow(df))
  n  <- ncol(df)
  sp <- match("Species", colnames(df))
  if (is.na(sp)) return(df)
  df[, c(seq_len(sp), n, setdiff(seq_len(n - 1), seq_len(sp))), drop = FALSE]
}

# Table mode: the EU-level assessment is requested for all queries or for none.
# "EU" tokens of the Country column are replaced by the include_eu setting.
apply_eu_flag <- function(country, include_eu) {
  vapply(country, function(cty) {
    tokens <- split_values(cty, "[,;|[:space:]]+")
    has_eu <- any(toupper(tokens) == "EU")
    non_eu <- tokens[toupper(tokens) != "EU"]
    if (length(non_eu) == 0) {
      if (has_eu && include_eu) return("EU")
      if (has_eu) message("WARNING : a row requests only 'EU' but the EU assessment is not enabled; all countries requested instead")
      non_eu <- "all_countries"
    }
    paste(c(non_eu, if (include_eu) "EU"), collapse = ",")
  }, character(1), USE.NAMES = FALSE)
}

# Fetch one query (one row of the query data frame) and return list(ms, eu)
run_query <- function(q) {
  q_opt <- list(
    source  = q$article,
    subject = q$species,
    group   = if (nzchar(q$group))   q$group   else NULL,
    period  = q$period,
    country = if (nzchar(q$country)) q$country else NULL,
    region  = if (nzchar(q$region))  q$region  else NULL
  )
  res <- if (q$article == "art12") run_art12(q_opt) else run_art17(q_opt)
  res$ms <- fix_column_names(res$ms)
  res$eu <- fix_column_names(res$eu)
  if (q$article == "art12") {
    res$ms <- insert_period(res$ms, q$period)
    res$eu <- insert_period(res$eu, q$period)
  }
  res
}

# One-row data frame describing the outcome of a query (for the log file)
make_log_row <- function(id, q, status, n_ms = 0, n_eu = 0, msg = "") {
  data.frame(
    Query = id, Article = q$article, Group = q$group, Period = q$period,
    Species = q$species, Country = q$country, Status = status,
    MS_rows = n_ms, EU_rows = n_eu,
    Message = gsub("[\t\r\n]+", " ", msg),
    stringsAsFactors = FALSE
  )
}

# Row-bind a list of tables; a header-only table is returned when all are empty
bind_or_empty <- function(tables, default_cols) {
  non_empty <- Filter(function(d) !is.null(d) && nrow(d) > 0, tables)
  if (length(non_empty) > 0) return(dplyr::bind_rows(non_empty))
  cols <- if (length(tables) > 0 && !is.null(tables[[1]])) colnames(tables[[1]]) else default_cols
  as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                stringsAsFactors = FALSE)
}

write_tsv <- function(df, file) {
  write.table(df, file = file.path(OUT_DIR, file), row.names = FALSE, col.names = TRUE,
              sep = "\t", quote = FALSE)
  message("File written : ", file.path(OUT_DIR, file), " (", nrow(df), " row(s))")
}

# ============================================================
# Main
# ============================================================
main <- function(opt) {
  dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

  mode <- tolower(trimws(opt$mode %||% "manual"))
  built <- if (mode == "table") {
    build_table_queries(opt$table)
  } else if (mode == "manual") {
    build_manual_queries(opt)
  } else {
    stop("Unknown --mode: '", mode, "'. Expected 'manual' or 'table'.")
  }
  queries <- built$queries
  skipped <- built$skipped
  if (mode == "table" && nrow(queries) > 0) {
    queries$country <- apply_eu_flag(queries$country, isTRUE(opt$include_eu))
  }

  store    <- list(art12_ms = list(), art12_eu = list(), art17_ms = list(), art17_eu = list())
  log_rows <- list()
  n_total  <- nrow(queries) + if (is.null(skipped)) 0 else nrow(skipped)

  for (i in seq_len(nrow(queries))) {
    q <- queries[i, , drop = FALSE]
    message("---- Query ", i, "/", nrow(queries), " : ", q$article, " | ", q$species,
            " | ", q$period, if (nzchar(q$group)) paste0(" | ", q$group) else "")

    # A failing query (misspelled species, no data, network error...) must not
    # stop the others: the error is caught and written to the log
    outcome <- tryCatch(
      list(ok = TRUE, res = run_query(q)),
      error = function(e) list(ok = FALSE, msg = conditionMessage(e))
    )

    if (outcome$ok) {
      n_ms <- nrow(outcome$res$ms)
      n_eu <- nrow(outcome$res$eu)
      store[[paste0(q$article, "_ms")]][[length(store[[paste0(q$article, "_ms")]]) + 1]] <- outcome$res$ms
      store[[paste0(q$article, "_eu")]][[length(store[[paste0(q$article, "_eu")]]) + 1]] <- outcome$res$eu
      status <- if (n_ms + n_eu == 0) "NO_DATA" else "OK"
      log_rows[[length(log_rows) + 1]] <- make_log_row(i, q, status, n_ms, n_eu)
    } else {
      message("FAILED : ", outcome$msg)
      log_rows[[length(log_rows) + 1]] <- make_log_row(i, q, "FAILED", msg = outcome$msg)
    }

    if (i < nrow(queries)) Sys.sleep(1)  # be gentle with the EIONET servers
  }

  # Rows skipped during table validation
  if (!is.null(skipped)) {
    for (j in seq_len(nrow(skipped))) {
      sk <- skipped[j, , drop = FALSE]
      log_rows[[length(log_rows) + 1]] <- make_log_row(NA, sk, "SKIPPED", msg = sk$message)
    }
  }

  log_df <- dplyr::bind_rows(log_rows)

  # All output files are always written (header-only when empty) so that Galaxy finds them
  write_tsv(bind_or_empty(store$art12_ms, c("Species", "Period", "MS")),             "MS_data.txt")
  write_tsv(bind_or_empty(store$art12_eu, c("Species", "Period", "EU")),             "EU_data.txt")
  write_tsv(bind_or_empty(store$art17_ms, c("Species", "Group", "Period", "MS")),    "ART17_data.txt")
  write_tsv(bind_or_empty(store$art17_eu, c("Species", "Group", "Period", "EU")),    "ART17_EU_data.txt")
  write_tsv(log_df, "QUERY_LOG.txt")

  n_ok <- sum(log_df$Status %in% c("OK", "NO_DATA"))
  message("Summary : ", n_total, " query(ies) -> ", sum(log_df$Status == "OK"), " OK, ",
          sum(log_df$Status == "NO_DATA"), " without data, ",
          sum(log_df$Status == "FAILED"), " failed, ", sum(log_df$Status == "SKIPPED"), " skipped")

  # Repeat the reasons of every failed or skipped query in the console
  problems <- log_df[log_df$Status %in% c("FAILED", "SKIPPED"), , drop = FALSE]
  for (k in seq_len(nrow(problems))) {
    message(problems$Status[k], " : ", problems$Species[k], " | ", problems$Period[k], " | ", problems$Message[k])
  }

  if (n_ok == 0) {
    first_msg <- log_df$Message[log_df$Message != ""][1]
    stop("No query succeeded. ", if (!is.na(first_msg)) first_msg else "")
  }
}

main(opt)

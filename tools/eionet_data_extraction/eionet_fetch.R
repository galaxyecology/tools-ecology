# Fetch species data from the EIONET Article 12 (birds) and Article 17 (other
# groups) web tools by querying the public pages and parsing the HTML tables.

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
  make_option("--source",        type="character", default="art12",
              help="Which EIONET reporting tool to query: 'art12' (birds) or 'art17' (other species groups)."),
  make_option("--subject",       type="character"),
  make_option("--group",         type="character", default=NULL,
              help="Taxonomic group (Article 17 only). Ignored for --source art12."),
  make_option("--period",        type="character"),
  make_option("--country",       type="character", default=NULL,
              help="Filter by Member State code(s), comma-separated. 'EU' selects the EU-level assessment table (Article 17: not available for 2019-2024)."),
  make_option("--region",        type="character", default=NULL,
              help="Article 17 only: filter by bio-geographical region code (ALP, ATL, BLS, BOR, CON, MED, PAN). Empty = all regions.")
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
  if (is.na(table_node)) stop("Tableau introuvable. Vérifiez le nom de l'espèce.")
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

    # Write the output files (header-only when there is nothing to report)
  if (show_ms && nrow(df_ms) > 0) {
    write.table(df_ms, file="output_files/MS_data.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)
    message("MS file written : output_files/MS_data.txt")
  } else {
    write.table(df_ms[0,], file="output_files/MS_data.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)
  }
 
  if (show_eu && !is.null(df_eu) && nrow(df_eu) > 0) {
    write.table(df_eu, file="output_files/EU_data.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)
    message("EU file written : output_files/EU_data.txt")
  } else {
    empty_eu <- data.frame(matrix(ncol=length(eu_col_names)+1, nrow=0))
    colnames(empty_eu) <- c("Species", eu_col_names)
    write.table(empty_eu, file="output_files/EU_data.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=FALSE)
  }
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
 
  write.table(if (show_ms) df else df[0, ], file = "output_files/ART17_data.txt",
              row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  message("Article 17 MS file written : output_files/ART17_data.txt")
 
  write.table(if (show_eu) df_eu else df_eu[0, ], file = "output_files/ART17_EU_data.txt",
              row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
  message("Article 17 EU file written : output_files/ART17_EU_data.txt")
}
 
# ============================================================
# Main
# ============================================================
if (opt$source == "art12") {
  run_art12(opt)
} else if (opt$source == "art17") {
  run_art17(opt)
} else {
  stop("Unknown --source: '", opt$source, "'. Expected 'art12' or 'art17'.")
}

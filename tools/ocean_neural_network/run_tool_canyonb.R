# ██████╗ ███████╗███████╗ ██████╗██████╗ ██╗██████╗ ████████╗██╗ ██████╗ ███╗   ██╗#nolint
# ██╔══██╗██╔════╝██╔════╝██╔════╝██╔══██╗██║██╔══██╗╚══██╔══╝██║██╔═══██╗████╗  ██║#nolint
# ██║  ██║█████╗  ███████╗██║     ██████╔╝██║██████╔╝   ██║   ██║██║   ██║██╔██╗ ██║#nolint
# ██║  ██║██╔══╝  ╚════██║██║     ██╔══██╗██║██╔═══╝    ██║   ██║██║   ██║██║╚██╗██║#nolint
# ██████╔╝███████╗███████║╚██████╗██║  ██║██║██║        ██║   ██║╚██████╔╝██║ ╚████║#nolint
# ╚═════╝ ╚══════╝╚══════╝ ╚═════╝╚═╝  ╚═╝╚═╝╚═╝        ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝#nolint

# this R script run canyon B on multi profile!
# launch :
# Rscript run_tool_galaxy.R $1 $2 $3 $4 $5 $6 $7 $8 $9
# where arguments are
#   $1 (mandatory) path of the txt or csv file to be processed
#   $2 to 5 (optional 1) name of the column pressure, temperature, salinity and oxygen
#   $6 to 9 (optional 2) name of the column error for pres, temp, sal and oxy
#
# input :
#   .csv built by the user with :
#       separator ";"
#       mandatory column name :
#             date/latitude/longitude/pressure/temperature/salinity/oxygen
#       optional column name for error:
#             epre/etemp/epsal/edoxy
#       only arg 1
#
# or
#
#   .txt built by the tools  QCV-ingestor-harmonizer then QCV-odv-tool
#                            (from easy)
#       only arg 1
#
# or
#   .txt extract from the (web)ODV software
#                            (from ODV)
#   need args 1 to 5 at list (optional, args 6 to 9)
#
# Important :
#   canyon B reads :
#       latitude [-90 to 90°N]
#       longitude [-180 to 180°E]
#       pressure in dbar
#       tempereture in °C
#       salinity in psu
#       oxygen in µmol/kg
#
# ouput :
#   input csv or txt enriched with canyonb output including their errors:
#     for more information read
#        https://github.com/HCBScienceProducts/CANYON-B/blob/master/CANYONB.R
#       NO3 (nitrate)
#       PO4 (phosphate)
#       AT (total alkalinity)
#       CT (total carbon)
#       pH (ph)
#       pCO2 (pCO2)
#       SiOH4 (silicate)
#   output is visible with odv
#
#  + tool_canyonb_galaxy.log save where the tool runs
#
# Remarks :
#   (1) for argo floats, read synthetic profile with qcv-ingestor-harmonizer
#   then qcv-odv-tool to have temperature, salinity and oxygen alined at the
#   same level
#   (2) the tool merge complementary lines only, it does not interpolate TS,
#     on BGC level or vice versa
#     exple : L1 : P = 4.5  T = 25  S = 35.4  O2 = NA
#             L2 : P = 4.5  T = NA  S = 35.4  O2 = 345.6
#    merged line : P = 4.5  T = NA  S = 35.4  O2 = 345.6
#   (3) Regarding .txt built by the tools  QCV-ingestor-harmonizer then
#    QCV-odv-tool, if PRES_DM or RTADJUSTED are empty for BGC variables
#    (like in argo files), then tool take PRES_RAW for all variables

# ███████╗██╗   ██╗███╗   ██╗ ██████╗████████╗██╗ ██████╗ ███╗   ██╗███████╗#nolint
# ██╔════╝██║   ██║████╗  ██║██╔════╝╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝#nolint
# █████╗  ██║   ██║██╔██╗ ██║██║        ██║   ██║██║   ██║██╔██╗ ██║███████╗#nolint
# ██╔══╝  ██║   ██║██║╚██╗██║██║        ██║   ██║██║   ██║██║╚██╗██║╚════██║#nolint
# ██║     ╚██████╔╝██║ ╚████║╚██████╗   ██║   ██║╚██████╔╝██║ ╚████║███████║#nolint
# ╚═╝      ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝#nolint
#
initialisation_log_file <- function() {

  # directory
  in_log <- getwd()

  # file name
  datetime <- format(Sys.time(), "%Y-%m-%dT%H%M")
  log_file_name <- paste0(paste("tool_canyonb_galaxy",
                                datetime,
                                sep = "_"),
                          ".log")
  log_file <- paste(in_log, log_file_name, sep = "/")

  if (file.exists(log_file)) {
    file.remove(log_file)
  }

  # creation
  logger::log_appender(logger::appender_file(log_file,
                                             max_lines = 5000,
                                             max_files = 5L))
}

update_log_file <- function(type, msge, stop = FALSE) {

  # update lofg file according to the message type
  if (type == "info") {
    logger::log_info(msge)
  }else if (type == "warning") {
    logger::log_warn(msge)
  }else if (type == "error") {
    logger::log_warn(msge)
  }else if (type == "done") {
    logger::log_info(msge)
    logger::log_info("END of the process")
  }
  message(paste(toupper(type), msge, sep = " "))

  # stop process
  if (stop == TRUE) {
    logger::log_info("END of the process")
    stop()
  }

}

exist_in_list <- function(value, lst) {
  return(value %in% lst)
}

convert_name_from_semantics <- function(elem, matrix) {

  # convert names to canyonB semantics
  if (!(elem %in% names(matrix))) {
    # Use lapply to check if the value is in any list
    result <- lapply(matrix, exist_in_list, value = elem)
    # Get the key where the value is found
    key <- names(matrix)[which(unlist(result))]
    return(key)
  }else {
    return(elem)
  }
}

datetime_standardisation <- function(datetime,
                                     in_mode = NULL,
                                     out_mode = NULL) {

  if ((!is.null(in_mode) && in_mode == "odv")
      || (!is.null(out_mode) && out_mode == "odv")) {
    date_format <- "%Y-%m-%dT%H:%M:%OS3"
    if (any(grepl("T", datetime))) {
      datetime <- gsub("Z",
                       "",
                       gsub("T", " ", datetime))
      date_format <- "%Y-%m-%d %H:%M:%S"
    }
  }else if ((!is.null(in_mode) && in_mode == "canyonb")
            || (!is.null(out_mode) && out_mode == "canyonb")) {
    date_format <- "%Y-%m-%d %H:%M:%S"
  }

  if (!is.null(in_mode)) {
    out_datetime <- as.POSIXct(datetime, format = date_format)
  }else if (!is.null(out_mode)) {
    out_datetime <- format(datetime, date_format)
  }

  return(out_datetime)

}

collapse_dupli_by_column <- function(x) {

  # collapse if and only if rows are complementary
  x <- as.data.frame(x)
  # default output
  collapse <- NULL

  # test complementary
  to_be_collapse <- FALSE
  test1 <- colSums(!is.na(x))
  if (any(test1 > 1) == FALSE) {
    to_be_collapse <- TRUE
  }
  if (to_be_collapse == FALSE) {
    test2 <- unlist(lapply(seq(1, dim(x)[2], 1),
                           function(y) dim(unique(x[, y]))[1]))
    if (any(test2[test1 > 1] > 1) == FALSE) {
      to_be_collapse <- TRUE
    }
  }

  if (to_be_collapse == TRUE) {
    # it is complementary
    reduce_tmp <- lapply(seq(1, dim(x)[2], 1),
                         function(xy) dplyr::coalesce(!!! x[, xy]))
    reduce_tmp <- unlist(reduce_tmp)
    reduce <- data.frame(matrix(NA, 1, dim(x)[2]))
    reduce[!(test1 == 0)] <- reduce_tmp[!(test1 == 0)]
    names(reduce) <- names(x)
    collapse <- reduce
  }
  return(collapse)
}

arrange_duplicate <- function(df, list_common, list_sort = NULL) {

  library(tidyr)
  library(dplyr)
  library(stringr)

  # need list of column help to group df and identify duplicate
  # 1. collapse real duplicate
  # 2. remove line with other column all na

  # list of column to be collapsed if complementary
  list_all <- names(df)
  line_common <- lapply(list_common, function(x) which(list_all == x))
  list_others <- list_all[-unlist(line_common)]

  # arrange df to reduce the number of action (action 2 by row)
  df <- filter(df, rowSums(is.na(df[, list_others])) != ncol(df[, list_others]))

  # define duplicate in df (action 1)
  dupli <- df %>%
    group_by(across(all_of(list_common))) %>%
    arrange(across(all_of(list_common))) %>%
    filter(n() > 1) %>%
    mutate(n_line = row_number()) %>%
    ungroup()

  # working on potential duplicate if exist
  if (dim(dupli)[1] > 0) {
    # remove potential duplicate
    without_dupli <- df %>%
      group_by(across(all_of(list_common))) %>%
      filter(!n() > 1) %>%
      ungroup()

    # how many duplicate and where are they?
    pos1 <- which(dupli$n_line == 1)
    rep <- pos1[seq(2, length(pos1), 1)] - pos1[seq(1, length(pos1) - 1, 1)]
    rep <- c(rep, length(dupli$n_line) - pos1[length(pos1)] + 1)
    n_dupli <- lapply(seq(1, length(rep), 1),
                      function(x) rep(x, rep[x]))
    dupli$n_dupli <- unlist(n_dupli)

    # collapse duplicate when they are complementary
    reduce_dupli <- lapply(seq(1, max(dupli$n_dupli), 1),
                           function(group) {
                             lignes <- which(dupli$n_dupli == group)
                             x <- dupli[lignes, list_others]
                             collapse <- collapse_dupli_by_column(x)
                             if (!is.null(collapse)) {
                               table <- cbind.data.frame(dupli[lignes[1],
                                                               list_common],
                                                         collapse)
                             }else {
                               table <- dupli[lignes, c(list_common,
                                                        list_others)]
                             }
                             return(table)
                           })
    reduce_dupli <- do.call(rbind.data.frame, reduce_dupli)

    # combine with table wihtout duplicate
    df <- rbind.data.frame(without_dupli, reduce_dupli)
  }

  return(df)
}

# ███╗   ███╗ █████╗ ██╗███╗   ██╗
# ████╗ ████║██╔══██╗██║████╗  ██║
# ██╔████╔██║███████║██║██╔██╗ ██║
# ██║╚██╔╝██║██╔══██║██║██║╚██╗██║
# ██║ ╚═╝ ██║██║  ██║██║██║ ╚████║
# ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝
#

# full path of csv file name -----------------------------------
##Load arguments

args_full <- commandArgs(trailingOnly = TRUE)

# Filter out empty arguments
cmd_args <- args_full[nzchar(args_full)]  # Removes elements that are empty ("") or just spaces

# Replace __ob__ with [ and __cb__ with ]
args <- gsub("__ob__", "[", cmd_args)
args <- gsub("__cb__", "]", args)
if (length(args) < 1) {
  stop("This tool needs at least 1 argument")
}else {
  csv_file <- args_full[1]
}

# compatibility matrix -----------------------------------------
odv_date_name <- "yyyy-mm-ddThh:mm:ss.sss"
mapping_date <- c("date", "Date", odv_date_name)
mapping_lat <- c("latitude",
                 "latitude [degrees_north]",
                 "Latitude [degrees_north]")
mapping_lon <- c("longitude",
                 "longitude [degrees_east]",
                 "Longitude [degrees_east]")

error_args <- paste0("This tool needs at least 5 mapped ",
                     "variables if the mapping is required")
if (length(args) > 1 ) {
  mapping_pres <- args[2]
  if (mapping_pres == "") {
    stop(error_args)
  }
  mapping_temp <- args[3]
  if (mapping_temp == "") {
    stop(error_args)
  }
  mapping_psal <- args[4]
  if (mapping_psal == "") {
    stop(error_args)
  }
  mapping_doxy <- args[5]
  if (mapping_doxy == "") {
    stop(error_args)
  }
}else if (length(args) == 1) {
  mapping_pres <- c("pressure",
                    "pressure_raw [decibar]",
                    "pressure_dmadjusted [decibar]",
                    "pressure_rtadjusted [decibar]")
  mapping_temp <- c("temperature",
                    "temperature_raw [degree_celsius]",
                    "temperature_dmadjusted [degree_celsius]",
                    "temperature_rtadjusted [degree_celsius]")
  mapping_psal <- c("salinity",
                    "salinity_raw [psu]",
                    "salinity_dmadjusted [psu]",
                    "salinity_rtadjusted [psu]")
  mapping_doxy <- c("oxygen",
                    "oxygen_raw [micromole/kg]",
                    "oxygen_dmadjusted [micromole/kg]",
                    "oxygen_rtadjusted [micromole/kg]")
}else {
  stop(error_args)
}

if (length(args) >= 6 && !(args[6] == "")) {
  mapping_epres <- args[6]
}else {
  mapping_epres <- c("epres")
}
if (length(args) >= 7 && !(args[7] == "")) {
  mapping_etemp <- args[7]
}else {
  mapping_etemp <- c("etemp")
}
if (length(args) >= 8 && !(args[8] == "")) {
  mapping_epsal <- args[8]
}else {
  mapping_epsal <- c("epsal")
}
if (length(args) >= 9 && !(args[9] == "")) {
  mapping_edoxy <- args[9]
}else {
  mapping_edoxy <- c("edoxy")
}

semantics_canyonB <- list(
  date = mapping_date,
  lat = mapping_lat,
  lon = mapping_lon,
  pres = mapping_pres,
  temp = mapping_temp,
  psal = mapping_psal,
  doxy = mapping_doxy,
  epres = mapping_epres,
  etemp = mapping_etemp,
  epsal = mapping_epsal,
  edoxy = mapping_edoxy
)
semantics_mandatory <- c("date", "lat", "lon", "pres", "temp", "psal", "doxy")

# default values -----------------------------------------------
from_odv <- 0
from_easy <- 0

# log file -----------------------------------------------------
initialisation_log_file()
update_log_file("info", "start process")

# check input file -----------------------------------------------
update_log_file("info", "check input file")
# check file extension
ext <- tools::file_ext(csv_file)
if (!(ext == "txt" || ext == "csv")) {
  msge <- "the extension should be .txt or .csv"
  update_log_file("error", msge, stop = TRUE)
}

# check format
if (ext == "csv") {
  # user file
  table <- read.table(csv_file, sep = ";", header = TRUE)
  if (dim(table)[2] < 2) {
    msge <- "column number < 2, csv separator might be wrong"
    update_log_file("error", msge, stop = TRUE)
  } else if (dim(table) [2] < 7) {
    msge <- "column number > 2 & < 7, missing column"
    update_log_file("error", msge, stop = TRUE)
  }

}else if (ext == "txt") {
  # odv collection
  count <- 1
  is_continue <- TRUE
  while (is_continue == TRUE) {
    first_lines <- read.csv(file = csv_file,
                            header = FALSE,
                            sep = "\n", nrows = count)
    table_tmp <- first_lines[count, ]
    if (length(grep("^//", table_tmp)) == 0) {
      is_continue <- FALSE
    }else {
      count <- count + 1
    }
  }

  if (count == 1) {
    msge <- "txt file not from the odv collection generator tool"
    update_log_file("error", msge, stop = TRUE)
  }

  odv_pattern1 <- "<Software>FAIR-EASE-CALVAL-PLATFORM"
  odv_pattern2 <- "<Software>Web Ocean Data View"
  odv_pattern3 <- "<Software>Ocean Data View"
  if (!any(grepl(odv_pattern1, first_lines)) == TRUE
      && !any(grepl(odv_pattern2, first_lines)) == TRUE
      && !any(grepl(odv_pattern3, first_lines)) == TRUE) {
    msge <- paste0("txt file do not come from the odv ",
                   "collection generator tool or (web)ODV software")
    update_log_file("error", msge, stop = TRUE)
  }

  # read table
  header <- first_lines[dim(first_lines)[1], ]
  if (any(grepl(odv_pattern1, first_lines)) == TRUE) {
    table <- read.table(csv_file,
                        sep = ";",
                        header = FALSE,
                        skip = count)
    header <- unlist(strsplit(header, split = ";"))
    from_easy <- 1
  }else {
    table <- read.csv(csv_file,
                      sep = "\t",
                      header = FALSE,
                      comment.char = "/")
    table <- table[-1, ]
    header <- unlist(strsplit(header, split = "\t"))
    from_odv <- 1
  }

  names(table) <- header
  first_lines <- first_lines[-dim(first_lines)[1], ]
}

# check input variables
header <- names(table)
canb_obj <- lapply(header, convert_name_from_semantics, semantics_canyonB)
for (var in semantics_mandatory){
  if (!any(unlist(canb_obj) == var)) {
    msge <- "at least one of the mandatory column name is missing"
    update_log_file("error", msge, stop = TRUE)
  }
}

# odv software specificity 
if (from_odv == 1) {
  # metadata available for the first line / profile
  # where is useful metadata
  col_dat <- unlist(lapply(mapping_date, function(x) which(header == x)))
  col_lon <- unlist(lapply(mapping_lon, function(x) which(header == x)))
  col_lat <- unlist(lapply(mapping_lat, function(x) which(header == x)))

  # lat and lon should be numeric variables
  table[[col_lat]] <- as.numeric(table[[col_lat]])
  table[[col_lon]] <- as.numeric(table[[col_lon]])

  # where is latitude/longitude information
  line_lat <- which(is.finite(table[[col_lat]]) == TRUE)
  line_lat <- c(line_lat, dim(table)[1] + 1)

  # repeat the lat/lon/data for each line of the profile
  for (i in seq(1, length(line_lat) - 1, 1)) {
    line_tbr <- seq(line_lat[i], line_lat[i + 1] - 1, 1)
    table[line_tbr, col_lat] <- rep(table[line_lat[i], col_lat],
                                    line_lat[i + 1] - line_lat[i])
    table[line_tbr, col_lon] <- rep(table[line_lat[i], col_lon],
                                    line_lat[i + 1] - line_lat[i])
    table[line_tbr, col_dat] <- rep(table[line_lat[i], col_dat],
                                    line_lat[i + 1] - line_lat[i])
  }

  # transform character into numeric
  col_temp <- unlist(lapply(mapping_temp, function(x) which(header == x)))
  col_psal <- unlist(lapply(mapping_psal, function(x) which(header == x)))
  col_doxy <- unlist(lapply(mapping_doxy, function(x) which(header == x)))
  col_pres <- unlist(lapply(mapping_pres, function(x) which(header == x)))
  for (i in c(col_temp, col_psal, col_doxy, col_pres)) {
    table[[i]] <- as.numeric(table[[i]])
  }
}

# define date format  ---------------------------------------------
if (ext == "txt") {
  date_name <- odv_date_name
  in_format <- "odv"
}else if (ext == "csv") {
  date_name <- "date"
  in_format <- "canyonb"
}
table[[date_name]] <- datetime_standardisation(table[[date_name]],
                                               in_mode = in_format)

# prepare output table
table_out <- table

# loop for running CANYON D--------------------------------------
if (ext == "txt" && from_easy == 1) {
  nb_run <- 3
}else if (ext == "csv" || (ext == "txt" && from_easy == 0)) {
  nb_run <- 1
}

# check dm and rtadjusted pressure for odv collection - empty for BGC argo
if (ext == "txt" && from_easy == 1) {
  sufx <- c("dmadjusted", "rtadjusted")
  for (suf in sufx) {
    oxyname <- semantics_canyonB$doxy[grep(suf,
                                           semantics_canyonB$doxy)]
    prsname <- semantics_canyonB$pres[grep(suf,
                                           semantics_canyonB$pres)]
    rawpres <- semantics_canyonB$pres[grep("raw",
                                           semantics_canyonB$pres)]
    if (any(oxyname == header)
        && any(prsname == header)
        && !any(is.finite(prsname[is.finite(table[[oxyname]]) == TRUE]))) {

      table[prsname] <- table[rawpres]
      update_log_file("info", paste0(prsname,
                                     " is empty for bgc variables, ",
                                     "tool replace by ",
                                     rawpres))
    }
  }
}

count <- 0
count1 <- 0
for (run in seq(1, nb_run, 1)) {
  update_log_file("info", paste0("run nb : ", run))
  update_log_file("info", "reduce input table to essential element")

  # txt (easy odv collection) specificite
  if (ext == "txt" && from_easy == 1 && run == 1) {
    sufix <- "raw"
  }else if (ext == "txt" && from_easy == 1 && run == 2) {
    sufix <- "rtadjusted"
  }else if (ext == "txt" && from_easy == 1 && run == 3) {
    sufix <- "dmadjusted"
  }

  # indexes of interesting column
  index <- NULL
  for (i in seq(1, length(canb_obj), 1)) {
    if (length(canb_obj[[i]]) > 0
        && (ext == "csv"
            || (ext == "txt" && from_easy == 0))) {
      index <- c(index, i)
    }else if (length(canb_obj[[i]]) > 0 && ext == "txt" && from_easy == 1) {
      if (!grepl("_", header[[i]])
          || grepl("degrees", header[[i]])
          || grepl(sufix, header[[i]])) {
        index <- c(index, i)
      }
    }
  }
  df <- table[, index]
  # default condition
  is_continu <- 1

  # check if one column is empty
  new_table <- df[, apply(df, 2, function(x) !all(is.na(x)))]
  if (length(index) > length(names(new_table))) {
    message(names(new_table))
    msge <- "at least one of the mandatory column name is missing"
    update_log_file("warning", msge)
    is_continu <- 0
  }

  # list of column for searching duplicate
  if (is_continu == 1) {
    list_common <- NULL
    index_oxy_col <- NULL
    for (i in index) {
      name <- canb_obj[[i]]
      if (name == "date"
          || name == "lat"
          || name == "lon"
          || name == "pres") {
        list_common <- c(list_common, header[i])
      }else if (name == "doxy") {
        index_oxy_col <- i
      }
    }

    # where is oxygen
    index_oxy_row <- seq(1, dim(table)[1], 1)
    is_noxy <- is.na(table[, index_oxy_col])
    index_oxy_row[is_noxy == TRUE] <- NA
    new_table$index_oxy <- index_oxy_row

    # arrange dataframe for collapsing complementary duplicate
    new_table <- arrange_duplicate(new_table, list_common)

    # identification non na
    is_nok <- lapply(seq(1, dim(new_table)[1], 1),
                     function(x) any(is.na(new_table[x, ])))
    is_nok <- unlist(is_nok)


    if (!any(is_nok == FALSE)) {
      count <- count + 1
      msge <- "at least one essential element is missing by row"
      if (count == nb_run) {
        update_log_file("error", msge, stop = TRUE)
      } else {
        update_log_file("warning", msge)
        is_continu <- 0
      }
    }
  }
  # creation of canb_list
  if (is_continu == 1) {
    update_log_file("info", "define canyon b object")
    canb_list <- list()
    for (i in index) {
      name <- canb_obj[[i]]
      canb_list[[name]] <- new_table[[header[[i]]]][is_nok == FALSE]
      if (name == "date" && ext == "txt") {
        canb_list$date <- datetime_standardisation(canb_list$date,
                                                   out_mode = "canyonb")
      }
    }

    # double check obj for txt (odv collection)
    for (var in semantics_mandatory) {
      if (length(canb_list[[var]]) == 0) {
        count <- count + 1
        msge <- paste(sufix,
                      " is not used because at least ",
                      "one of the mandatory column name is missing")
        if (count == nb_run) {
          update_log_file("error", msge, stop = TRUE)
        }else {
          update_log_file("warning", msge)
          is_continu <- 0
        }
      }
    }
  }

  # run canyonb
  if (is_continu == 1) {
    update_log_file("info", "run canyonB")
    res <- do.call(canyonb::CANYONB, canb_list)

    # transformer la liste en data.frame
    table_sus_tmp <- NULL
    header_sus <- NULL
    for (name in names(res)) {
      sus <- matrix(res[[name]], length(res$AT), 1)
      table_sus_tmp <- cbind(table_sus_tmp, sus)
      if (ext == "txt" && from_easy == 1) {
        name <- paste(name, "fromcanyon", sufix, sep = "_")
      }else {
        name <- paste(name, "fromcanyon", sep = "_")
      }
      header_sus <- c(header_sus, name)
    }

    # resize table with additional data
    table_sus <- data.frame(matrix(NA, dim(table)[1], length(header_sus)))
    ligne_oxy <- new_table$index_oxy[is_nok == FALSE]
    table_sus[ligne_oxy, ] <- table_sus_tmp
    names(table_sus) <- header_sus

    # output file
    table_out <- cbind(table_out, table_sus)
  }
}

# prepare date for odv
if (ext == "csv") {
  datetime <- table_out$date
  table_out$date <- datetime_standardisation(datetime,
                                             out_mode = "odv")
  header <- names(table_out)
  header[header == "date"] <- odv_date_name
  names(table_out) <- header
}

# write output file
file_out <- gsub(paste0(".", ext), paste0("_canyonb.", ext), csv_file)
file.create(file_out)
if (ext == "txt") {
  write.table(first_lines,
              file_out,
              append = TRUE,
              col.names = FALSE,
              row.names = FALSE,
              quote = FALSE)
}
write.table(table_out, file_out, append = TRUE, row.names = FALSE, col.names = TRUE, sep = ";")

# final log update
msge <- paste0(file_out, " is available")
update_log_file("done", msge)

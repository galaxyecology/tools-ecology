##05/05/2025
##Jean Le Cras
### Enrich dataset with data from WoRMS

#load libraries
library(tidyverse)
library(worrms)
library(fastDummies)

### parameters
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
    stop("This tool needs at least one argument")
}

scientificName_name <- args[3]
occurrence <- read.csv(args[1], header=T, sep="\t") %>% 
  arrange(.[[scientificName_name]])
measurement_types <- unlist(str_split(args[2], ","))
include_inherited <- ifelse(args[4]=="true", T, F)
pivot_wider <- ifelse(args[5]=="true", T, F)
exclude_NA <- ifelse(args[6]=="true", T, F)

regex_find <- "^([A-Z][a-z]+)\\s+([a-z-]+).*"
regex_replace <- "\\1 \\2"


# function to extract the measurement values from the attributes data tibble
extract_traits_values <- function(traits_data) {
  result <- setNames(rep(NA, length(measurement_types)), measurement_types)
  
  if (is.null(traits_data) || nrow(traits_data) == 0) {
    return(result)
  }
  
  traits_filtered <- traits_data %>%
    filter(measurementType %in% measurement_types) %>%
    filter(!is.na(measurementValue))
  
  if (nrow(traits_filtered) == 0) {
    return(result)
  }
  
  for (i in 1:nrow(traits_filtered)) {
    result[traits_filtered$measurementType[i]] <- traits_filtered$measurementValue[i]
  }
  return(result)
}

# function to call the call the WoRMS API and get the measurement values
get_life_history_traits <- function(scientific_name) {
  clean_scientific_name <- gsub(regex_find, regex_replace, scientific_name)

  if (clean_scientific_name %in% names(cache)) { 
    return(cache[[clean_scientific_name]])  
  }
  
  worms_id <- tryCatch(
    wm_name2id(name = clean_scientific_name),
    error = function(e) NA
  )
  
  if (is.na(worms_id) || length(worms_id) == 0) {
    cache[[clean_scientific_name]] <<- NULL
    return(NULL)
  }
  
  data_attr <- tryCatch(
    wm_attr_data(worms_id, include_inherited=include_inherited),
    error = function(e) NULL
  )
  
  if (is.null(data_attr)) {
    cache[[clean_scientific_name]] <<- NULL
    return(NULL)
  }
  
  traits <- extract_traits_values(data_attr)
  cache[[clean_scientific_name]] <<- traits
  return(traits)
}

# a cache to limit API calls
cache <- list()

# add a columns conataining the lists of values of the measurments requested
trait_data <- occurrence %>%
  mutate(life_history_traits = map(.data[[scientificName_name]], ~ get_life_history_traits(.x)))

# convert the column of lists to multiple columns of unique values
trait_data <- trait_data %>%
  unnest_wider(life_history_traits)

# make sur each measurement type has a column
for (col in measurement_types) {
  if (!(col %in% names(trait_data))) {
    trait_data[[col]] <- NA
  }
}

# list of quantitativ measurements
numeric_cols <- c()

# try to convert columns to numeric and remember them
trait_data <- trait_data %>%
  mutate(across(all_of(measurement_types), ~ {
    numeric_col <- suppressWarnings(as.numeric(.))
    if (all(is.na(.) == is.na(numeric_col))) {
      numeric_cols <<- c(numeric_cols, cur_column())
      numeric_col
    } else {
      .
    }
  }))

# filter NA but only in the added columns
if (exclude_NA) {
  trait_data <- trait_data[complete.cases(trait_data[, measurement_types]),]
}

# determine what are the qualitativ columns to be one hot encoded
factor_cols <- setdiff(measurement_types, numeric_cols)

# one hot encode quantitativ columns
if (pivot_wider & length(factor_cols) > 0) {
  trait_data <- dummy_cols(trait_data, select_columns = factor_cols, remove_selected_columns=T, ignore_na=T)
}

# write the enriched dataset as tabular
write.table(trait_data, "enriched_data.tabular", sep="\t", row.names = FALSE)
# Load packages
library(adegenet)
library(poppr)
library(hierfstat)
 
# Parse named arguments  --flag value
args <- commandArgs(trailingOnly = TRUE)
 
get_arg <- function(args, flag) {
  idx <- which(args == flag)
  if (length(idx) == 0 || idx == length(args)) stop("Missing argument: ", flag)
  return(args[idx + 1])
}
 
input_path     <- get_arg(args, "--input")
input_name     <- get_arg(args, "--name")
filter_order   <- get_arg(args, "--order")
ind_md         <- as.numeric(get_arg(args, "--ind-md"))
ssr_md         <- as.numeric(get_arg(args, "--ssr-md"))
low_quar       <- as.numeric(get_arg(args, "--low-quar"))
high_quar      <- as.numeric(get_arg(args, "--high-quar"))
multi          <- as.numeric(get_arg(args, "--multi"))
 
# Clean input_name (handle Galaxy element_identifier with parentheses)
if (grepl("\\([^)]+\\)\\s*$", input_name)) {
  input_name <- sub(".*\\(([^)]+)\\)\\s*$", "\\1", input_name)
} else {
  input_name <- sub("\\.[^.]+$", "", input_name)
}
 
# Parse filter order from comma-separated string (e.g. "A,B,C")
# Map letters to internal filter names, preserving user-defined order
filter_map <- list(
  A = "ind_md_ssr",
  B = "ssr_md_filtering",
  C = "null_allele"
)
 
if (filter_order == "") {
  stop("No filter selected. Please select at least one filter to apply.")
}

selected_letters <- trimws(strsplit(filter_order, ",")[[1]])
filters <- unlist(filter_map[selected_letters], use.names = FALSE)
filters <- filters[!is.na(filters)]
 
message("Filters to apply (in order): ", paste(filters, collapse = " -> "))
 
# Load data
data <- read.csv(input_path, head = TRUE, sep = "\t")

# Genind conversion
data_genind <- data[, !colnames(data) %in% c("Ind", "Pop")]
genind_file <- df2genind(
  data_genind,
  sep = "/",
  NA.char = "0/0",
  ploidy = 2,
  pop = data$Pop,
  ind.names = data$Ind
)

#####################################################################################
# Function : ind_md_filtering
# Description : Remove individuals with more missing data than the defined threshold
######################################################################################

ind_md_filtering <- function(genind_file, ind_md){
  genind_ind_md <- missingno(genind_file, type = "geno", cutoff = ind_md)

  return(genind_ind_md)
}

####################################################################################
# Function : ssr_md_filtering
# Description : Remove SSR with more missing data than the defined threshold
####################################################################################

ssr_md_filtering <- function(genind_file, ssr_md){
  genind_ssr_md <- missingno(genind_file, type = "loci", cutoff = ssr_md)

  return(genind_ssr_md)
}

######################################################################################################################
# Function : null_allele
# Description : Detection and removal of null or paralogue alleles using the distribution of Fis. 
#               Outliers are considered to be probable null alleles and are removed. 
#               For the detection of outliers, the threshold is estimate using the IQR (interquartile range) method.
#####################################################################################################################

null_allele <- function(genind_file, low_quar, high_quar, multi) {
  # Fis calculation
  basic_stats <- basic.stats(genind_file)

  # Extraction Fis per loci
  fis_per_locus <- basic_stats$perloc$Fis

  # Boxplot visualisation 
  png_name=paste0("figure_output/", input_name, "_fis_null_allele.png")
  png(png_name, width = 800, height = 600)
  boxplot(fis_per_locus,
          main = paste0("Fis distribution per locus: ", input_name),
          ylab = "Fis",
          col  = "steelblue")
  dev.off()

  # Statistical identification of outliers (IQR method)
  quartiles <- quantile(fis_per_locus, probs=c(low_quar, high_quar), na.rm = TRUE)
  IQR <- diff(quartiles)

  low_threshold <- quartiles[1] - multi * IQR
  high_threshold <- quartiles[2] + multi * IQR

  # Identify the names of outlier loci
  outliers_loci <- names(fis_per_locus[fis_per_locus < low_threshold | fis_per_locus > high_threshold])
  print(paste("Locus outliers identified :", paste(outliers_loci, collapse=", ")))

  # Only keep the loci that are NOT in the list of outliers.
  locus_to_keep <- setdiff(locNames(genind_file), outliers_loci)
  genind_without_outliers <- genind_file[loc = locus_to_keep]

  return(genind_without_outliers)
}

#########################
# Main execution
#########################

# Suffix accumulation for output filename
filter_suffixes <- list(
  ind_md_ssr       = "_mdInd",
  ssr_md_filtering = "_mdloci",
  null_allele      = "_null"
)
output_suffix <- ""

# Summary table
summary_rows <- list()
 
# Filter labels and parameter strings
filter_labels <- list(
  ind_md_ssr       = "Individuals missing data",
  ssr_md_filtering = "Loci missing data",
  null_allele      = "Null alleles"
)
 
filter_params <- list(
  ind_md_ssr       = paste0("MAX_MISSING_IND=", ind_md),
  ssr_md_filtering = paste0("MAX_MISSING_LOCI=", ssr_md),
  null_allele      = paste0("low_quar=", low_quar, ", high_quar=", high_quar, ", IQR_multi=", multi)
)

# Initial row (before any filtering)
summary_rows[[1]] <- data.frame(
  File   = input_name,
  Step   = 0,
  Filter = "Raw data",
  Params = "-",
  n_ind  = nInd(genind_file),
  n_loci = nLoc(genind_file),
  stringsAsFactors = FALSE
)

for (i in seq_along(filters)) {
  step <- filters[i]
 
  if (step == "ind_md_ssr") {
    message("==> Filter: ind_md_ssr (threshold = ", ind_md, ")")
    genind_file <- ind_md_filtering(genind_file, ind_md)
 
  } else if (step == "ssr_md_filtering") {
    message("==> Filter: ssr_md_filtering (threshold = ", ssr_md, ")")
    genind_file <- ssr_md_filtering(genind_file, ssr_md)
 
  } else if (step == "null_allele") {
    message("==> Filter: null_allele (low_quar=", low_quar,
            ", high_quar=", high_quar, ", multi=", multi, ")")
    genind_file <- null_allele(genind_file, low_quar, high_quar, multi)

  } else {
    warning("Unknown filter: ", step, " — skipped.")
  }

  output_suffix <- paste0(output_suffix, filter_suffixes[[step]])

  if (nInd(genind_file) == 0) {
    stop("No individuals remaining after filter '", step, "'. ",
         "The threshold is too strict: all individuals were removed. Please use a higher value.")
  }
  if (nLoc(genind_file) == 0) {
    stop("No loci remaining after filter '", step, "'. ",
         "The threshold is too strict: all loci were removed. Please use a higher value.")
  }

  summary_rows[[i + 1]] <- data.frame(
  File   = input_name,
  Step   = i,
  Filter = filter_labels[[step]],
  Params = filter_params[[step]],
  n_ind  = nInd(genind_file),
  n_loci = nLoc(genind_file),
  stringsAsFactors = FALSE
  )
}

# Write filtering summary table
summary_table <- do.call(rbind, summary_rows)
output_summary <- paste0("summary/filtering_summary.txt")
write.table(summary_table,
            file      = output_summary,
            sep       = "\t",
            row.names = FALSE,
            quote     = FALSE)
 
# Write filtered dataset (same format as input)
df_out <- genind2df(genind_file, sep = "/", usepop = FALSE)
df_out <- data.frame(
  Ind = rownames(df_out),
  Pop = pop(genind_file),
  df_out,
  row.names = NULL
)
 
output_file <- paste0("filter_output/", input_name, output_suffix, ".txt")
write.table(df_out,
            file      = output_file,
            sep       = "\t",
            row.names = FALSE,
            quote     = FALSE)
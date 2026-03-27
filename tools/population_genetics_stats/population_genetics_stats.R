#Load packages
library(dplyr)
library(ade4)
library(adegenet)
library(vcfR)
library(hierfstat)
library(mmod)
library(ggplot2)
library(tibble)
library(viridis)
library(tidyr)

#Load arguments
args <- commandArgs(trailingOnly = TRUE)

marker_type <- args[1]
input_path  <- args[2]
input_name  <- args[3]

# Load indpop only for SNP
if (marker_type == "SNP") {
  indpop_path <- args[4]
  indpop <- read.table(indpop_path, header = FALSE, sep = "\t")
  colnames(indpop) <- c("Ind", "Pop")
}

# Statistics selection flags
stat_start_index <- if (marker_type == "SNP") 5 else 4
selected_stats <- args[stat_start_index:length(args)]

calc_heterozygosity <- "heterozygosity" %in% selected_stats
calc_fis            <- "fis"            %in% selected_stats
calc_ar             <- "ar"             %in% selected_stats
calc_fst            <- "fst"            %in% selected_stats
calc_gst            <- "gst"            %in% selected_stats
calc_djost          <- "djost"          %in% selected_stats

# Differentiation stats: at least one must be selected to compute pairwise matrices
calc_diff <- calc_fst || calc_gst || calc_djost

#Create output file with dynamic columns based on selected stats
base_cols <- c("Dataset", "Pop", "Marker_type", "n_ind")
if (calc_heterozygosity) base_cols <- c(base_cols, "Hobs", "Hexp")
if (calc_ar)             base_cols <- c(base_cols, "Ar")
if (calc_fis)            base_cols <- c(base_cols, "Fis")
if (calc_fst)   base_cols <- c(base_cols, "average_pairwise_Fst")
if (calc_gst)   base_cols <- c(base_cols, "average_Gst_Nei")
if (calc_djost) base_cols <- c(base_cols, "average_Jost_D")

summary_div_stats <- as.data.frame(matrix(ncol = length(base_cols), nrow = 0))
colnames(summary_div_stats) <- base_cols


#############################################################################
# Function : load_snp_data
# Description : Loads SNP data from VCF file and converts to genind object
# with population assignments from indpop file.
#############################################################################

load_snp_data <- function(vcf_path, indpop) {
  #Load VCF file

  vcf_file <- vcfR::read.vcfR(vcf_path)

  #Conversion to genind object
  gen_file <- vcfR::vcfR2genind(vcf_file)

  # Add individuals and their population
  ind_list <- data.frame(Ind = adegenet::indNames(gen_file))
  colnames(ind_list) <- "Ind"

  # Check for individuals in VCF but not in indpop
  missing_ind <- setdiff(ind_list$Ind, indpop$Ind)
  if (length(missing_ind) > 0) {
    stop(sprintf("%d individual(s) from VCF not found in d'indpop : %s", 
                 length(missing_ind), 
                 paste(head(missing_ind, 5), collapse = ", ")))
  }

  ind_df <- ind_list %>%
    mutate(order = row_number()) %>%
    left_join(indpop, by = "Ind") %>%
    arrange(order) %>%
    select(-order) #make sure same order is kept

    adegenet::pop(gen_file) <- as.factor(ind_df$Pop) #add pop on genind object

    return(gen_file)

}

#############################################################################
# Function : load_ssr_data
# Description : Loads SSR/microsatellite data from tabular file and
# converts to genind object
#############################################################################

load_ssr_data <- function(input_path) {
  ssr_raw <- read.csv(input_path, sep="\t")

  geno_cols <- setdiff(colnames(ssr_raw), c("Ind", "Pop"))

  gen_file <- df2genind(ssr_raw[geno_cols],
                      pop = ssr_raw$Pop,
                      ind.names = ssr_raw$Ind,
                      ploidy = 2,
                      NA.char = "0",
                      sep = "/")

  
  return(gen_file)
}

##############################################################################
# Function : compute_div_stats
# Description : Core function that computes diversity statistics from a
# genind object. Works for both SNP and SSR data.
# Calculates: Hobs, Hexp, Fis and allelic richness per population.
##############################################################################

compute_div_stats <- function(gen_file, dataset_name, marker_type) {

  pops <- seppop(gen_file, drop = TRUE)
  n_ind_pop <- table(pop(gen_file))
  pop_names <- levels(pop(gen_file))

  div_stats_dataset <- data.frame(
    Dataset = dataset_name,
    Pop = names(n_ind_pop),
    Marker_type = marker_type,
    n_ind = as.vector(n_ind_pop)
  )

  # Hobs and Hexp computation
  if (calc_heterozygosity) {
    bs_list <- lapply(pops, function(ls) {
      hf <- hierfstat::genind2hierfstat(ls)
      hierfstat::basic.stats(hf)
    })
    div_stats_dataset$Hobs <- as.vector(sapply(bs_list, function(bs) bs$overall["Ho"]))
    div_stats_dataset$Hexp <- as.vector(sapply(bs_list, function(bs) bs$overall["Ht"]))
  }

  # Allelic richness
  if (calc_ar) {
    Richness <- hierfstat::allelic.richness(gen_file, diploid = TRUE)
    Richness_mean <- colMeans(as.matrix(Richness$Ar), na.rm = TRUE)
    Richness_mean <- Richness_mean[names(Richness_mean) != "dumpop"]
    names(Richness_mean) <- pop_names
    div_stats_dataset$Ar <- Richness_mean
  }

  # Fis
  if (calc_fis) {
    Fis <- t(sapply(seppop(gen_file), function(ls) basic.stats(ls)$perloc$Fis))
    div_stats_dataset$Fis <- rowMeans(as.matrix(Fis), na.rm = TRUE)
  }

  # Differentiation stats placeholders (filled later by average_pairwise_by_pop)
  if (calc_fst)   div_stats_dataset$average_pairwise_Fst <- NA
  if (calc_gst)   div_stats_dataset$average_Gst_Nei      <- NA
  if (calc_djost) div_stats_dataset$average_Jost_D        <- NA

  return(div_stats_dataset)
}

########################################################################
# Function : pairwise_values_Fst_DJost
# Description : 
########################################################################
pairwise_values_Fst_DJost <- function(gen_path) {
  matrix_list <- list()

  if (calc_fst) {
    matrix_list$fst <- genet.dist(gen_path, diploid = TRUE, method = "WC84")
  }
  if (calc_gst) {
    matrix_list$gst_pr_nei <- pairwise_Gst_Nei(gen_path)
  }
  if (calc_djost) {
    matrix_list$jost_D <- pairwise_D(gen_path)
  }
  
  return(matrix_list)
}


########################################################################
# Function : average_pairwise_by_pop
# Description : 
########################################################################

average_pairwise_by_pop <- function(matrix_list, dataset_name) {
  # Build list of stats to process based on what was computed
  stats_diff <- names(matrix_list)  # only contains selected stats

  # Get population labels from first matrix
  labels <- attr(matrix_list[[stats_diff[1]]], "Labels")
  
  # Initialize lists to store results
  pops_list <- c()
  fst_list   <- if (calc_fst)   c() else NULL
  gst_list   <- if (calc_gst)   c() else NULL
  jost_list  <- if (calc_djost) c() else NULL

    
  for (pop in labels) {
    pops_list <- c(pops_list, pop)

    if (calc_fst) {
      vals <- as.matrix(matrix_list$fst)[pop, ]
      fst_list <- c(fst_list, mean(vals[names(vals) != pop], na.rm = TRUE))
    }
    if (calc_gst) {
      vals <- as.matrix(matrix_list$gst_pr_nei)[pop, ]
      gst_list <- c(gst_list, mean(vals[names(vals) != pop], na.rm = TRUE))
    }
    if (calc_djost) {
      vals <- as.matrix(matrix_list$jost_D)[pop, ]
      jost_list <- c(jost_list, mean(vals[names(vals) != pop], na.rm = TRUE))
    }
  }

  result <- data.frame(Dataset = dataset_name, Pop = pops_list, stringsAsFactors = FALSE)
  if (calc_fst)   result$average_pairwise_Fst <- fst_list
  if (calc_gst)   result$average_Gst_Nei      <- gst_list
  if (calc_djost) result$average_Jost_D        <- jost_list
  
  return(result)
}

##############################################################
# Function : save_matrices
# Description : Save pairwise matrices to files
###############################################################

save_matrices <- function(matrix_list, dataset_name) {
   measure_labels <- list(
    fst        = "Fst",
    gst_pr_nei = "Gst (Nei)",
    jost_D     = "Jost's D"
  )

  for (measure in names(matrix_list)) {
    # Map internal names to display labels
    matrix_data <- as.matrix(matrix_list[[measure]])
    labels <- attr(matrix_list[[measure]], "Labels")
    rownames(matrix_data) <- labels
    colnames(matrix_data) <- labels

    filename <- paste0("matrices_output/", measure, "_matrix.txt")
    write.table(matrix_data,
                file = filename,
                quote = FALSE,
                sep = "\t",
                row.names = TRUE,
                col.names = NA)

    fill_lab <- measure_labels[[measure]]

    df_long <- as.data.frame(matrix_data) |>
      rownames_to_column("Pop_row") |>
      pivot_longer(-Pop_row, names_to = "Pop_col", values_to = "Value")

    df_long$Value[df_long$Pop_row == df_long$Pop_col] <- NA

    p <- ggplot(df_long, aes(Pop_row, Pop_col, fill = Value)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(name = fill_lab) +
      coord_fixed() +
      theme_minimal() +
      labs(
        title = paste("Pairwise", fill_lab, ":", dataset_name),
        x = "Population",
        y = "Population"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      )

    plot_file <- paste0("matrices_output/", measure, "_heatmap.png")

    ggsave(
      filename = plot_file,
      plot = p,
      width = 8,
      height = 7,
      dpi = 300
    )
  }
}

#######################################
# Main execution 
#######################################

# Extract base name (remove extension, handle parentheses from Galaxy labels)
if (grepl("\\([^)]+\\)\\s*$", input_name)) {
  input_name <- sub(".*\\(([^)]+)\\)\\s*$", "\\1", input_name)
} else {
  input_name <- sub("\\.[^.]+$", "", input_name)
}  

# Load data according to marker type
if (marker_type == "SNP") {
  gen_file <- load_snp_data(input_path, indpop)
} else if (marker_type == "SSR") {
  gen_file <- load_ssr_data(input_path)
}
  
######### By pop ############
# Compute diversity statistics
result <- compute_div_stats(gen_file, input_name, marker_type)

# Pairwise differentiation (only if at least one diff stat selected)
if (calc_diff) {
  pairwise_matrices <- pairwise_values_Fst_DJost(gen_file)
  mean_pairwise <- average_pairwise_by_pop(pairwise_matrices, dataset_name = input_name)

  # Merge mean pairwise values with diversity stats
  diff_cols <- intersect(c("average_pairwise_Fst", "average_Gst_Nei", "average_Jost_D"),
                         colnames(mean_pairwise))
  result <- result %>%
    left_join(mean_pairwise %>% select(Pop, all_of(diff_cols)),
              by = "Pop",
              suffix = c("", "_new"))

  for (col in diff_cols) {
    new_col <- paste0(col, "_new")
    if (new_col %in% colnames(result)) {
      result[[col]] <- result[[new_col]]
      result[[new_col]] <- NULL
    }
  }

  # Save matrices to files
  save_matrices(pairwise_matrices, input_name)
}

# Add results to the global dataframe
if (!is.null(result) && nrow(result) > 0) {
  summary_div_stats <- rbind(summary_div_stats, result)
}

############ Total (all populations combined) #################
# Reload data with "tot" population
if (marker_type == "SNP") {
  #Create a modified indpop with "tot" for all individuals
  indpop_tot <- indpop
  indpop_tot$Pop <- "All_populations_combined"
  gen_file_tot <- load_snp_data(input_path, indpop_tot)
} else if (marker_type == "SSR") {
  gen_file_tot <- load_ssr_data(input_path)
  # For SSR, manually set all populations to "tot"
  adegenet::pop(gen_file_tot) <- as.factor(rep("All_populations_combined", nInd(gen_file_tot)))
}

# Compute diversity statistics for "tot"
result_tot <- compute_div_stats(gen_file_tot, input_name, marker_type)

#Mean pairwise values can't be estimate because only 1 population
if (calc_fst)   result_tot$average_pairwise_Fst <- NA
if (calc_gst)   result_tot$average_Gst_Nei      <- NA
if (calc_djost) result_tot$average_Jost_D        <- NA

# Add "tot results" to the global dataframe
if (!is.null(result_tot) && nrow(result_tot) > 0) {
  summary_div_stats <- rbind(summary_div_stats, result_tot)
}

########### MEAN (average across populations, excluding "tot" rows) #########
summary_means <- summary_div_stats %>%
  filter(Pop != "All_populations_combined") %>%
  group_by(Dataset, Marker_type) %>%
  summarise(
    Pop   = "mean",
    n_ind = sum(n_ind, na.rm = TRUE),
    across(any_of(c("Hobs", "Hexp", "Ar", "Fis",
                    "average_pairwise_Fst", "average_Gst_Nei", "average_Jost_D")),
           ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  select(any_of(colnames(summary_div_stats)))

# Add means to the final table
summary_div_stats <- rbind(summary_div_stats, summary_means)

#Round to 5 decimal
all_numeric_cols <- c("Hobs", "Hexp", "Ar", "Fis",
                      "average_pairwise_Fst", "average_Gst_Nei", "average_Jost_D")
cols_to_round <- intersect(all_numeric_cols, colnames(summary_div_stats))
summary_div_stats[cols_to_round] <- lapply(summary_div_stats[cols_to_round], round, 5)

write.table(summary_div_stats,
              file = "summary_file/summary_div_stats.txt",
              row.names = FALSE,
              quote = FALSE,
              sep = "\t")


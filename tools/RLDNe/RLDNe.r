##### Load package #####
library(RLDNe)
library(dplyr)

##### Give execution permissions to the Ne2-1L binary #####
ne_binary <- Sys.which("Ne2-1L")
if (ne_binary == "") {
  ne_binary <- system.file("bin/linux/Ne2-1L", package = "RLDNe")
}

##### Load arguments #####
args <- commandArgs(trailingOnly = TRUE)
list_gen <- unlist(strsplit(args[1], ",\\s*"))
list_names <- unlist(strsplit(args[2], ",\\s*"))
stopifnot(length(list_gen) == length(list_names))

indpop_path <- args[3]
indpop <- read.table(indpop_path, header = FALSE, sep = "\t")
colnames(indpop) <- c("Ind", "Pop")
marker_type <- args[4]
n_critical_values <- args[5]
critical_freqs <- args[6]
tabular_output <- 0
confidence_intervals <- args[7]
mating_system <- args[8]
max_individuals <- args[9]
pop_range <- args[10]
loc_range <- args[11]
ld_method <- 1
apply_correction <- as.logical(args[12])
apply_harmo      <- as.logical(args[13])

next_args <- 14
if (apply_correction == TRUE) {
  nb_chrom <- as.numeric(args[next_args])
  next_args <- next_args + 1
}

if(apply_harmo == TRUE) {
  maf <- unlist(strsplit(args[next_args], "\\s+"))
  maf <- c(maf,"0+") #Systematic calculation for MAF 0+
}

##### Validate inputs #####
# Genepop files
if (length(list_gen) == 0) stop("gen_files is empty.", call. = FALSE)
if (length(list_gen) != length(list_names)) {
  stop("gen_files and gen_names must have the same number of entries ",
       "(got ", length(list_gen), " vs ", length(list_names), ").", call. = FALSE)
}

missing_gen <- list_gen[!file.exists(list_gen)]
if (length(missing_gen) > 0) {
  stop("Genepop input file(s) not found: ", paste(missing_gen, collapse = ", "), call. = FALSE)
}

#Indpop file
indpop <- tryCatch(
  read.table(indpop_path, header = FALSE, sep = "\t"),
  error = function(e) stop("Could not read indpop file: ", indpop_path,
                            "\n  ", conditionMessage(e), call. = FALSE)
)

if (ncol(indpop) < 2) {
  stop("indpop must have at least 2 tab-separated columns (Individual, Population). ",
       "Found ", ncol(indpop), " column(s).", call. = FALSE)
}

colnames(indpop) <- c("Ind", "Pop")

#########################################################################
# Function : population_order
# Description : Retrieve the order of the population on the genepop file
#########################################################################
population_order <- function(gen_path, indpop, input_name) {
  lines <- readLines(gen_path)
  pop_indices <- grep("^POP", lines, ignore.case = TRUE)

  pop_order <- data.frame(Pop=character(), pop_count=integer(), stringsAsFactors = FALSE)
  pop_id <- 0
  for (pop_index in pop_indices){
    pop_id <- pop_id + 1
    first_ind <- TRUE
    
    for (k in (pop_index + 1):length(lines)) {
      
      current_line <- trimws(lines[k])
      
      #Stop if we hit the next POP block
      if (grepl("^POP", current_line, ignore.case = TRUE)) break
      
      # Skip empty lines if any
      if (nchar(current_line) == 0) next
      
      # For the first individual of each POP, retrieve population name
      if (first_ind) {
        Ind <- trimws(sub(",.*", "", current_line))
        match_row <- indpop[indpop$Ind == Ind, ]
        
        if (nrow(match_row) > 0) {
          actual_Pop <- match_row$Pop[1]
        } else {
          actual_Pop <- paste0("Unknown_Pop_", pop_id)
        }
        
        pop_order <- rbind(pop_order, data.frame(
          Pop  = actual_Pop,
          pop_count = pop_id,
          stringsAsFactors = FALSE
        ))
        first_ind <- FALSE
      }
    }
  }
  return(pop_order)
}

#####################################################################
# Function : anonymise_genepop
# Description : Convert individuals name in number to avoid issue due to 
# the longer of individual's names.
######################################################################
anonymise_genepop <- function(gen_path) {
  ###### Change individuals' names to avoid reading problems for RLDNe #####
  lines <- readLines(gen_path)

  # Find POP line
  pop_indices <- grep("^POP", lines, ignore.case = TRUE)

  # Copy lines (we will modify only individual names)
  new_lines <- lines
  pop_id <- 0

  for (pop_index in pop_indices){
    ind_counter <- 1
    pop_id <- pop_id +1

    # Loop on individuals (lines after POP)
    for (k in (pop_index + 1):length(lines)) {

      current_line <- trimws(lines[k])
      #Stop if we hit the next POP block
      if (grepl("^POP", current_line, ignore.case = TRUE)) break

      # Skip empty lines if any
      if (nchar(current_line) == 0) next

      # Replace only what is before the first comma
      new_lines[k] <- sub(
        "^[^,]+",
        paste0(pop_id,"_",ind_counter),
        lines[k]
      )

      ind_counter <- ind_counter + 1
    }
  }
  name_file <- sub("^.*/(.*)\\.[^.]+$", "\\1", gen_path)
  n_gen_path <- paste0("genfiles/", name_file, ".txt")

  writeLines(new_lines, n_gen_path)
  return(n_gen_path)
}

######################################################################
# Function : extract_base_name
# Description : Extract the input real name
#####################################################################
extract_base_name <- function(input_name) {
  #Extract base name
  input_name <- basename(input_name)
  # Extract content from last parentheses if present
  if (grepl("\\([^)]+\\)\\s*$", input_name)) {
    input_name <- sub(".*\\(([^)]+)\\)\\s*$", "\\1", input_name)
  } else {
    input_name <- sub("\\.[^.]+$", "", input_name)
  }  
  return(input_name)
}

#######################################################################
# Function : params_file
# Description : Creation of the parameter file required for NeEstimator
#######################################################################
params_file <- function(gen_path,
                        gen_name,
                        ld_method,
                        n_critical_values,
                        critical_freqs,
                        tabular_output,
                        confidence_intervals,
                        mating_system,
                        max_individuals,
                        pop_range,
                        loc_range) {
  #Remove extension
  base_params <- sub("\\.txt$", "", gen_name) 

  #Output file path
  results_file <- paste0("results_LDNe/", base_params, "_LDNe_results.txt")

  params_file_name <- paste0("LDNe_params_", base_params, ".txt")

  lines <- c(
    paste0(ld_method, "\t* LD Method"),
    paste0(n_critical_values, "\t* number of critical values"),
    paste0(critical_freqs, "\t* critical allele frequency values"),
    paste0(tabular_output, "\t* tabular output"),
    paste0(confidence_intervals, "\t* confidence intervals"),
    paste0(mating_system, "\t* 0: Random mating, 1: Monogamy (LD method)"),
    paste0(max_individuals,
           "\t* max individual to be processed per pop, 0 for no limit"),
    paste0(pop_range,
           "\t* Pop. range to run, given in pair. No limit if the first = 0"),
    paste0(loc_range,
           "\t* Loc. ranges to run, given in pairs. No limit if the 1st = 0"),
    paste0(results_file, "\t* output file name"),
    paste0(gen_path, "\t* input file")
  )

  writeLines(lines, params_file_name)
  return(list(params_file = params_file_name, results_file = results_file))
}

###################################################################
# Function : extract_info_dataset
# Description : Function to extract information about the dataset
# (pop, dataset name and subsample number)
# Population name is retrieved from indpop based on individuals
# present in the input genepop file (not from the filename)
###################################################################
extract_info_dataset <- function(result_path, pop_order) {

  unique_pops <- unique(as.character(pop_order$Pop))

  #Extract basename (for dataset name and subsample)
  filename <- gsub("\\.txt", "",
                   basename(result_path), ignore.case = TRUE)

  #Extract subsample number
  subsample_match <- regmatches(filename, regexpr("subsample_[0-9]+", filename))
  subsample_number <- if (length(subsample_match) == 1) sub("subsample_", "", subsample_match) else NA

  #Extract name of the dataset
  if (length(unique_pops) == 1) {
    #Single pop : remove pop name from filename if present
    dataset_name <- sub(paste0("_",unique_pops[1],".*"),"",filename)
    dataset_name <- sub("_subsampled?_[0-9]+.*", "", dataset_name)
    pop_in_filename <- unique_pops[1]
  } else {
    # Multiple pops : don't touch the filename
    dataset_name <- sub("_subsampled?_[0-9]+.*", "", filename)
    pop_in_filename <- NA
  }

  return(list(
    dataset_name = dataset_name,
    pop_in_filename  = pop_in_filename,
    subsample = subsample_number
  ))
}

###################################################################
# Function : extract_values
# Description : allows you to extract values from RLDNe result file
###################################################################

extract_values <- function(line) {
  values <- unlist(strsplit(trimws(line), "\\s{2,}|\\t+"))
  values <- values[values != ""]
}

##########################
# LDNe : Main execution
##########################
results_path <- c()
pop_order_list <- list()
i_name <- 0

for (gen_path in list_gen) {

  ##### Extract basename #####
  i_name <- i_name + 1
  gen_base <- extract_base_name(list_names[i_name])

  # Change the name of individuals to avoid issue due to the length of individual's name
  n_gen_path <- anonymise_genepop(gen_path)
  pop_order_current <- population_order(gen_path, indpop, gen_base)

  ###### Create params file #####
  params_output <- params_file(n_gen_path,
                      gen_base,
                        ld_method,
                        n_critical_values,
                        critical_freqs,
                        tabular_output,
                        confidence_intervals,
                        mating_system,
                        max_individuals,
                        pop_range,
                        loc_range)

    ##### Extract params outputs #####
    results_path <- c(results_path, params_output$results_file)
    pop_order_list[[params_output$results_file]] <- pop_order_current

  ##### Run LDNe and return result file #####
  std_out <- RLDNe::run_LDNe(params_output$params_file)
}

############################# Extract RLDNe results ################################
#Create output file
ldne_results <- as.data.frame(matrix(ncol = 12, nrow = 0))
colnames(ldne_results) <- c("Dataset",
                            "Marker_type",
                            "Pop",
                            "Subset",
                            "loci",
                            "Polymorphic_loci",
                            "MAF",
                            "NeLD",
                            "JK_CI_down",
                            "JK_CI_up",
                            "Overall_LD_r2",
                            "Expected_LD_r2")


###################################################################
# Extract value execution
##################################################################
for (text_file in results_path){
  lines <- readLines(text_file)

  # Check result from RLDNe before extract data
  if (any(grepl("Fatal error", lines, ignore.case = TRUE))) {
    message("File ignored due to Fatal error: ", text_file)
    next
  }

  pop_order_file <- pop_order_list[[text_file]]
  n_pops <- nrow(pop_order_file)

  # Find all population start indices
  pop_start_indices <- grep("^Population\\s+[0-9]+", lines)
  all_start_indices <- grep("Lowest Allele Frequency Used", lines)

  # Get SNP line (global, one per file)
  snp_line_index <- grep("Number of Loci", lines)
  snp_line <- lines[snp_line_index]
  loci_used <- sub(".*Number of Loci\\s*=\\s*([0-9]+).*", "\\1", snp_line[1])
  
  # Get dataset info
  info <- extract_info_dataset(text_file, pop_order_file)
  dataset <- info$dataset_name
  subsample <- info$subsample
  
  for (i in seq_len(n_pops)) {
    
    # Define block boundaries for this population
    start_index <- all_start_indices[i]
    
    # End of block = next Population line or end of file
    if (i < n_pops) {
      end_index <- pop_start_indices[i + 1] - 1
    } else {
      end_index <- length(lines)
    }
    
    # Extract block lines
    block_lines <- lines[start_index:end_index]
    
    # Check for fatal/empty block
    if (length(block_lines) < 10) {
      message("Block too short for pop ", i, " in file: ", text_file)
      next
    }
    
    # Remove empty lines for indexing
    table_lines <- block_lines[5:(length(block_lines))]
    table_lines <- table_lines[table_lines != ""]
    
    # Extract MAF
    maf_raw <- block_lines[1]
    maf_content <- sub("Lowest Allele Frequency Used", "", maf_raw)
    raw_mafs <- extract_values(maf_content)
    crit_freqs <- sapply(raw_mafs, function(x) {
      if (grepl("No S\\*", x)) return("1")
      if (grepl("0\\+", x)) return("0")
      return(as.character(as.numeric(x)))
    })
    names(crit_freqs) <- NULL
    n_critical_values = length(crit_freqs)
    
    # Extract Ne and CI values
    find_line <- function(lines, pattern) {
      idx <- grep(pattern, lines)
      if (length(idx) == 0) return(NULL)
        lines[idx[1]]
    }

    estimated_ne_line  <- find_line(block_lines, "Estimated Ne\\^")
    jk_ci_down_line    <- find_line(block_lines, "\\* JackKnife on Samples")
    jk_ci_up_line      <- block_lines[grep("\\* JackKnife on Samples", block_lines)[1] + 1]
    overall_ld_r2_line <- find_line(block_lines, "OverAll r\\^2")
    expected_ld_r2_line <- find_line(block_lines, "Expected r\\^2 Sample")

    estimated_ne   <- if (!is.null(estimated_ne_line))  extract_values(sub("Estimated Ne\\^\\s*=", "", estimated_ne_line))  else rep(NA, n_critical_values)
    overall_ld_r2  <- if (!is.null(overall_ld_r2_line)) extract_values(sub("OverAll r\\^2\\s*=", "", overall_ld_r2_line))   else rep(NA, n_critical_values)
    expected_ld_r2 <- if (!is.null(expected_ld_r2_line)) extract_values(sub("Expected r\\^2 Sample\\s*=", "", expected_ld_r2_line)) else rep(NA, n_critical_values)

    # JackKnife : if all Ne infinite, JackNife on Samples line absents
    jk_idx <- grep("\\* JackKnife on Samples", block_lines)
    if (length(jk_idx) > 0) {
      jk_ci_down <- extract_values(sub("\\* JackKnife on Samples", "", block_lines[jk_idx[1]]))
      jk_ci_up   <- extract_values(block_lines[jk_idx[1] + 1])
    } else {
      jk_ci_down <- rep(NA, n_critical_values)
      jk_ci_up   <- rep(NA, n_critical_values)
    }

    non_poly_line <- block_lines[grepl("Total non-polymorphic", block_lines)]
    if (length(non_poly_line) > 0) {
      non_poly <- as.numeric(sub(".*Total non-polymorphic\\s*=\\s*([0-9]+).*", "\\1", non_poly_line[1]))
      loci_polymorphic <- as.numeric(loci_used) - non_poly
    } else {
      loci_polymorphic <- as.numeric(loci_used)
    }

    #Check extract values
    extracted_lengths <- c(
      length(tail(estimated_ne,  n_critical_values)),
      length(tail(jk_ci_down,    n_critical_values)),
      length(tail(jk_ci_up,      n_critical_values)),
      length(tail(overall_ld_r2, n_critical_values)),
      length(tail(expected_ld_r2,n_critical_values))
    )

    if (any(extracted_lengths != n_critical_values)) {
      message("Skipping pop ", i, " in ", text_file,
              ": extracted value counts don't match n_critical_values (",
              n_critical_values, "). Got: ", paste(extracted_lengths, collapse=", "))
      next
    }
    
    # Get population name from pop_order
    population <- pop_order_file$Pop[pop_order_file$pop_count == i]
    if (length(population) == 0) {
      population <- paste0("Unknown_Pop_", i)
    } else {
      population <- population[1]  # Ensure scalar — take first match only
    }
    
    # Store results
    ldne_results <- rbind(ldne_results,
                          data.frame(
                            Dataset        = rep(dataset, n_critical_values),
                            Pop            = rep(population, n_critical_values),
                            Marker_type    = rep(marker_type, n_critical_values),
                            Subset         = rep(subsample, n_critical_values),
                            loci           = rep(loci_used, n_critical_values),
                            Polymorphic_loci = rep(loci_polymorphic, n_critical_values),
                            MAF            = crit_freqs,
                            NeLD           = tail(estimated_ne, n_critical_values),
                            JK_CI_down     = tail(jk_ci_down, n_critical_values),
                            JK_CI_up       = tail(jk_ci_up, n_critical_values),
                            Overall_LD_r2  = tail(overall_ld_r2, n_critical_values),
                            Expected_LD_r2 = tail(expected_ld_r2, n_critical_values)
                          ))
  }
}

ldne_results <- ldne_results %>%
  mutate(across(c("NeLD",
                  "JK_CI_down",
                  "JK_CI_up",
                  "Overall_LD_r2",
                  "Expected_LD_r2"), as.numeric),
    Subset = ifelse(is.na(Subset), "All_snps", Subset),
    JK_CI_up = ifelse(is.na(JK_CI_up), 999999, JK_CI_up),
    NeLD     = ifelse(is.na(NeLD), 999999, NeLD)
  )

######################## Pseudoreplication correction ######################
if (apply_correction == TRUE) {
  ldne_results <- ldne_results %>%
    mutate(
      NeLD = as.numeric(NeLD),
      NeLD = ifelse(is.na(NeLD), 999999, NeLD),
      NeLD_corrected = ifelse(
        NeLD == 999999,
        999999,
        NeLD / (0.098 + 0.219 * log(nb_chrom))
      )
    )
}

######################## Harmonic mean ###########################
if (apply_harmo == TRUE) {
  harmonic_mean <- function(x) {
    n <- length(x)
    x[is.na(x)] <- 999999 #NA means Infinite
    return(n / sum(1 / x))
  }

  #Detect which colums are present in the input file
  available_cols <- colnames(ldne_results)

  numeric_cols <- c("NeLD", "JK_CI_down",
                  "JK_CI_up",
                  "Overall_LD_r2",
                  "Expected_LD_r2",
                  "NeLD_corrected")
  cols_to_use <- numeric_cols[numeric_cols %in% available_cols]

  #Final table
  ne_estim_all <- data.frame()

  for (i in maf){
    ldne_harm <- ldne_results
    for (col in cols_to_use) {
      ldne_harm[[col]] <- as.numeric(ldne_harm[[col]])
    }

    # Count the number of subsets for each Pop and Marker_type combination
    subset_counts <- ldne_harm %>%
      filter(MAF == i) %>%
      group_by(Dataset, Pop, Marker_type) %>%
      summarize(n_subsets = n(), .groups = "drop")
  
    #Build the summarize file
    ne_estim <- ldne_harm %>%
      filter(MAF == i) %>%
      group_by(Dataset, Pop, Marker_type) %>%
      summarize(across(all_of(cols_to_use), harmonic_mean))
  
    ne_estim$MAF <- i
  
    ne_estim <- ne_estim %>%
      left_join(subset_counts, by = c("Dataset", "Pop", "Marker_type")) %>%
      mutate(Subset = case_when(
        is.na(n_subsets) | n_subsets == 1 ~ "All_snps",
        TRUE ~ paste0("H_mean_btw_", n_subsets, "sub")
        ))%>%
      select(-n_subsets) %>%
      relocate(Subset, .after = "Marker_type") %>%
      relocate(MAF, .after = "Subset") %>%
      mutate(across(any_of(c("NeLD", "JK_CI_down", "JK_CI_up", "NeLD_corrected")), round, digits=0),
            across(any_of(c("Overall_LD_r2", "Expected_LD_r2")), round, digits=5))

      ne_estim_all <- bind_rows(ne_estim_all, ne_estim)
  }

  output_file <- paste0("summary_file/LDNe_harmonic_mean.txt")

  # save file
  write.table(ne_estim_all, output_file, sep = "\t", row.names = FALSE, quote = FALSE)
}

if (apply_correction == TRUE) {
  ldne_results <- ldne_results %>%
    mutate(across(c(NeLD, JK_CI_down, JK_CI_up, NeLD_corrected), round, digits=0),
            across(c(Overall_LD_r2, Expected_LD_r2), round, digits=5))
} else {
  ldne_results <- ldne_results %>%
    mutate(across(c(NeLD, JK_CI_down, JK_CI_up), round, digits=0),
            across(c(Overall_LD_r2, Expected_LD_r2), round, digits=5))
}

write.table(ldne_results,
            file = "summary_file/LDNe_results.txt",
            row.names = FALSE,
            quote = FALSE,
            sep = "\t")
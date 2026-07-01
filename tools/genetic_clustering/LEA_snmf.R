##### Load packages #####

library(LEA)
library(vcfR)
library(hierfstat)
library(ggplot2)
library(tidyr)
library(dplyr)
library(forcats)

##### Load arguments
args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(args) {
  params <- list(
    input_file    = NULL,
    input_format  = NULL,    # "VCF" | "SSR"
    k_fixed       = NULL,    # NULL = auto, integer = K fixed
    k_min         = 1L,
    k_max         = 10L,
    fst_check     = NULL,
    threshold_q   = NULL,
    repetitions = NULL
  )
  i <- 1
  while (i <= length(args)) {
    switch(args[i],
           "--input"     = { params$input_file   <- args[i+1]; i <- i+2 },
           "--format"    = { params$input_format  <- args[i+1]; i <- i+2 },
           "--k-min"     = { params$k_min         <- as.integer(args[i+1]); i <- i+2 },
           "--k-max"     = { params$k_max         <- as.integer(args[i+1]); i <- i+2 },
           "--k-fixed"   = { params$k_fixed       <- as.integer(args[i+1]); i <- i+2 },
           "--fst-check" = { params$fst_check     <- args[i+1]; i <- i+2},
           "--threshold-q" = { params$threshold_q <- as.numeric(args[i+1]); i <- i+2},
           "--repetitions" = { params$repetitions <- as.integer(args[i+1]); i <- i+2},
           { stop(paste("Unknown argument :", args[i])) }
    )
  }
  return(params)
}

##### Functions #####
# --- Load genetic data and write input.geno ---
# Returns:
#   $indiv_names : character vector of individual IDs
#   $geno_matrix : individuals x loci matrix (dosage 0/1/2)
#   (side-effect: writes "input.geno" to disk for snmf())
load_genetic_data <- function(file, format) {
  
  if (format == "VCF") {
    vcf2geno(file, "input.geno")
    vcf         <- read.vcfR(file, verbose = FALSE)
    indiv_names <- colnames(vcf@gt)[-1]
    geno_matrix <- t(extract.gt(vcf, element = "GT",
                                convertNA = TRUE, as.numeric = TRUE))
    
  } else if (format == "SSR") {
    raw_data <- read.table(file, header = TRUE)
    indiv_names <- as.character(raw_data[, 1])
    
    # 1. Split alleles into two separate columns
    geno_raw <- raw_data[, -c(1, 2)]
    
    processed_loci <- lapply(geno_raw, function(col) {
      s <- strsplit(as.character(col), "/")
      do.call(rbind, lapply(s, function(x) {
        if(length(x) == 2) return(x) else return(c("-9", "-9"))
      }))
    })
    ssr_expanded <- do.call(cbind, processed_loci)
    
    # 2. Write temporary STRUCTURE file
    ssr_expanded[is.na(ssr_expanded) | ssr_expanded == "0"] <- "9"
    
    write.table(ssr_expanded, "input.struct", 
                sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
    
    # 3. Corrected conversion
    # FORMAT = 1 : each individual is on a single line
    # ploidy = 2 : LEA knows it must read two columns to form a locus
    struct2geno("input.struct", ploidy = 2, FORMAT = 1, extra.column = 0)
    
    if (file.exists("input.struct.geno")) {
      file.rename("input.struct.geno", "input.geno")
    }
    geno_matrix <- matrix(as.numeric(ssr_expanded), nrow = nrow(raw_data))
  }
  return(list(indiv_names = indiv_names, geno_matrix = geno_matrix))
}


# --- Convert to hierfstat data.frame ---
convert_to_hierfstat <- function(geno_mat, pop, format) {
  if (format == "VCF") {
    formatted_geno <- apply(as.matrix(geno_mat), 2, function(x) {
      res <- rep(NA, length(x))
      res[x == 0] <- 11
      res[x == 1] <- 12
      res[x == 2] <- 22
      return(res)
    })
  } else if (format == "SSR") {
    n_ind <- nrow(geno_mat)
    n_col <- ncol(geno_mat)
    
    # Automatic detection of the number of digits required
    max_val <- max(geno_mat, na.rm = TRUE)
    n_digits <- nchar(as.character(max_val))
    
    fmt_string <- paste0("%0", n_digits, "d")
    
    # We iterate in pairs of two to merge the pairs of alleles
    locus_indices <- seq(1, n_col, by = 2)
    formatted_geno <- matrix(NA, nrow = n_ind, ncol = length(locus_indices))
    
    for (i in 1:length(locus_indices)) {
      col_idx <- locus_indices[i]
      a1 <- sprintf("%03d", geno_mat[, col_idx])
      a2 <- sprintf("%03d", geno_mat[, col_idx + 1])
      
      combined <- paste0(a1, a2)
      combined[is.na(geno_mat[, col_idx]) | is.na(geno_mat[, col_idx + 1])] <- NA
      
      formatted_geno[, i] <- as.numeric(combined)
    }
    colnames(formatted_geno) <- colnames(geno_mat)[locus_indices]
  }
  
  final_df <- data.frame(Pop = pop, formatted_geno)
  return(final_df)
}

# --- Validate K via bootstrap CI on minimum pairwise Fst ---
# Strategy:
#   1. Assign pure individuals (>= threshold_q) to their majority cluster.
#   2. Compute all pairwise Fst via genet.dist() (observed values).
#   3. Identify the least differentiated pair (minimum Fst).
#   4. Run boot.ppfst() on that pair only.
#   5. If lower CI bound <= 0 -> pair not significantly differentiated -> K too large.
#
# Admixed individuals (< threshold_q) are excluded from the test but kept in outputs.
validate_k <- function(project, current_k, geno_matrix,
                       nboot = 999,
                       method = "WC84") {
  
  if (current_k <= 1) return(TRUE)
  
  best_run <- which.min(cross.entropy(project, K = current_k))
  qmat     <- Q(project, K = current_k, run = best_run)
  max_q    <- apply(qmat, 1, max)
  assigned <- apply(qmat, 1, which.max)
  
  pure_idx <- which(max_q >= threshold_q)
  n_pure   <- length(pure_idx)
  cat(sprintf("  K=%d: %d pure (>=%.0f%%), %d admixed\n",
              current_k, n_pure, threshold_q * 100, nrow(qmat) - n_pure))
  
  if (n_pure < current_k * 2) {
    cat("  -> Too few pure individuals. K invalid.\n")
    return(FALSE)
  }
  if (length(unique(assigned[pure_idx])) < current_k) {
    cat("  -> Empty cluster(s) after filtering. K invalid.\n")
    return(FALSE)
  }
  
  geno_pure <- geno_matrix[pure_idx, , drop = FALSE]
  pop_pure  <- assigned[pure_idx]
  dat_hf    <- convert_to_hierfstat(geno_pure, pop_pure, params$input_format)
  
  if (ncol(dat_hf) < 2) {
    cat("  -> No polymorphic loci. K invalid.\n")
    return(FALSE)
  }
  
  # Step 1: observed pairwise Fst (all pairs)
  dist_obs <- as.matrix(genet.dist(dat_hf, method = method))
  if (is.null(dist_obs)) {
    cat("  -> genet.dist error. K invalid.\n")
    return(FALSE)
  }
  
  # Step 2: identify the least differentiated pair
  pairs_idx   <- which(lower.tri(dist_obs), arr.ind = TRUE)
  min_idx     <- which.min(dist_obs[lower.tri(dist_obs)])
  pop_a       <- pairs_idx[min_idx, 1]
  pop_b       <- pairs_idx[min_idx, 2]
  fst_min_obs <- dist_obs[pop_a, pop_b]
  cat(sprintf("  Min pairwise Fst: clusters %d vs %d = %.4f\n",
              pop_a, pop_b, fst_min_obs))
  
  # Step 3: bootstrap CI on that pair only
  pair_mask   <- pop_pure %in% c(pop_a, pop_b)
  dat_hf_pair <- dat_hf[pair_mask, , drop = FALSE]
  
  #Conversion in numeric matrix
  pop_vector <- as.numeric(as.factor(dat_hf_pair[,1]))
  geno_matrix_pure <- as.matrix(dat_hf_pair[,-1])
  mode(geno_matrix_pure) <- "numeric"
  
  #Keep only SNPs polymorphic
  #Keep only SNPs polymorphic
  keep_cols <- apply(geno_matrix_pure, 2, function(x) {
    valid_data <- na.omit(x)
    return(length(valid_data) > 0 && length(unique(valid_data)) > 1)
  })
  
  # FIXED: was geno_matrix[, keep_cols, ...] — must be geno_matrix_pure
  dat_hf_pair_final <- data.frame(Pop = pop_vector, geno_matrix_pure[, keep_cols, drop = FALSE])
  
  if (ncol(dat_hf_pair_final) < 2) {
    cat("  -> ERROR : No actionable SNPs for this pair.\n")
    return(FALSE)
  }
  
  boot_res <- boot.ppfst(dat_hf_pair_final, nboot = nboot, quant = c(0.025,0.975))
  if (is.null(boot_res)) {
    cat("  -> boot.ppfst error. K invalid.\n")
    return(FALSE)
  }
  
  ll_min <- boot_res$ll[1, 2]
  cat(sprintf("  Bootstrap CI lower bound: %.4f\n", ll_min))
  
  if (ll_min <= 0) {
    cat(sprintf("  -> CI includes 0: K=%d too large.\n", current_k))
    return(FALSE)
  }
  cat(sprintf("  -> CI > 0: K=%d retained.\n", current_k))
  return(TRUE)
}

# --- Build cross-entropy plot (with 95% CI ribbon across repetitions) ---
plot_cross_entropy <- function(project, k_min, k_max, best_k) {
  ce_df <- data.frame(
    K    = k_min:k_max,
    CE   = sapply(k_min:k_max, function(k) mean(cross.entropy(project, K = k))),
    CE_min = sapply(k_min:k_max, function(k) min(cross.entropy(project, K = k))),
    CE_max = sapply(k_min:k_max, function(k) max(cross.entropy(project, K = k)))
  )
  ggplot(ce_df, aes(x = K, y = CE)) +
    # Confidence ribbon (min–max across repetitions)
    geom_ribbon(aes(ymin = CE_min, ymax = CE_max),
                fill = "#0072B2", alpha = 0.15) +
    geom_line(color = "#0072B2", linewidth = 0.8) +
    geom_point(color = "#0072B2", size = 2.5) +
    # Error bars (min–max) at each K
    geom_errorbar(aes(ymin = CE_min, ymax = CE_max),
                  width = 0.2, color = "#0072B2", linewidth = 0.5) +
    geom_vline(xintercept = best_k, linetype = "dashed", color = "#D55E00") +
    annotate("text", x = best_k + 0.15, y = max(ce_df$CE_max),
             label = paste("K selected =", best_k),
             hjust = 0, color = "#D55E00", size = 3.5) +
    scale_x_continuous(breaks = k_min:k_max) +
    labs(title    = "Cross-entropy",
         subtitle = "Points = mean  |  Bars = Minimum and maximum across all repetitions",
         x = "K (number of clusters)",
         y = "Cross-entropy") +
    theme_bw(base_size = 13) +
    theme(plot.subtitle = element_text(size = 10, colour = "grey40"))
}

# --- Build structure / admixture barplot ---
# Individuals are sorted by sNMF-assigned cluster (majority Q), then within
# each cluster block by descending dominant ancestry proportion — exactly the
# same logic as the DAPC posterior barplot.
plot_structure <- function(qmatrix, indiv_names, max_q, assigned, best_k) {
  
  # Okabe-Ito colorblind-friendly palette (same as DAPC script)
  okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                 "#0072B2", "#D55E00", "#CC79A7", "#000000")
  colors_k  <- colorRampPalette(okabe_ito)(best_k)
  
  colnames(qmatrix) <- paste0("P", seq_len(best_k))
  
  q_df <- as.data.frame(qmatrix) %>%
    mutate(
      individual  = indiv_names,
      statut      = ifelse(max_q >= threshold_q, "Pure", "Admixed"),
      # cluster_maj = sNMF majority-assigned cluster (1..K)
      cluster_maj = factor(assigned, levels = sort(unique(assigned))),
      # dominant_q  = ancestry proportion for the assigned cluster
      #   used to sort individuals within each cluster block (descending)
      dominant_q  = max_q
    ) %>%
    arrange(cluster_maj, desc(dominant_q)) %>%
    mutate(individual = fct_inorder(factor(individual))) %>%
    pivot_longer(cols = starts_with("P"),
                 names_to  = "pop",
                 values_to = "q")
  
  # Cluster block sizes (for vertical separators)
  cluster_sizes <- q_df %>%
    distinct(individual, cluster_maj) %>%
    count(cluster_maj) %>%
    pull(n)
  separator_x <- cumsum(cluster_sizes) + 0.5
  # Drop last separator (sits beyond the last bar)
  separator_x <- separator_x[-length(separator_x)]
  
  ggplot(q_df) +
    geom_col(aes(x = individual, y = q, fill = pop),
             width = 1, color = NA) +
    scale_fill_manual(values = colors_k,
                      labels = paste0("Cluster ", seq_len(best_k))) +
    # Vertical white separators between cluster blocks
    geom_vline(xintercept = separator_x,
               colour = "white", linewidth = 0.6) +
    # Triangle markers below admixed individuals
    geom_point(
      data = q_df %>% filter(statut == "Admixed") %>% distinct(individual),
      aes(x = individual, y = -0.03),
      shape = 25, size = 1.5, fill = "black", color = "black"
    ) +
    scale_y_continuous(expand = c(0.05, 0),
                       breaks = c(0, 0.5, 1),
                       labels = c("0", "0.5", "1")) +
    labs(title   = paste("Structure sNMF - K =", best_k),
         x       = NULL,
         y       = "Proportion of ancestry (q)",
         fill    = "Cluster",
         caption = "▼ = admixed (q_max < 80%)") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank(),
      axis.line.y      = element_line(color = "grey50"),
      panel.grid       = element_blank(),
      panel.spacing.x  = unit(0, "lines"),
      strip.background = element_rect(fill = "transparent", color = "black"),
      plot.caption     = element_text(size = 8, hjust = 0)
    )
}

##### Main execution #####

# --- Parse & unpack parameters ---
params    <- parse_args(args)
k_min     <- params$k_min
k_max     <- params$k_max
auto_k    <- is.null(params$k_fixed)
fst_check <- !is.null(params$fst_check) && tolower(params$fst_check) == "true"
threshold_q <- params$threshold_q

output_tabular <- "outputs/results_snmf.txt"

cat("=== snmf ===\n")
cat("File   :", params$input_file, "\n")
cat("Format :", params$input_format, "\n")
cat("K mode :", ifelse(auto_k, "auto", paste("fixed =", params$k_fixed)), "\n\n")

# --- Load data ---
genetic_data <- load_genetic_data(params$input_file, params$input_format)
indiv_names  <- genetic_data$indiv_names
geno_matrix  <- genetic_data$geno_matrix

# --- Run sNMF ---
k_range <- if (auto_k) k_min:k_max else params$k_fixed

project <- snmf("input.geno",
                K           = k_range,
                entropy     = TRUE,
                repetitions = params$repetitions,
                alpha       = 100,
                project     = "new")

# --- K selection ---
best_k        <- k_min
found_valid_k <- FALSE

if (auto_k) {
  ce_mean   <- sapply(k_min:k_max, function(k) mean(cross.entropy(project, K = k)))
  k_ce_best <- (k_min:k_max)[which.min(ce_mean)]
  cat(sprintf("\n=== Best K by cross-entropy: %d ===\n", k_ce_best))
  
  if (fst_check) {
    cat("=== Fst validation (bootstrap CI) ===\n")
    for (k_test in seq(k_ce_best, k_min, by = -1)) {
      cat(sprintf("\nTesting K = %d\n", k_test))
      if (validate_k(project, k_test, geno_matrix)) {
        best_k        <- k_test
        found_valid_k <- TRUE
        break
      }
    }
    if (!found_valid_k) {
      warning("No valid K found by Fst test. Falling back to cross-entropy best K.")
      best_k <- k_ce_best
    }
  } else {
    cat("=== Fst validation skipped. Retaining cross-entropy best K. ===\n")
    best_k        <- k_ce_best
    found_valid_k <- TRUE
  }
} else {
  best_k        <- params$k_fixed
  found_valid_k <- TRUE
}

cat(sprintf("\n=== Final K retained: %d ===\n", best_k))

# --- Final Q-matrix ---
best_run <- which.min(cross.entropy(project, K = best_k))
qmatrix  <- Q(project, K = best_k, run = best_run)
max_q    <- apply(qmatrix, 1, max)
assigned <- apply(qmatrix, 1, which.max)

n_admixed <- sum(max_q < threshold_q)
n_pure    <- sum(max_q >= threshold_q)


# --- Plots ---
if (auto_k) {
  p_ce <- plot_cross_entropy(project, k_min, k_max, best_k)
  ggsave("outputs/cross-entropy.png", plot = p_ce,
         width = 12, height = 8, dpi = 150, bg = "white")
} else {
  cat("K fixed by the user: non-generated cross-entropy plot.\n")
}
p_struct <- plot_structure(qmatrix, indiv_names, max_q, assigned, best_k)

# --- Output table ---
q_proportions <- round(qmatrix, 3)
colnames(q_proportions) <- paste0("Prop_", colnames(q_proportions))

final_table <- data.frame(
  Individu         = indiv_names,
  Pop_Assignation     = assigned,
  Max_ancestry = round(max_q, 3),
  Statut           = ifelse(max_q >= threshold_q, "Pure", "Admixed"),
  q_proportions,
  stringsAsFactors = FALSE
)

##### Save outputs #####
write.table(final_table, file = output_tabular,
            sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)

ggsave("outputs/barplot_t_struc.png", plot = p_struct,
       width = 12, height = 8, dpi = 150, bg = "white")

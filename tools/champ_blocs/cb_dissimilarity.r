# author: "Jonathan Richir"
# date: "01 October 2022"


#Rscript

###############################
##  ##
###############################

#####Packages : dplyr
#               tidyr
#               readr
#               writexl
#               stringr
#               readxl
#               tibble
#               lubridate
#               cowplot
#               magrittr
#               rmarkdown
library(magrittr)
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    ecology_input <- args[1]
    choice <- args[2]
    choice_date <- as.numeric(args[3])
}


#############################################################
#                                                           #
#               Load and some data handling                 #
#                                                           #
#############################################################
## load qecbNew data ; df saved from CB_qecb script

qecbnato0 <- readRDS(ecology_input)

# first, create vector (4) for qecb and fishing by region (same as above)

bret_egmp_basq_qecb <- c(
  "X..algues.brunes",
  "X..algues.rouges",
  "X..algues.vertes",
  "X..Cladophora",
  "X..Lithophyllum",
  "Nb.Littorina.obtusata",
  "Nb.Gibbula.cineraria",
  "Nb.Gibbula.pennanti",
  "Nb.Gibbula.umbilicalis",
  "Nb.Phallusia.mamillata",
  "Nb.Tethya.aurantium",
  "Nb.Spirobranchus.lamarckii.total",
  "Nb.spirorbis.total",
  "X..Eponges",
  "X..Ascidies.Coloniales",
  "X..Ascidies.Solitaires",
  "X..Bryozoaires.Dresses",
  "X..Balanes.Vivantes"
  #, "X..Recouvrement.Sediment"
  #, "X..Roche.Nue"
  #, "X..Surface.Accolement"
  )

egmp_basq_qecb <- c("Nb.Crassostrea.gigas", "Nb.Ostrea.edulis", "X..Mytilus.sp.", "X..Hermelles", "X..Hydraires")

bret_egmp_basq_fishing <- c("Nb.Cancer.pagurus..Tourteau.",
                            "Nb.Necora.puber..Etrille.",
                            "Nb.Carcinus.maenas..Crabe.vert.",
                            "Nb.Nucella.lapilus..Pourpre.",
                            "Nb.Galathea..Galathées.",
                            "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
                            "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
                            "Nb.Haliotis.tuberculata..Ormeau.",
                            "Nb.Littorina.littorea..Bigorneau.",
                            "Nb.Xantho.pilipes..Xanthe.poilu.",
                            "Nb.Mimachlamys.varia..Pétoncle.noir.")

egmp_basq_fishing <- c("Nb.Eriphia.verrucosa..Crabe.verruqueux.", "Nb.Octopus.vulgaris..Poulpe.", "Nb.Paracentrotus.lividus..Oursin.", "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.")

# here I can choose to either replace spirorbis and/or spirobranchus by their log10 transformation in bret_egmp_basq_qecb vector
bret_egmp_basq_qecb <- replace(bret_egmp_basq_qecb, bret_egmp_basq_qecb == "Nb.spirorbis.total", "log10.Nb.spirorbis.total")
saveRDS(bret_egmp_basq_qecb, "bret_egmp_basq_qecb.RDS")


#############################################################
#                                                           #
#                  Compute dissimilarity                    #
#                                                           #
#############################################################
### determination of coefficient of dissimilarity face sup mobile bloc vs fixed bloc

# loop in a fct

matri_fct_bmf <- function(data, conca) {

  matri_df <- data

  for (x in c(1:length(unique(matri_df$site_year_month_day)))) {

    qecbnato0_x <- matri_df %>% dplyr::filter(site_year_month_day == unique(matri_df$site_year_month_day)[[x]])

    rownames(qecbnato0_x) <- paste0(qecbnato0_x$Type.Bloc, "_", qecbnato0_x$Face,  "_", qecbnato0_x$Numéro.Bloc.échantillon, "_", qecbnato0_x$quadrat_bis)


  mtxdis <- vegan::vegdist(
    sqrt(qecbnato0_x[, conca]), #Transform your species abundance data_ Typically, raw abundances are transformed prior to analysis. Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats.stackexchange.com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)

    na.rm = TRUE,
    method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis. If your data contains samples that are all-zero you will run into the double zero problem. This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  )


 #https://rdrr.io/github/phytomosaic/ecole/man/bray0.html
 # mtxdis <- ecole::bray0(
 #   sqrt
  #  (qecbnato0_x[,conca]), na.rm = TRUE)

  expand.grid(mtxdis)

  mtxdisdf_ <- as.data.frame(as.matrix(mtxdis))

  a_ <- NA
  b_ <- NA
  c_ <- NA
  d_ <- NA
  e_ <- NA
  f_ <- NA
  g_ <- NA
  h_ <- NA
  i_ <- NA
  j_ <- NA
  k_ <- NA
  l_ <- NA
  m_ <- NA
  n_ <- NA

  for (z in c(1:nrow(mtxdisdf_))) {

    a_[[z]] <- (paste0(rownames(mtxdisdf_[z + 1, ]), " & ", ifelse(nrow(mtxdisdf_) >= 1, colnames(mtxdisdf_[1]), NA)))
    b_[[z]] <- (paste0(rownames(mtxdisdf_[z + 2, ]), " & ", ifelse(nrow(mtxdisdf_) >= 2, colnames(mtxdisdf_[2]), NA)))
    c_[[z]] <- (paste0(rownames(mtxdisdf_[z + 3, ]), " & ", ifelse(nrow(mtxdisdf_) >= 3, colnames(mtxdisdf_[3]), NA)))
    d_[[z]] <- (paste0(rownames(mtxdisdf_[z + 4, ]), " & ", ifelse(nrow(mtxdisdf_) >= 4, colnames(mtxdisdf_[4]), NA)))
    e_[[z]] <- (paste0(rownames(mtxdisdf_[z + 5, ]), " & ", ifelse(nrow(mtxdisdf_) >= 5, colnames(mtxdisdf_[5]), NA)))
    f_[[z]] <- (paste0(rownames(mtxdisdf_[z + 6, ]), " & ", ifelse(nrow(mtxdisdf_) >= 6, colnames(mtxdisdf_[6]), NA)))
    g_[[z]] <- (paste0(rownames(mtxdisdf_[z + 7, ]), " & ", ifelse(nrow(mtxdisdf_) >= 7, colnames(mtxdisdf_[7]), NA)))
    h_[[z]] <- (paste0(rownames(mtxdisdf_[z + 8, ]), " & ", ifelse(nrow(mtxdisdf_) >= 8, colnames(mtxdisdf_[8]), NA)))
    i_[[z]] <- (paste0(rownames(mtxdisdf_[z + 9, ]), " & ", ifelse(nrow(mtxdisdf_) >= 9, colnames(mtxdisdf_[9]), NA)))
    j_[[z]] <- (paste0(rownames(mtxdisdf_[z + 10, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 10, colnames(mtxdisdf_[10]), NA)))
    k_[[z]] <- (paste0(rownames(mtxdisdf_[z + 11, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 11, colnames(mtxdisdf_[11]), NA)))
    l_[[z]] <- (paste0(rownames(mtxdisdf_[z + 12, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 12, colnames(mtxdisdf_[12]), NA)))
    m_[[z]] <- (paste0(rownames(mtxdisdf_[z + 13, ]), " & ", ifelse(nrow(mtxdisdf_) >= 13, colnames(mtxdisdf_[13]), NA)))
    n_[[z]] <- (paste0(rownames(mtxdisdf_[z + 14, ]), " & ", ifelse(nrow(mtxdisdf_) >= 14, colnames(mtxdisdf_[14]), NA)))

  }

  rm(z)

  y <- length(a_) - (ifelse(nrow(mtxdisdf_) >= 1, 1, nrow(mtxdisdf_)))
  a_ <- a_[1:y]
  y <- length(b_) - (ifelse(nrow(mtxdisdf_) >= 2, 2, nrow(mtxdisdf_)))
  b_ <- b_[1:y]
  y <- length(c_) - (ifelse(nrow(mtxdisdf_) >= 3, 3, nrow(mtxdisdf_)))
  c_ <- c_[1:y]
  y <- length(d_) - (ifelse(nrow(mtxdisdf_) >= 4, 4, nrow(mtxdisdf_)))
  d_ <- d_[1:y]
  y <- length(e_) - (ifelse(nrow(mtxdisdf_) >= 5, 5, nrow(mtxdisdf_)))
  e_ <- e_[1:y]
  y <- length(f_) - (ifelse(nrow(mtxdisdf_) >= 6, 6, nrow(mtxdisdf_)))
  f_ <- f_[1:y]
  y <- length(g_) - (ifelse(nrow(mtxdisdf_) >= 7, 7, nrow(mtxdisdf_)))
  g_ <- g_[1:y]
  y <- length(h_) - (ifelse(nrow(mtxdisdf_) >= 8, 8, nrow(mtxdisdf_)))
  h_ <- h_[1:y]
  y <- length(i_) - (ifelse(nrow(mtxdisdf_) >= 9, 9, nrow(mtxdisdf_)))
  i_ <- i_[1:y]
  y <- length(j_) - (ifelse(nrow(mtxdisdf_) >= 10, 10, nrow(mtxdisdf_)))
  j_ <- j_[1:y]
  y <- length(k_) - (ifelse(nrow(mtxdisdf_) >= 11, 11, nrow(mtxdisdf_)))
  k_ <- k_[1:y]
  y <- length(l_) - (ifelse(nrow(mtxdisdf_) >= 12, 12, nrow(mtxdisdf_)))
  l_ <- l_[1:y]
  y <- length(m_) - (ifelse(nrow(mtxdisdf_) >= 13, 13, nrow(mtxdisdf_)))
  m_ <- m_[1:y]
  y <- length(n_) - (ifelse(nrow(mtxdisdf_) >= 14, 14, nrow(mtxdisdf_)))
  n_ <- n_[1:y]

  rm(y)

  name_ <- c(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_)
  df_ <- data.frame(expand.grid(mtxdis), name_[1:nrow(expand.grid(mtxdis))])
  names(df_) <- c("dist.", "name_")

  rm(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_)
  rm(name_)

  q_ <- strsplit(df_$name_, " & ")
  mat_  <- matrix(unlist(q_), ncol = 2, byrow = TRUE)
  q_df_   <- as.data.frame(matrix(unlist(q_), ncol = 2, byrow = TRUE))
  df_ <- dplyr::bind_cols(df_, q_df_)

  rm(q_, mat_, q_df_)

  split_ <- strsplit(df_$V1, "_")
  v1_split_ <- as.data.frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))
  split_ <- strsplit(df_$V2, "_")
  v2_split_ <- as.data.frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))

  df_ <- dplyr::bind_cols(df_, v1_split_)
  df_ <- dplyr::bind_cols(df_, v2_split_)
  df_red_ <- subset(df_, V4...8 == V4...12 & V1...5 != V1...9)
  site_year_month_day <- rep(unique(qecbnato0_x$site_year_month_day), nrow(df_red_))

  df_red_ <- tibble::add_column(df_red_, site_year_month_day, .before = "dist.")

  rm(split_, v1_split_, v2_split_)
  rm(mtxdis, mtxdisdf_, df_, site_year_month_day)

  matri_list[[x]] <- df_red_
  matri_list <<- matri_list

  rm(df_red_, qecbnato0_x, x)

  }

  matri_df <- do.call("rbind", matri_list)

  names(matri_df) <- c("site_year_month_day", "dist.", "name_", "name_left", "name_right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")

  matri_df <<- matri_df

  hist(matri_df$dist.)

}

data_ <- dplyr::filter(qecbnato0, Face == "face supérieure")
data_$Type.Bloc <- ifelse(as.character(data_$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data_$Type.Bloc))
matri_list <- vector("list", length(unique(data_$site_year_month_day)))

matri_fct_bmf(data = data_, conca = c(bret_egmp_basq_qecb, egmp_basq_qecb))
hist(matri_df$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BF -")))

matri_full_bm_bf_fs <- matri_df


rm(data_, matri_df, matri_list)


### determination of coefficient of dissimilarity between blocs mobiles face sup vs face inf.

# loop in a fct

matri_fct_bmm <- function(data, conca) {

  matri_df <- data

  for (x in c(1:length(unique(matri_df$site_year_month_day)))) {

    qecbnato0_x <- matri_df %>% dplyr::filter(site_year_month_day == unique(matri_df$site_year_month_day)[[x]])

    rownames(qecbnato0_x) <- paste0(qecbnato0_x$Type.Bloc, "_", qecbnato0_x$Face,  "_", qecbnato0_x$Numéro.Bloc.échantillon, "_", qecbnato0_x$quadrat_bis)


  mtxdis <- vegan::vegdist(
  sqrt(qecbnato0_x[, c(bret_egmp_basq_qecb)]), #Transform your species abundance data_ Typically, raw abundances are transformed prior to analysis. Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats.stackexchange.com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)
  na.rm = TRUE,
  method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis. If your data contains samples that are all-zero you will run into the double zero problem. This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  )


  #mtxdis <- ecole::bray0(
   # sqrt(qecbnato0_x[, conca]), na.rm = TRUE)


  expand.grid(mtxdis)

  mtxdisdf_ <- as.data.frame(as.matrix(mtxdis))

  a_ <- NA
  b_ <- NA
  c_ <- NA
  d_ <- NA
  e_ <- NA
  f_ <- NA
  g_ <- NA
  h_ <- NA
  i_ <- NA
  j_ <- NA
  k_ <- NA
  l_ <- NA
  m_ <- NA
  n_ <- NA
  o_ <- NA
  p_ <- NA
  q_ <- NA
  r_ <- NA
  s_ <- NA

  for (z in c(1:nrow(mtxdisdf_))) {

    a_[[z]] <- (paste0(rownames(mtxdisdf_[z + 1, ]), " & ", ifelse(nrow(mtxdisdf_) >= 1, colnames(mtxdisdf_[1]), NA)))
    b_[[z]] <- (paste0(rownames(mtxdisdf_[z + 2, ]), " & ", ifelse(nrow(mtxdisdf_) >= 2, colnames(mtxdisdf_[2]), NA)))
    c_[[z]] <- (paste0(rownames(mtxdisdf_[z + 3, ]), " & ", ifelse(nrow(mtxdisdf_) >= 3, colnames(mtxdisdf_[3]), NA)))
    d_[[z]] <- (paste0(rownames(mtxdisdf_[z + 4, ]), " & ", ifelse(nrow(mtxdisdf_) >= 4, colnames(mtxdisdf_[4]), NA)))
    e_[[z]] <- (paste0(rownames(mtxdisdf_[z + 5, ]), " & ", ifelse(nrow(mtxdisdf_) >= 5, colnames(mtxdisdf_[5]), NA)))
    f_[[z]] <- (paste0(rownames(mtxdisdf_[z + 6, ]), " & ", ifelse(nrow(mtxdisdf_) >= 6, colnames(mtxdisdf_[6]), NA)))
    g_[[z]] <- (paste0(rownames(mtxdisdf_[z + 7, ]), " & ", ifelse(nrow(mtxdisdf_) >= 7, colnames(mtxdisdf_[7]), NA)))
    h_[[z]] <- (paste0(rownames(mtxdisdf_[z + 8, ]), " & ", ifelse(nrow(mtxdisdf_) >= 8, colnames(mtxdisdf_[8]), NA)))
    i_[[z]] <- (paste0(rownames(mtxdisdf_[z + 9, ]), " & ", ifelse(nrow(mtxdisdf_) >= 9, colnames(mtxdisdf_[9]), NA)))
    j_[[z]] <- (paste0(rownames(mtxdisdf_[z + 10, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 10, colnames(mtxdisdf_[10]), NA)))
    k_[[z]] <- (paste0(rownames(mtxdisdf_[z + 11, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 11, colnames(mtxdisdf_[11]), NA)))
    l_[[z]] <- (paste0(rownames(mtxdisdf_[z + 12, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 12, colnames(mtxdisdf_[12]), NA)))
    m_[[z]] <- (paste0(rownames(mtxdisdf_[z + 13, ]), " & ", ifelse(nrow(mtxdisdf_) >= 13, colnames(mtxdisdf_[13]), NA)))
    n_[[z]] <- (paste0(rownames(mtxdisdf_[z + 14, ]), " & ", ifelse(nrow(mtxdisdf_) >= 14, colnames(mtxdisdf_[14]), NA)))
    o_[[z]] <- (paste0(rownames(mtxdisdf_[z + 15, ]), " & ", ifelse(nrow(mtxdisdf_) >= 15, colnames(mtxdisdf_[15]), NA)))
    p_[[z]] <- (paste0(rownames(mtxdisdf_[z + 16, ]), " & ", ifelse(nrow(mtxdisdf_) >= 16, colnames(mtxdisdf_[16]), NA)))
    q_[[z]] <- (paste0(rownames(mtxdisdf_[z + 17, ]), " & ", ifelse(nrow(mtxdisdf_) >= 17, colnames(mtxdisdf_[17]), NA)))
    r_[[z]] <- (paste0(rownames(mtxdisdf_[z + 18, ]), " & ", ifelse(nrow(mtxdisdf_) >= 18, colnames(mtxdisdf_[18]), NA)))
    s_[[z]] <- (paste0(rownames(mtxdisdf_[z + 19, ]), " & ", ifelse(nrow(mtxdisdf_) >= 19, colnames(mtxdisdf_[19]), NA)))

  }

  rm(z)

  y <- length(a_) - (ifelse(nrow(mtxdisdf_) >= 1, 1, nrow(mtxdisdf_)))
  a_ <- a_[1:y]
  y <- length(b_) - (ifelse(nrow(mtxdisdf_) >= 2, 2, nrow(mtxdisdf_)))
  b_ <- b_[1:y]
  y <- length(c_) - (ifelse(nrow(mtxdisdf_) >= 3, 3, nrow(mtxdisdf_)))
  c_ <- c_[1:y]
  y <- length(d_) - (ifelse(nrow(mtxdisdf_) >= 4, 4, nrow(mtxdisdf_)))
  d_ <- d_[1:y]
  y <- length(e_) - (ifelse(nrow(mtxdisdf_) >= 5, 5, nrow(mtxdisdf_)))
  e_ <- e_[1:y]
  y <- length(f_) - (ifelse(nrow(mtxdisdf_) >= 6, 6, nrow(mtxdisdf_)))
  f_ <- f_[1:y]
  y <- length(g_) - (ifelse(nrow(mtxdisdf_) >= 7, 7, nrow(mtxdisdf_)))
  g_ <- g_[1:y]
  y <- length(h_) - (ifelse(nrow(mtxdisdf_) >= 8, 8, nrow(mtxdisdf_)))
  h_ <- h_[1:y]
  y <- length(i_) - (ifelse(nrow(mtxdisdf_) >= 9, 9, nrow(mtxdisdf_)))
  i_ <- i_[1:y]
  y <- length(j_) - (ifelse(nrow(mtxdisdf_) >= 10, 10, nrow(mtxdisdf_)))
  j_ <- j_[1:y]
  y <- length(k_) - (ifelse(nrow(mtxdisdf_) >= 11, 11, nrow(mtxdisdf_)))
  k_ <- k_[1:y]
  y <- length(l_) - (ifelse(nrow(mtxdisdf_) >= 12, 12, nrow(mtxdisdf_)))
  l_ <- l_[1:y]
  y <- length(m_) - (ifelse(nrow(mtxdisdf_) >= 13, 13, nrow(mtxdisdf_)))
  m_ <- m_[1:y]
  y <- length(n_) - (ifelse(nrow(mtxdisdf_) >= 14, 14, nrow(mtxdisdf_)))
  n_ <- n_[1:y]
  y <- length(o_) - (ifelse(nrow(mtxdisdf_) >= 15, 15, nrow(mtxdisdf_)))
  o_ <- o_[1:y]
  y <- length(p_) - (ifelse(nrow(mtxdisdf_) >= 16, 16, nrow(mtxdisdf_)))
  p_ <- p_[1:y]
  y <- length(q_) - (ifelse(nrow(mtxdisdf_) >= 17, 17, nrow(mtxdisdf_)))
  q_ <- q_[1:y]
  y <- length(r_) - (ifelse(nrow(mtxdisdf_) >= 18, 18, nrow(mtxdisdf_)))
  r_ <- r_[1:y]
  y <- length(s_) - (ifelse(nrow(mtxdisdf_) >= 19, 19, nrow(mtxdisdf_)))
  s_ <- s_[1:y]

  rm(y)

  name_ <- c(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_, o_, p_, q_, r_, s_)
  df_ <- data.frame(expand.grid(mtxdis), name_[1:seq_len(nrow(expand.grid(mtxdis)))])
  names(df_) <- c("dist.", "name_")

  rm(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_, o_, p_, q_, r_, s_)
  rm(name_)

  q_ <- strsplit(df_$name_, " & ")
  mat_  <- matrix(unlist(q_), ncol = 2, byrow = TRUE)
  q_df_   <- as.data.frame(matrix(unlist(q_), ncol = 2, byrow = TRUE))
  df_ <- dplyr::bind_cols(df_, q_df_)

  rm(q_, mat_, q_df_)

  split_ <- strsplit(df_$V1, "_")
  v1_split_ <- as.data.frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))
  split_ <- strsplit(df_$V2, "_")
  v2_split_ <- as.data.frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))

  df_ <- dplyr::bind_cols(df_, v1_split_)
  df_ <- dplyr::bind_cols(df_, v2_split_)
  df_red_ <- subset(df_, V4...8 == V4...12 & V3...7 == V3...11)
  site_year_month_day <- rep(unique(qecbnato0_x$site_year_month_day), nrow(df_red_))

  df_red_ <- tibble::add_column(df_red_, site_year_month_day, .before = "dist.")

  rm(split_, v1_split_, v2_split_)
  rm(mtxdis, mtxdisdf_, df_, site_year_month_day)

  matri_list[[x]] <- df_red_
  matri_list <<- matri_list

  rm(df_red_, qecbnato0_x, x)

  }

  matri_df <- do.call("rbind", matri_list)

  names(matri_df) <- c("site_year_month_day", "dist.", "name_", "name_left", "name_right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")

  matri_df <<- matri_df

  hist(matri_df$dist.)

}

data_ <- dplyr::filter(qecbnato0, Type.Bloc == "Bloc mobile")
matri_list <- vector("list", length(unique(data_$site_year_month_day)))

matri_fct_bmm(data = data_, conca = c(bret_egmp_basq_qecb, egmp_basq_qecb))
hist(matri_df$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BMfi -")))

matri_full_bm_bf_fi <- matri_df

rm(data_, matri_df, matri_list)


#############################################################
#                                                           #
#                     Plot dissimilarity                    #
#                                                           #
#############################################################
## plot

# activate line

matri_full_bm_bf_fs <- tidyr::separate(matri_full_bm_bf_fs, "site_year_month_day", into = c("departement", "Site", "Year", "Month", "Day"), remove = FALSE)
matri_full_bm_bf_fs$Site <- paste0(matri_full_bm_bf_fs$departement, "_", matri_full_bm_bf_fs$Site)
matri_full_bm_bf_fs <- subset(matri_full_bm_bf_fs, select = - c(departement))

matri_full_bm_bf_fs <- tibble::add_column(matri_full_bm_bf_fs, Date = as.Date(paste0(matri_full_bm_bf_fs$Year, "-", matri_full_bm_bf_fs$Month, "-", matri_full_bm_bf_fs$Day), origin = "1970-01-01"), .after = "Site")
matri_full_bm_bf_fs$Site <- as.factor(matri_full_bm_bf_fs$Site)

matri_full_bm_bf_fi <- tidyr::separate(matri_full_bm_bf_fi, "site_year_month_day", into = c("departement", "Site", "Year", "Month", "Day"), remove = FALSE)
matri_full_bm_bf_fi$Site <- paste0(matri_full_bm_bf_fi$departement, "_", matri_full_bm_bf_fi$Site)
matri_full_bm_bf_fi <- subset(matri_full_bm_bf_fi, select = - c(departement))
matri_full_bm_bf_fi <- tibble::add_column(matri_full_bm_bf_fi, Date = as.Date(paste0(matri_full_bm_bf_fi$Year, "-", matri_full_bm_bf_fi$Month, "-", matri_full_bm_bf_fi$Day), origin = "1970-01-01"), .after = "Site")
matri_full_bm_bf_fi$Site <- as.factor(matri_full_bm_bf_fi$Site)

# if error message "Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state"


bf_fs_plot <- ggplot2::ggplot(matri_full_bm_bf_fs, ggplot2::aes(x = Site, y = dist.)) +
  ggplot2::geom_boxplot() +
  #geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("distance diss. BM.BF_FS") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot2::ggsave("distance_diss_BF_FS.png", bf_fs_plot, height = 4.5, width = 4)

fs_fi_plot <- ggplot2::ggplot(matri_full_bm_bf_fi, ggplot2::aes(x = Site, y = dist.)) +
  ggplot2::geom_boxplot() +
  #geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("distance diss. BM_FS.FI") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot2::ggsave("distance_diss_FS_FI.png", fs_fi_plot, height = 4.5, width = 4)

# issue with type de bloc, numéro de bloc and quadrat for df_ BM.BF_FS, cfr df_ left vs right variables doesn't give the right combination (variables with left vs right label in names come from the dissimilarity coefficient functions).
matri_full_bm_bf_fs$Quadrat <- NA
for (i in c(1:seq_len(nrow(matri_full_bm_bf_fs)))) {
  ifelse(matri_full_bm_bf_fs$Type.Bloc.left[i] == "Bloc mobile", matri_full_bm_bf_fs$Quadrat[i] <- matri_full_bm_bf_fs$Quadrat.left[i], matri_full_bm_bf_fs$Quadrat[i] <- matri_full_bm_bf_fs$Quadrat.right[i])
}
matri_full_bm_bf_fs$Numéro.Bloc <- NA
for (i in c(1:seq_len(nrow(matri_full_bm_bf_fs)))) {
  ifelse(matri_full_bm_bf_fs$Type.Bloc.left[i] == "Bloc mobile", matri_full_bm_bf_fs$Numéro.Bloc[i] <- matri_full_bm_bf_fs$Numéro.Bloc.échantillon.left[i], matri_full_bm_bf_fs$Numéro.Bloc[i] <- matri_full_bm_bf_fs$Numéro.Bloc.échantillon.right[i])
}

matri_full_bm_bf_fs <- tibble::add_column(matri_full_bm_bf_fs, site_year_month_day.q_BMnb = paste0(matri_full_bm_bf_fs$site_year_month_day, "_",  matri_full_bm_bf_fs$Quadrat, "_", matri_full_bm_bf_fs$Numéro.Bloc), .after = "site_year_month_day")
matri_full_bm_bf_fi <- tibble::add_column(matri_full_bm_bf_fi, site_year_month_day.q_BMnb = paste0(matri_full_bm_bf_fi$site_year_month_day, "_",  matri_full_bm_bf_fi$Quadrat.left, "_", matri_full_bm_bf_fi$Numéro.Bloc.échantillon.left), .after = "site_year_month_day")

colnames(matri_full_bm_bf_fs) <- paste("BM.BF_FS", colnames(matri_full_bm_bf_fs), sep = "_")
matri_full_bm_bf_fs <- dplyr::rename(matri_full_bm_bf_fs, site_year_month_day.q_BMnb = BM.BF_FS_site_year_month_day.q_BMnb)
colnames(matri_full_bm_bf_fi) <- paste("BM_FS.FI", colnames(matri_full_bm_bf_fi), sep = "_")
matri_full_bm_bf_fi <- dplyr::rename(matri_full_bm_bf_fi, site_year_month_day.q_BMnb = BM_FS.FI_site_year_month_day.q_BMnb)

matri_full <- dplyr::full_join(matri_full_bm_bf_fs[, c("site_year_month_day.q_BMnb", "BM.BF_FS_dist.")], matri_full_bm_bf_fi[, c("site_year_month_day.q_BMnb", "BM_FS.FI_dist.")])

matri_full <- tidyr::separate(matri_full, "site_year_month_day.q_BMnb", into = c("departement", "Site", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"), remove = FALSE)
matri_full$Site <- paste0(matri_full$departement, "_", matri_full$Site)
matri_full <- subset(matri_full, select = - c(departement))
matri_full <- tibble::add_column(matri_full, Date = as.Date(paste0(matri_full$Year, "-", matri_full$Month, "-", matri_full$Day), origin = "1970-01-01"), .after = "Site")

# Name for report/plot

matri_full <- tibble::add_column(matri_full, Site_bis = matri_full$Site, .after = "Site")

matri_full$Site_bis <- ifelse(matri_full$Site == "GDMO_Locmariaquer", "Locmariaquer", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "GDMO_BegLann", "Beg Lann", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FOUR_PlateauFour", "Plateau du Four", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_GroinCou", "Groin du Cou", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_PasEmsembert", "Le Pas d'Emsembert", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_BreeBains", "La Brée-les-Bains", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_PerreAntiochat", "Le Perré d'Antiochat", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_Chassiron", "Chassiron", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "BASQ_FlotsBleusZP", "Les Flots Bleus / zone pêcheurs", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "BASQ_FlotsBleusZF", "Les Flots Bleus / zone familles", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "GONB_IlotStMichel", "Îlot Saint-Michel", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_Quemenes", "Quéménès", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_SeinGoulenez", "Île de Sein - Goulenez", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_SeinKilaourou", "Île de Sein - Kilaourou", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_Verdelet", "Îlot du Verdelet", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_Piegu", "Piégu", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_Bilfot", "Pointe de Bilfot", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_IlePlate", "Île Plate", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "PDMO_Perharidy", "Perharidy", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "BRES_Keraliou", "Keraliou", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_Mousterlin", "Pointe de Mousterlin", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_StNicolasGlenan", "Saint-Nicolas des Glénan", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_AnseRoz", "Pointe de l'Anse du Roz", matri_full$Site_bis)



#############################################################
#                                                           #
#            Plot the dissimilarity per site                #
#                                                           #
#############################################################
## plot dissimilarity coefficient

matri_full$Year <- as.integer(matri_full$Year)
matri_full$Month <- as.integer(matri_full$Month)
matri_full$Day <- as.integer(matri_full$Day)


## BM_FS.FI_dist => mobile boulder upper vs lower faces

# Stats

bm_fs_fi_dist_stat <- matri_full %>% dplyr::group_by(Site, Site_bis, Date, Year, Month, Day) %>% dplyr::summarize(BM_FS.FI_dist.moy = mean(BM_FS.FI_dist., na.rm = TRUE), BM_FS.FI_dist.et = sd(BM_FS.FI_dist., na.rm = TRUE), BM_FS.FI_dist.med = median(BM_FS.FI_dist., na.rm = TRUE), BM_FS.FI_dist.min = min(BM_FS.FI_dist., na.rm = TRUE), BM_FS.FI_dist.max = max(BM_FS.FI_dist., na.rm = TRUE), nb. = dplyr::n(), nb.notNa = sum(!is.na(BM_FS.FI_dist.)))

bm_fs_fi_dist_stat <- dplyr::ungroup(bm_fs_fi_dist_stat)

# Quality scale based on quartiles

if (choice == "N") {
  one <- round(mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. <= quantile(matri_full$BM_FS.FI_dist., 0.25, na.rm = TRUE))["BM_FS.FI_dist."])), digits = 3)
  two <- round(mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. > quantile(matri_full$BM_FS.FI_dist., 0.25, na.rm = TRUE) & BM_FS.FI_dist. <= quantile(matri_full$BM_FS.FI_dist., 0.5, na.rm = TRUE))["BM_FS.FI_dist."])), digits = 3)
  three <- round(mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. > quantile(matri_full$BM_FS.FI_dist., 0.5, na.rm = TRUE) & BM_FS.FI_dist. <= quantile(matri_full$BM_FS.FI_dist., 0.75, na.rm = TRUE))["BM_FS.FI_dist."])), digits = 3)
  four <- round(mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. > quantile(matri_full$BM_FS.FI_dist., 0.75, na.rm = TRUE))["BM_FS.FI_dist."])), digits = 3)
}else {
  one <- 0.47
  two <- 0.7
  three <- 0.83
  four <- 0.98
}

val_xmax <- as.Date(paste0(as.character(choice_date + 1), "-01-01"), origin = "1970-01-01")

for (i in c(1:length(unique(bm_fs_fi_dist_stat$Site)))) {

  df1 <- dplyr::filter(bm_fs_fi_dist_stat, bm_fs_fi_dist_stat$Site == unique(bm_fs_fi_dist_stat$Site)[i])

  bm_fs_fi_plot <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = bm_fs_fi_dist_stat$Date, y = bm_fs_fi_dist_stat$BM_FS.FI_dist.med), col = "grey") +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = - 0.1, ymax = one, fill = "blue"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = one, ymax = two, fill = "green"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = two, ymax = three, fill = "yellow"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = three, ymax = four, fill = "orange"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = four, ymax = 1.1, fill = "red"), alpha = 0.3) +
  ggplot2::scale_fill_manual(values = c("#FF0000", "#F59404", "#18E125", "#1A1AE8", "#FAFA15")) +
  ggplot2::geom_pointrange(ggplot2::aes(x = df1$Date, y = df1$BM_FS.FI_dist.med, ymin = df1$BM_FS.FI_dist.min, ymax =  df1$BM_FS.FI_dist.max), col = "black") +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Coef dissim BM FS-FI") +
  ggplot2::ggtitle(unique(df1$Site_bis)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none")

ggplot2::ggsave(paste0("fs_fi_", df1$Site, ".png"), device = "png", bm_fs_fi_plot, height = 3, width = 3.5)

}

rm(df1, four, i, one, three, two, xmax_, xmin_, ymax_, ymin_)


## BM.BF_FS_dist => mobile boulder vs fixed boulder upper faces

# Stats

bm_bf_fs_dist_stat <- matri_full %>% dplyr::group_by(Site, Site_bis, Date, Year, Month, Day) %>% dplyr::summarize(BM.BF_FS_dist.moy = mean(BM.BF_FS_dist., na.rm = TRUE), BM.BF_FS_dist.et = sd(BM.BF_FS_dist., na.rm = TRUE), BM.BF_FS_dist.med = median(BM.BF_FS_dist., na.rm = TRUE), BM.BF_FS_dist.min = min(BM.BF_FS_dist., na.rm = TRUE), BM.BF_FS_dist.max = max(BM.BF_FS_dist., na.rm = TRUE), nb. = dplyr::n(), nb.notNa = sum(!is.na(BM.BF_FS_dist.)))

bm_bf_fs_dist_stat <- dplyr::ungroup(bm_bf_fs_dist_stat)

# Quality scale based on quartiles

if (choice == "N") {
  one <- round(mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. <= quantile(matri_full$BM.BF_FS_dist., 0.25, na.rm = TRUE))["BM.BF_FS_dist."])), digits = 3)
  two <- round(mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. > quantile(matri_full$BM.BF_FS_dist., 0.25, na.rm = TRUE) & BM.BF_FS_dist. <= quantile(matri_full$BM.BF_FS_dist., 0.5, na.rm = TRUE))["BM.BF_FS_dist."])), digits = 3)
  three <- round(mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. > quantile(matri_full$BM.BF_FS_dist., 0.5, na.rm = TRUE) & BM.BF_FS_dist. <= quantile(matri_full$BM.BF_FS_dist., 0.75, na.rm = TRUE))["BM.BF_FS_dist."])), digits = 3)
  four <- round(mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. > quantile(matri_full$BM.BF_FS_dist., 0.75, na.rm = TRUE))["BM.BF_FS_dist."])), digits = 3)

}else {
  one <- 0.19
  two <- 0.32
  three <- 0.455
  four <- 0.735
}
# Plot

for (i in c(1:length(unique(bm_bf_fs_dist_stat$Site)))) {

  df1 <- dplyr::filter(bm_bf_fs_dist_stat, bm_bf_fs_dist_stat$Site == unique(bm_bf_fs_dist_stat$Site)[i])

  bm_bf_fs_plot <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = bm_bf_fs_dist_stat$Date, y = bm_bf_fs_dist_stat$BM.BF_FS_dist.med), col = "grey") +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = - 0.1, ymax = one, fill = "red"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = one, ymax = two, fill = "orange"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = two, ymax = three, fill = "yellow"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = three, ymax = four, fill = "green"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = four, ymax = 1.1, fill = "blue"), alpha = 0.3) +
  ggplot2::scale_fill_manual(values = c("#FF0000", "#F59404", "#18E125", "#1A1AE8", "#FAFA15")) +
  ggplot2::geom_pointrange(ggplot2::aes(x = df1$Date, y = df1$BM.BF_FS_dist.med, ymin = df1$BM.BF_FS_dist.min, ymax =  df1$BM.BF_FS_dist.max), col = "black") +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Coef dissim BM-BF FS") +
  ggplot2::ggtitle(unique(df1$Site_bis)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none")

ggplot2::ggsave(paste0("bm_bf_", df1$Site, ".png"), device = "png", bm_bf_fs_plot, height = 3, width = 3.5)

}

rm(df1, four, i, one, three, two, xmax_, xmin_, ymax_, ymin_)

write.table(bm_bf_fs_dist_stat, "Valeurs_stat.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")

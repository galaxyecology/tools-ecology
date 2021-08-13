#Rscript

#########################
##    Beta diversity   ##
#########################

#####Packages : ggplot2
#               vegan
#               adespatial
#               dplyr
#               tibble
#               tdyr

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
    stop("This tool needs at least 2 arguments")
}else{
    table <- args[1]
    hr <- args[2]
    abund <- as.numeric(args[3])
    loc <- as.numeric(args[4])
    spe <- as.numeric(args[5])
    date <- as.numeric(args[6])
    map <- as.logical(args[7])
    sepa <- as.logical(args[8])
    not <- as.logical(args[9])
    lat <- as.numeric(args[10])
    long <- as.numeric(args[11])
    var <- as.numeric(args[12])
    source(args[13])
}

if (hr == "false") {
  hr <- FALSE
}else{
  hr <- TRUE
}

#####Import data
data <- read.table(table, sep = "\t", dec = ".", header = hr, fill = TRUE, encoding = "UTF-8")
colabund <- colnames(data)[abund]
colloc <- colnames(data)[loc]
if (map) {
  collat <- colnames(data)[lat]
  collong <- colnames(data)[long]
}
colspe <- colnames(data)[spe]
coldate <- colnames(data)[date]
data[, coldate] <- as.factor(data[, coldate])

data <- data[grep("^$", data[, spe], invert = TRUE), ]

if (sepa) {
colvar <- colnames(data)[var]
}

# Data for species
data_num <- make_table_analyse(data, colabund, colspe, colloc, coldate)
nb_spe <- length(unique(data[, spe]))
nb_col <- ncol(data_num) - nb_spe + 1

#Data with coordinates and environmental
if (map) {
  data_xy <- data_num[, c(collat, collong)]
  colnames(data_xy) <- c("latitude", "longitude")
  # Data for environment
  data_env <- data_num[, c(colloc, collat, collong)]
  colnames(data_env) <- c("site", "latitude", "longitude")
}

# Data with only species and their abundance
data_spe <- data_num[, nb_col:ncol(data_num)]
rownames(data_spe) <- paste0(data_num[, colloc], " - ", data_num[, coldate])

#####Your analysis

# Computation beta.div {adespatial}
# Beta.div on Hellinger-transformed species data
data_beta <- adespatial::beta.div(data_spe, method = "hellinger", nperm = 9999)

save(data_beta, file = "beta_diversity.Rdata")
cat("##############################################################################",
    "\n########################### Beta Diversity Summary ###########################",
    "\n##############################################################################",
    "\n\n### All data ###",
    "\nBeta diversity: ", data_beta$beta[[2]],
    "\nSum of Squares: ", data_beta$beta[[1]], 
    "\n\n### Vector of Local Contributions to Beta Diversity (LCBD) for the sites each date ###", 
    "\n", capture.output(data_beta$LCBD), 
    "\n\n### Vector of P-values associated with the LCBD indices ###",
    "\n", capture.output(data_beta$p.LCBD), 
    "\n\n### Vector of Corrected P-values for the LCBD indices, Holm correction ###",
    "\n", capture.output(data_beta$p.adj), 
    "\n\n### Vector of Species contributions to beta diversity (SCBD) ###",
    "\n", capture.output(data_beta$SCBD), file = "LCBD.txt", fill = 1, append = TRUE)

# Which species have a SCBD larger than the mean SCBD?
scbd <- capture.output(data_beta$SCBD[data_beta$SCBD >= mean(data_beta$SCBD)])
write(scbd, "SCBD.txt")

##1st fonction
beta_div_ext <- function(data_beta, data_xy, data_env) {
   data_beta_ext <- data.frame(data_xy, data_env, LCBD = data_beta$LCBD * 100, p.LCBD = data_beta$p.LCBD, signif = data_beta$p.LCBD < 0.05)

  graph_beta_ext <- ggplot2::ggplot(data = data_beta_ext, ggplot2::aes(x = latitude, y = longitude, size = LCBD, col = signif)) +
  ggplot2::geom_point() +
  ggplot2::scale_colour_manual(values = c("#57bce0", "#ce0b0b"), labels = c("Non significant", "Significant"), name = "Significance at 0.05") +
  ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

  ggplot2::ggsave("Beta_diversity_through_space.png", graph_beta_ext)
}

## Boyé et al. 2017 JSR Fig R
####################################################

####LCBD####
lcbd_site <- adespatial::beta.div(data_spe, "hellinger", nperm = 999)

compute_lcbd <- function(data_beta, data_spe, data_num) {

#############
  mat_lcbd_site <- data.frame(data_spe, LCBD = data_beta$LCBD * 100, p.LCBD = data_beta$p.LCBD, signif = data_beta$p.LCBD < 0.05, site = data_num[, colloc], date = data_num[, coldate])

## Map spatio-temp
##################
  p1 <- ggplot2::qplot(date, site, size = LCBD, col = signif, data = mat_lcbd_site)
  p1 <- p1 + ggplot2::scale_colour_manual(values = c("#57bce0", "#ce0b0b"), labels = c("Non significant", "Significant"), name = "Significance at 0.05")
  p1 <- p1 + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) + ggplot2::xlab("Date") + ggplot2::ylab("Site")

  ggplot2::ggsave("LCBD_sites_time.png", p1)


## Par années
#############
  mean_time <- tapply(mat_lcbd_site$LCBD, mat_lcbd_site$date, mean)
  sd_time <- tapply(mat_lcbd_site$LCBD, mat_lcbd_site$date, sd)
  date <- unique(mat_lcbd_site$date)

  data <- data.frame(date, mean_time, sd_time)

  time <- ggplot2::ggplot() + ggplot2::geom_pointrange(ggplot2::aes(x = date, y = mean_time, ymin = mean_time - sd_time, ymax = mean_time + sd_time), data = data)
  time <- time + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), axis.line.y = ggplot2::element_line(size = 0.5)) + ggplot2::ylab("mean LCBD")

  ggplot2::ggsave("Mean_LCBD_through_time.png", time)
}

## Choose another graph
#######################
compute_lcbd2 <- function(data_beta, data_spe, data_num) {

#############
  mat_lcbd_site <- data.frame(data_spe, LCBD = data_beta$LCBD * 100, p.LCBD = data_beta$p.LCBD, signif = data_beta$p.LCBD < 0.05, site = data_num[, colloc], date = data_num[, coldate], variable = data_num[, colvar])

  p1 <- ggplot2::qplot(date, variable, size = LCBD, col = signif, data = mat_lcbd_site)
  p1 <- p1 + ggplot2::scale_colour_manual(values = c("#57bce0", "#ce0b0b"), labels = c("Non significant", "Significant"), name = "Significance at 0.05")
  p1 <- p1 + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) + ggplot2::xlab("Date") + ggplot2::ylab(colvar)

  ggplot2::ggsave(paste0("LCBD_per_", colvar, "_through_time.png"), p1)
}

####SCBD###
# Function to compute SCBD
library(dplyr)
make_scbd_uvc <- function(data_spe, z, data_beta) {
  # Computation using beta.div {adespatial} on
  # Hellinger-transformed species data

  # Which species have a SCBD larger than the mean SCBD?
  spe_scbd <-  data_beta$SCBD[data_beta$SCBD >= mean(data_beta$SCBD)] %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Taxon") %>%
    dplyr::mutate("Methode" = z)

  return(spe_scbd)
}

# Function to make a radar plot

coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggplot2::ggproto("CordRadar", ggplot2::coord_polar(theta = theta, start = start,
          direction = sign(direction)),
          is_linear = function(coord) TRUE)
}

# Make the radar plot
radar_plot <- function(scbd_uvc_tc) {
  uvc_rd_plot_data <- scbd_uvc_tc %>%
    rename(scbd_score = ".")

  rad_uvc <- ggplot2::ggplot(uvc_rd_plot_data, ggplot2::aes(x = Taxon, y = scbd_score, group = Methode)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 3) +
    coord_radar() +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
        legend.position = "bottom")

  ggplot2::ggsave("SCBD_Species_Radar_plot.png", rad_uvc)
}

## LCBD

if (map) {
  #Beta diversity
  beta_div_ext(data_beta, data_xy, data_env)
}

#Lcbd per places and time
compute_lcbd(data_beta, data_spe, data_num)

#Lcbd of your choice
if (sepa) {
  compute_lcbd2(data_beta, data_spe, data_num)
}

##SCBD

scbd_uvc_tc <- make_scbd_uvc(data_spe, z = "TC", data_beta)

radar_plot(scbd_uvc_tc)

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
    lat <- as.numeric(args[5])
    long <- as.numeric(args[6])
    spe <- as.numeric(args[7])
    date <- as.numeric(args[8])
    sepa <- as.logical(args[9])
    not <- as.logical(args[10])
    var <- as.numeric(args[11])
    source(args[12])
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
if (!is.na(lat)) {
  collat <- colnames(data)[lat]
  collong <- colnames(data)[long]
}
colspe <- colnames(data)[spe]
coldate <- colnames(data)[date]

data <- data[grep("^$", data[, spe], invert = TRUE), ]

if (sepa) {
colvar <- colnames(data)[var]
}

# Data for species
data_num <- make_table_analyse(data, colabund, colspe, colloc, coldate)
nb_spe <- length(unique(data[, spe]))
nb_col <- ncol(data_num) - nb_spe
data_spe <- data_num[, nb_col:ncol(data_num)]

#Data with coordinates
if (!is.na(lat)) {
  data_xy <- data[, c(lat, long, date, loc)]
  colnames(data_xy) <- c("latitude", "longitude", "date", "site")
  data_xy <- unique(data_xy)
  data_xy$longlat <- paste(data_xy$latitude, data_xy$longitude)
  if (length(unique(data_xy$site)) != length(unique(data_xy$longlat))) {
  stop("There is a different number of sites and coordinates, please select the site column matching your coordinates, we need one site name per pair of coordinates")
  }
}else{
  data_xy <- data[, c(date, loc)]
  colnames(data_xy) <- c("date", "site")
  data_xy <- unique(data_xy)
}


data_full <- cbind(data_xy[match(data_spe[, 1], data_xy[, "site"]),], data_spe)

if (sepa) {
  data_meth <- data[, c(var, date, loc)]
  colnames(data_meth) <- c(colvar, "date", "site")
  data_meth <- unique(data_meth)
  data_full2 <- cbind(data_meth[match(data_spe[, 1], data_meth[, "site"]),], data_spe)
}

if (!is.na(lat)) {
  data_xy <- data_full[, c("latitude", "longitude")]
  # Data for environment
  data_env <- data_full[, c("site", "latitude", "longitude")]
}

# Data with only species and their abundance
data_spe <- data_spe[, -1]

#####Your analysis 

# Computation beta.div {adespatial}
# Beta.div on Hellinger-transformed species data
data_beta <- adespatial::beta.div(data_spe, method = "hellinger", nperm = 9999)

beta <- capture.output(summary(data_beta))
write(beta, "LCBD.txt")

# Which species have a SCBD larger than the mean SCBD?
scbd <- capture.output(data_beta$SCBD[data_beta$SCBD >= mean(data_beta$SCBD)])
write(scbd, "SCBD.txt")

##1st fonction 
beta_div_ext <- function(data_beta, data_xy, data_env) {
   data_beta_ext <- data.frame(data_xy, data_env, LCBD = data_beta$LCBD * 100, p.LCBD = data_beta$p.LCBD, signif = data_beta$p.LCBD < 0.05)

  graph_beta_ext <- ggplot2::ggplot(data = data_beta_ext, ggplot2::aes(x = latitude, y = longitude, size = LCBD, col = signif)) +
	ggplot2::geom_point() +
	ggplot2::scale_colour_manual(values = c("grey56", "black"), labels = c("Non significant", "Significant"), name = "Significance at 0.05") +
	ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")

  ggplot2::ggsave("Beta_diversity_ext.png", graph_beta_ext)
}

## Boyé et al. 2017 JSR Fig R
####################################################

####LCBD####
compute_lcbd <- function(data_spe, data_full) {
  lcbd_site <- adespatial::beta.div(data_spe, "hellinger", nperm = 999)

#############
  mat_lcbd_site <- data.frame(data_spe, LCBD = lcbd_site$LCBD * 100, p.LCBD = lcbd_site$p.LCBD, signif = lcbd_site$p.LCBD < 0.05, site = data_full$site, date = data_full$date)

## Map spatio-temp
##################
  p1 <- ggplot2::qplot(date, site, size = LCBD, col = signif, data = mat_lcbd_site)
  p1 <- p1 + ggplot2::scale_colour_manual(values = c("grey56", "black"), labels = c("Non significant", "Significant"), name = "Significance at 0.05")
  p1 <- p1 + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) + ggplot2::xlab("Date") + ggplot2::ylab("Site")

  ggplot2::ggsave("Spatio_temp.png", p1)


## Par années
#############
  mean_time <- tapply(mat_lcbd_site$LCBD, mat_lcbd_site$date, mean)
  sd_time <- tapply(mat_lcbd_site$LCBD, mat_lcbd_site$date, sd)
  year <- unique(mat_lcbd_site$date)

  data <- data.frame(year, mean_time, sd_time)

  time <- ggplot2::ggplot() + ggplot2::geom_pointrange(ggplot2::aes(x = year, y = mean_time, ymin = mean_time - sd_time, ymax = mean_time + sd_time), data = data)
  time <- time + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90), axis.line.y = ggplot2::element_line(size = 0.5)) + ggplot2::ylab("mean LCBD")
 # time <- time + ggplot2::scale_y_continuous(limits = c(0.9, 1.6))

  ggplot2::ggsave("Time.png", time)
}

## Choose another graph
#######################
compute_lcbd2 <- function(data_spe, data_full) {
  lcbd_site <- adespatial::beta.div(data_spe, "hellinger", nperm = 999)

#############
  mat_lcbd_site <- data.frame(data_spe, LCBD = lcbd_site$LCBD * 100, p.LCBD = lcbd_site$p.LCBD, signif = lcbd_site$p.LCBD < 0.05, site = data_full$site, date = data_full$date, variable = data_full[, colvar])

  p1 <- ggplot2::qplot(date, variable, size = LCBD, col = signif, data = mat_lcbd_site)
  p1 <- p1 + ggplot2::scale_colour_manual(values = c("grey56", "black"), labels = c("Non significant", "Significant"), name = "Significance at 0.05")
  p1 <- p1 + ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) + ggplot2::xlab("Date") + ggplot2::ylab(colvar)

  ggplot2::ggsave(paste0(colvar, ".png"), p1)
}

####SCBD###
# Function to compute SCBD
library(dplyr)
make_scbd_uvc <- function(data_spe, data, z) {
  # Computation using beta.div {adespatial} on
  # Hellinger-transformed species data
  spe_beta <- adespatial::beta.div(data_spe, method = "hellinger", nperm = 9999)

  # Which species have a SCBD larger than the mean SCBD?
  spe_scbd <-  spe_beta$SCBD[spe_beta$SCBD >= mean(spe_beta$SCBD)] %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "species") %>%
    dplyr::mutate("Methode" = z)

  return(spe_scbd)
}

# Function to make a radar plot

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggplot2::ggproto("CordRadar", ggplot2::coord_polar(theta = theta, start = start,
          direction = sign(direction)),
          is_linear = function(coord) TRUE)
}

# Make the radar plot
radar_plot <- function(scbd_UVC_TC) {
  UVC_rd_plot_data <- scbd_UVC_TC %>%
    rename(scbd_score = ".")

  rad_uvc <- ggplot2::ggplot(UVC_rd_plot_data, ggplot2::aes(x = species, y = scbd_score, group = Methode)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 3) +
    coord_radar() +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 10),
        legend.position = "bottom")

  ggplot2::ggsave("Radar_plot.png", rad_uvc)
}

## LCBD

if (!is.na(lat)) {
  #Beta diversity
  beta_div_ext(data_beta, data_xy, data_env)
}

#Lcbd per places and time
compute_lcbd(data_spe, data_full)

#Lcbd of your choice
if (sepa) {
  compute_lcbd2(data_spe, data_full2)
}

##SCBD

scbd_UVC_TC <- make_scbd_uvc(data_spe, data_full, z = "TC")

radar_plot(scbd_UVC_TC)

#Rscript

###########################################
##    Mapping alpha and beta diversity   ##
###########################################

#####Packages : stars
#               utils
#               biodivmapr
#               raster
#               sf
#               mapview
#               leafpop
#               RColorBrewer
#               labdsv
#               rgdal
#               ggplot2
#               gridExtra
remotes::install_github("jbferet/biodivMapR")
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import the S2 data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    data_raster <- args[1]
    rasterheader <- args[2]
    data <- args[3]
    plots_zip <- args[4]
    choice <- as.character(args[5])
    source(args[6])
    # type of PCA:
    # PCA: no rescaling of the data
    # SPCA: rescaling of the data
    typepca <- as.character(args[7])
}

################################################################################
##              DEFINE PARAMETERS FOR DATASET TO BE PROCESSED                 ##
################################################################################
if (data_raster == "") {
  #Create a directory where to unzip your folder of data
  dir.create("data_dir")
  unzip(data, exdir = "data_dir")
  # Path to raster
  data_raster <- list.files("data_dir/results/Reflectance", pattern = "_Refl")
  input_image_file <- file.path("data_dir/results/Reflectance", data_raster[1])
  input_header_file <- file.path("data_dir/results/Reflectance", data_raster[2])

} else {
  input_image_file <- file.path(getwd(), data_raster, fsep = "/")
  input_header_file <- file.path(getwd(), rasterheader, fsep = "/")
}

################################################################################
##                              PROCESS IMAGE                                 ##
################################################################################
# 1- Filter data in order to discard non vegetated / shaded / cloudy pixels

print("PERFORM PCA ON RASTER")
pca_output <- biodivMapR::perform_PCA(Input_Image_File = input_image_file, Input_Mask_File = input_mask_file,
                          Output_Dir = output_dir, TypePCA = typepca, FilterPCA = filterpca, nbCPU = nbcpu, MaxRAM = maxram)

pca_files <- pca_output$PCA_Files
pix_per_partition <- pca_output$Pix_Per_Partition
nb_partitions <- pca_output$nb_partitions
# path for the updated mask
input_mask_file <- pca_output$MaskPath

# 3- Select principal components from the PCA raster
# Select components from the PCA/SPCA/MNF raster
sel_compo <- c("1\n", "2\n", "3\n", "4\n", "5\n", "6\n", "7\n", "8")
image_name <- tools::file_path_sans_ext(basename(input_image_file))
output_dir_full <- file.path(output_dir, image_name, typepca, "PCA")

write.table(sel_compo, paste0(output_dir_full, "/Selected_Components.txt"))
sel_pc <-  file.path(output_dir_full, "Selected_Components.txt")


################################################################################
##                      MAP ALPHA AND BETA DIVERSITY                          ##
################################################################################
print("MAP SPECTRAL SPECIES")

kmeans_info <- biodivMapR::map_spectral_species(Input_Image_File = input_image_file, Output_Dir = output_dir, PCA_Files = pca_files, Input_Mask_File = input_mask_file, Pix_Per_Partition = pix_per_partition, nb_partitions = nb_partitions, nbCPU = nbcpu, MaxRAM = maxram, nbclusters = nbclusters, TypePCA = typepca)

################################################################################
##          COMPUTE ALPHA AND BETA DIVERSITY FROM FIELD PLOTS                 ##
################################################################################
## read selected features from dimensionality reduction
selected_features <- read.table(sel_pc)[[1]]
## path for selected components

# location of the directory where shapefiles used for validation are saved
dir.create("VectorDir")
unzip(plots_zip, exdir = "VectorDir")

# list vector data
path_vector <- biodivMapR::list_shp("VectorDir")
name_vector <- tools::file_path_sans_ext(basename(path_vector))

# location of the spectral species raster needed for validation
path_spectralspecies <- kmeans_info$SpectralSpecies
# get diversity indicators corresponding to shapefiles (no partitioning of spectral dibversity based on field plots so far...)

biodiv_indicators <- biodivMapR::diversity_from_plots(Raster_SpectralSpecies = path_spectralspecies, Plots = path_vector, nbclusters = nbclusters, Raster_Functional = pca_files, Selected_Features = selected_features)

shannon_rs <- c(biodiv_indicators$Shannon)[[1]]
fric <- c(biodiv_indicators$FunctionalDiversity$FRic)
feve <- c(biodiv_indicators$FunctionalDiversity$FEve)
fdiv <- c(biodiv_indicators$FunctionalDiversity$FDiv)
# if no name for plots
biodiv_indicators$Name_Plot <- seq(1, length(biodiv_indicators$Shannon[[1]]), by = 1)


####################################################
# write RS indicators                              #
####################################################
# write a table for Shannon index

# write a table for all spectral diversity indices corresponding to alpha diversity
results <- data.frame(name_vector, biodiv_indicators$Richness, biodiv_indicators$Fisher,
                      biodiv_indicators$Shannon, biodiv_indicators$Simpson,
                      biodiv_indicators$FunctionalDiversity$FRic,
                      biodiv_indicators$FunctionalDiversity$FEve,
                      biodiv_indicators$FunctionalDiversity$FDiv)

names(results) <- c("ID_Plot", "Species_Richness", "Fisher", "Shannon", "Simpson", "fric", "feve", "fdiv")
write.table(results, file = "Diversity.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)

if (choice == "Y") {
# write a table for Bray Curtis dissimilarity
bc_mean <- biodiv_indicators$BCdiss
bray_curtis <- data.frame(name_vector, bc_mean)
colnames(bray_curtis) <- c("ID_Plot", bray_curtis[, 1])
write.table(bray_curtis, file = "BrayCurtis.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)

####################################################
# illustrate results
####################################################
# apply ordination using PCoA (same as done for map_beta_div)

mat_bc_dist <- as.dist(bc_mean, diag = FALSE, upper = FALSE)
betapco <- labdsv::pco(mat_bc_dist, k = 3)

# assign a type of vegetation to each plot, assuming that the type of vegetation
# is defined by the name of the shapefile

nbsamples <- shpname <- c()
for (i in 1:seq_along(path_vector)) {
  shp <- path_vector[i]
  nbsamples[i] <- length(rgdal::readOGR(shp, verbose = FALSE))
  shpname[i] <- tools::file_path_sans_ext(basename(shp))
}

type_vegetation <- c()
for (i in 1: seq_along(nbsamples)) {
  for (j in 1:nbsamples[i]) {
    type_vegetation <- c(type_vegetation, shpname[i])
  }
}

#data frame including a selection of alpha diversity metrics and beta diversity expressed as coordinates in the PCoA space
results <- data.frame("vgtype" = type_vegetation, "pco1" = betapco$points[, 1], "pco2" = betapco$points[, 2], "pco3" = betapco$points[, 3], "shannon" = shannon_rs, "fric" = fric, "feve" = feve, "fdiv" = fdiv)

#plot field data in the PCoA space, with size corresponding to shannon index
g1 <- ggplot2::ggplot(results, ggplot2::aes(x = pco1, y = pco2, color = vgtype, size = shannon)) + ggplot2::geom_point(alpha = 0.6) + ggplot2::scale_color_manual(values = c("#e6140a", "#e6d214", "#e68214", "#145ae6"))

g2 <- ggplot2::ggplot(results, ggplot2::aes(x = pco1, y = pco3, color = vgtype, size = shannon)) + ggplot2::geom_point(alpha = 0.6) + ggplot2::scale_color_manual(values = c("#e6140a", "#e6d214", "#e68214", "#145ae6"))

g3 <- ggplot2::ggplot(results, ggplot2::aes(x = pco2, y = pco3, color = vgtype, size = shannon)) + ggplot2::geom_point(alpha = 0.6) + ggplot2::scale_color_manual(values = c("#e6140a", "#e6d214", "#e68214", "#145ae6"))

#extract legend
get_legend <- function(a_gplot) {
    tmp <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a_gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}

legend <- get_legend(g3)
gall <- gridExtra::grid.arrange(gridExtra::arrangeGrob(g1 + ggplot2::theme(legend.position = "none"), g2 + ggplot2::theme(legend.position = "none"), g3 + ggplot2::theme(legend.position = "none"), nrow = 1), legend, nrow = 2, heights = c(3, 2))


filename <- ggplot2::ggsave("BetaDiversity_PcoA1_vs_PcoA2_vs_PcoA3.png", gall, scale = 0.65, width = 12, height = 9, units = "in", dpi = 200, limitsize = TRUE)

filename
}

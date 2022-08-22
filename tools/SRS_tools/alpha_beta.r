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
    # type of PCA:
    # PCA: no rescaling of the data
    # SPCA: rescaling of the data
    typepca <- as.character(args[4])
    alpha <- as.logical(args[5])
    beta <- as.logical(args[6])
    funct <- as.logical(args[7])
    all <- as.logical(args[8])
    source(args[9])
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

if (alpha == TRUE || beta == TRUE || all == TRUE) {
## alpha
  print("MAP ALPHA DIVERSITY")
  index_alpha <- c("Shannon")
  alpha_div <- biodivMapR::map_alpha_div(Input_Image_File = input_image_file, Output_Dir = output_dir, TypePCA = typepca, window_size = window_size, nbCPU = nbcpu, MaxRAM = maxram, Index_Alpha = index_alpha, nbclusters = nbclusters)

  alpha_path <- file.path(output_dir, image_name, typepca, "ALPHA", "Shannon_10_Fullres.zip")
  alpha_raster <- raster::raster(alpha_path)
  get_alpha <- convert_raster(alpha_raster)

 if (alpha == TRUE || all == TRUE) {
   colnames(get_alpha) <- c("Alpha", "longitude", "latitude")
   plot_indices(get_alpha, titre = "Alpha")

   write.table(get_alpha, file = "alpha.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)
}
  if (beta == TRUE || all == TRUE) {
## beta
  print("MAP BETA DIVERSITY")
  beta_div <- biodivMapR::map_beta_div(Input_Image_File = input_image_file, Output_Dir = output_dir, TypePCA = typepca, window_size = window_size, nb_partitions = nb_partitions, nbCPU = nbcpu, MaxRAM = maxram, nbclusters = nbclusters)

  beta_path <- file.path(output_dir, image_name, typepca, "BETA", "BetaDiversity_BCdiss_PCO_10")
  beta_raster <- raster::raster(beta_path)
  get_beta <- convert_raster(beta_raster)

  colnames(get_beta) <- c("Beta", "longitude", "latitude")
  plot_indices(get_beta, titre = "Beta")

  write.table(get_beta, file = "beta.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)
  }
}


################################################################################
##          COMPUTE ALPHA AND BETA DIVERSITY FROM FIELD PLOTS                 ##
################################################################################
## read selected features from dimensionality reduction
selected_features <- read.table(sel_pc)[[1]]
## path for selected components

if (funct == TRUE || all == TRUE) {
mapper <- biodivMapR::map_functional_div(Original_Image_File = input_image_file, Functional_File = pca_files,  Selected_Features = selected_features, Output_Dir = output_dir, window_size = window_size, nbCPU = nbcpu, MaxRAM = maxram, TypePCA = typepca)

funct_path <- file.path(output_dir, image_name, typepca, "FUNCTIONAL", "FunctionalDiversity_Map_MeanFilter_Fullres.zip")
funct_raster <- raster::raster(funct_path)
get_funct <- convert_raster(funct_raster)

colnames(get_funct) <- c("Functionnal", "longitude", "latitude")
plot_indices(get_funct, titre = "Functionnal")

write.table(get_funct, file = "Functionnal.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)
}

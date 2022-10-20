#Rscript

###########################################
##    Getting PCA raster   ##
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
## remotes::install_github("jbferet/biodivMapR")
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import the S2 data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    data_raster <- args[1]
    rasterheader <- args[2]
    data <- args[3]
    typepca <- as.character(args[4])
    source(args[5])
}

################################################################################
##              DEFINE PARAMETERS FOR DATASET TO BE PROCESSED                 ##
################################################################################
# expected to be in ENVI HDR

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


pca_path <- file.path(output_dir, basename(data_raster), typepca, "PCA", "OutputPCA_8_PCs")
pca_raster <- raster::raster(pca_path)
get_pca <- convert_raster(pca_raster)

colnames(get_pca) <- c("PCA", "longitude", "latitude")
plot_indices(get_pca, titre = "PCA")

write.table(get_pca, file = "PCA.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)
#### Get the raster layer files
pca_files <- file.path("RESULTS", basename(data_raster), typepca, "PCA")
to_dir_short <- output_dir
file.copy(pca_files, to_dir_short) #copy files from long to short paths

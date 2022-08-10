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
    type <- as.character(args[4])
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

# path for the Mask raster corresponding to image to process
# expected to be in ENVI HDR format, 1 band, integer 8bits
# expected values in the raster: 0 = masked, 1 = selected
# set to FALSE if no mask available
input_mask_file <- FALSE

# relative or absolute path for the Directory where results will be stored
# For each image processed, a subdirectory will be created after its name
output_dir <- "RESULTS"

# SPATIAL RESOLUTION
# resolution of spatial units for alpha and beta diversity maps (in pixels), relative to original image
# if Res.Map = 10 for images with 10 m spatial resolution, then spatial units will be 10 pixels x 10m = 100m x 100m surfaces
# rule of thumb: spatial units between 0.25 and 4 ha usually match with ground data
# too small window_size results in low number of pixels per spatial unit, hence limited range of variation of diversity in the image
window_size <- 10

# PCA FILTERING: Set to TRUE if you want second filtering based on PCA outliers to be processed. Slower
filterpca <- TRUE

# type of PCA:
# PCA: no rescaling of the data
# SPCA: rescaling of the data
typepca <-type


################################################################################
##                    DEFINE PARAMETERS FOR METHOD                            ##
################################################################################
nbcpu <- 4
maxram <- 0.5
nbclusters <- 50

################################################################################
##                              PROCESS IMAGE                                 ##
################################################################################
# 1- Filter data in order to discard non vegetated / shaded / cloudy pixels
ndvi_thresh <- 0.5
blue_thresh <- 500
nir_thresh  <- 1500
continuum_removal <- TRUE

print("PERFORM PCA ON RASTER")
pca_output <- biodivMapR::perform_PCA(Input_Image_File = input_image_file, Input_Mask_File = input_mask_file,
                          Output_Dir = output_dir, TypePCA = typepca, FilterPCA = filterpca, nbCPU = nbcpu, MaxRAM = maxram)


pca_path <- file.path(output_dir, basename(data_raster), type, "PCA", "OutputPCA_8_PCs")
pca_raster <- raster::raster(pca_path)
get_pca <- convert_raster(pca_raster)

colnames(get_pca) <- c("PCA", "longitude", "latitude")
plot_indices(get_pca, titre = "PCA")

write.table(get_pca, file = "PCA.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)

pca_files <- file.path("RESULTS", basename(data_raster), type, "PCA")
pca_raster <- list.files(pca_files, pattern = "Output")
data_raster <- file.path(pca_files, pca_raster[1])
data_header <- file.path(pca_files, pca_raster[2])
zip_raster <- file.path("PCA.zip")
zip_header <- file.path("PCA_header.zip")
zip::zip(zip_raster, data_raster)
zip::zip(zip_header, data_header)

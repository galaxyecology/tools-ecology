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
remotes::install_github('jbferet/biodivMapR')
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import the S2 data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else{
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
  data_raster <-list.files("data_dir/results/Reflectance", pattern = "_Refl")
  Input_Image_File <- file.path("data_dir/results/Reflectance", data_raster[1])
  Input_Header_File <- file.path("data_dir/results/Reflectance", data_raster[2])

} else{
  Input_Image_File <- file.path(getwd(), data_raster, fsep = "/")
  Input_Header_File <- file.path(getwd(), rasterheader, fsep = "/")
}

# path for the Mask raster corresponding to image to process
# expected to be in ENVI HDR format, 1 band, integer 8bits
# expected values in the raster: 0 = masked, 1 = selected
# set to FALSE if no mask available
Input_Mask_File <- FALSE

# relative or absolute path for the Directory where results will be stored
# For each image processed, a subdirectory will be created after its name
Output_Dir  = 'RESULTS'

# SPATIAL RESOLUTION
# resolution of spatial units for alpha and beta diversity maps (in pixels), relative to original image
# if Res.Map = 10 for images with 10 m spatial resolution, then spatial units will be 10 pixels x 10m = 100m x 100m surfaces
# rule of thumb: spatial units between 0.25 and 4 ha usually match with ground data
# too small window_size results in low number of pixels per spatial unit, hence limited range of variation of diversity in the image
window_size <- 6

# PCA FILTERING: Set to TRUE if you want second filtering based on PCA outliers to be processed. Slower
FilterPCA <- TRUE

# type of PCA:
# PCA: no rescaling of the data
# SPCA: rescaling of the data
TypePCA <- type


################################################################################
##                    DEFINE PARAMETERS FOR METHOD                            ##
################################################################################
nbCPU <- 4
MaxRAM <- 0.5
nbclusters <- 50

################################################################################
##                              PROCESS IMAGE                                 ##
################################################################################
# 1- Filter data in order to discard non vegetated / shaded / cloudy pixels
NDVI_Thresh <- 0.5
Blue_Thresh <- 500
NIR_Thresh  <- 1500
Continuum_Removal <- TRUE

print("PERFORM PCA ON RASTER")
PCA_Output <- biodivMapR::perform_PCA(Input_Image_File = Input_Image_File, Input_Mask_File = Input_Mask_File,
                          Output_Dir = Output_Dir, TypePCA = TypePCA, FilterPCA = FilterPCA, nbCPU = nbCPU, MaxRAM = MaxRAM)

pca_path <- file.path(Output_Dir, basename(data_raster), type, "PCA", "OutputPCA_8_PCs")
pca_raster <- raster::raster(pca_path)
get_pca <- convert_raster(pca_raster)

colnames(get_pca) <- c('PCA', 'longitude', 'latitude')
plot_indices(get_pca, titre = "PCA")

write.table(get_pca, file = "PCA.tabular", sep = "\t", dec = ".", na = " ", row.names = F, col.names = T, quote = FALSE)

#PCA_Files <- PCA_Output$PCA_Files
PCA_Files <- file.path("RESULTS", basename(data_raster), type, "PCA")
PCA_raster <- list.files(PCA_Files, pattern = "Output")
data_raster <- file.path(PCA_Files, PCA_raster[1])
data_header<- file.path(PCA_Files, PCA_raster[2])
zip_raster <- file.path('PCA.zip')
zip_header <- file.path('PCA_header.zip')
zip::zip(zip_raster, data_raster)
zip::zip(zip_header, data_header)


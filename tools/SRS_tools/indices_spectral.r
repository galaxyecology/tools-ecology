#Rscript

###########################################
##    Mapping alpha and beta diversity   ##
###########################################

#####Packages :  expint,
#                pracma,
#                R.utils,
#                raster,
#                sp,
#                matrixStats,
#                ggplot2,
#                expandFunctions,
#                stringr,
#                XML,
#                rgdal,
#                stars,
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import the S2 data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else{
    data_raster <- args[1]
    data_header <- args[2]
    data <- args[3]
    source(args[4])
    source(args[5])
    source(args[6])
    indice_choice <- args[7]
    source(args[8])
    output_raster <- as.character(args[9])

}

########################################################################
##                  COMPUTE SPECTRAL INDEX : NDVI                     ##
########################################################################

if (data != "") {
  #Create a directory where to unzip your folder of data
  dir.create("data_dir")
  unzip(data, exdir = "data_dir")

  # Read raster
  data_raster <-list.files("data_dir/results/Reflectance", pattern = "_Refl")
  data_raster <- file.path("data_dir/results/Reflectance", data_raster[1])
  Refl <- raster::brick(data_raster)
  Refl2 <- raster::raster(data_raster)
} else{
  # Read raster
  Refl <- raster::brick(data_raster)
  Refl2 <- raster::raster(data_raster)
}
# get raster band name and clean format. Expecting band name and wavelength to be documented in image
HDR_Refl <- read_ENVI_header(get_HDR_name(data_raster))
SensorBands <- HDR_Refl$wavelength
# compute a set of spectral indices defined by IndexList from S2 data
IndexList <- indice_choice
# ReflFactor = 10000 when reflectance is coded as INT16
Refl <- raster::aggregate(Refl, fact = 10)
Indices <- ComputeSpectralIndices_Raster(Refl = Refl, SensorBands = SensorBands,
                                                  Sel_Indices = indice_choice,
                                                  ReflFactor = 10000, StackOut=F)

# Convert raster to SpatialPointsDataFrame
Refl2 <- raster::aggregate(Refl2, fact = 10)
r_pts <- convert_raster(Refl2)                       

# create directory for Spectral indices
results_site_path <- "RESULTS"
SI_path <- file.path(results_site_path, 'SpectralIndices')
dir.create(path = SI_path, showWarnings = FALSE, recursive = TRUE)
# Save spectral indices
for (SpIndx in names(Indices$SpectralIndices)) {
  Index_Path <- file.path(SI_path, paste(basename(data_raster), '_', SpIndx, sep = ''))
  spec_indices <- stars::write_stars(stars::st_as_stars(Indices$SpectralIndices[[SpIndx]]), dsn = Index_Path, driver = "ENVI", type = 'Float32')
  
  # write band name in HDR
  HDR <- read_ENVI_header(get_HDR_name(Index_Path))
  HDR$`band names` <- SpIndx
  HDR_name <- write_ENVI_header(HDR = HDR, HDRpath = get_HDR_name(Index_Path))
}

# Get the raster layer of the indice as an output
if (output_raster == "Y") {
raster_zip <- file.path("raster.zip")
zip::zip(raster_zip, Index_Path)

header_zip <- file.path("header.zip")
zip::zip(header_zip, get_HDR_name(Index_Path))
}

  # Writting the tabular and the plot
  spec_indices <- as.data.frame(spec_indices)
  r_pts[, indice_choice] <- spec_indices[, 3]
  plot_indices(data = r_pts, titre = indice_choice)

write.table(r_pts, file = "Spec_Index.tabular", sep = "\t", dec = ".", na = " ", row.names = F, col.names = T, quote = FALSE)

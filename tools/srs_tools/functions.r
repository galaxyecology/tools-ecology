#Rscript

#########################
##      Functions      ##
#########################

#####Packages : raster
#               sp
#               ggplot2

####Set paramaters for all tools using BiodivMapR

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



#### Convert raster to dataframe

# Convert raster to SpatialPointsDataFrame
convert_raster <- function(data_raster) {
r_pts <- raster::rasterToPoints(data_raster, spatial = TRUE)

# reproject sp object
geo_prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
r_pts <- sp::spTransform(r_pts, sp::CRS(geo_prj))


# Assign coordinates to @data slot, display first 6 rows of data.frame
r_pts@data <- data.frame(r_pts@data, longitude = sp::coordinates(r_pts)[, 1],
                         latitude = sp::coordinates(r_pts)[, 2])

return(r_pts@data)
}


#### Potting indices

plot_indices <- function(data, titre) {
  graph_indices <- ggplot2::ggplot(data) +
  ggplot2::geom_point(ggplot2::aes_string(x = data[, 2], y = data[, 3], color = data[, titre]), shape = "square", size = 2) + ggplot2::scale_colour_gradient(low = "blue", high = "orange", na.value = "grey50") +
  ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold")) + ggplot2::ggtitle(titre)

ggplot2::ggsave(paste0(titre, ".png"), graph_indices, width = 12, height = 10, units = "cm")
}

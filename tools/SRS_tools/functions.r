#Rscript

#########################
##      Functions      ##
#########################

#####Packages : raster
#               sp
#               ggplot2

#### Convert raster to dataframe

# Convert raster to SpatialPointsDataFrame
convert_raster <- function(data_raster) {
r_pts <- raster::rasterToPoints(data_raster, spatial = T)

# reproject sp object
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" 
r_pts <- sp::spTransform(r_pts, sp::CRS(geo.prj)) 


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


###########
###########

    ## Reorganisation of files arborescence : uniformization as we use .zip files, arborescence may be different and there is several difficulties to overcome : 
    ##  - many types of raster files exists
    ##  - sometimes a raster layer is builted from a directory, sometimes from a singular file, sometimes from several files through the designation of a single file
    ##  - sometimes a raster file contains one layer (when stacked) or band (when only rasterized), sometimes it contains several layers. We consider one layer represents one environmental variable.
    ##  - sometimes it is necessary for each variables of a same environmental data type at a particular scenario to be in separated directories as several raster files are needed to build the raster and these files must have a particular name. Which makes n directories (one per variables) containing the same number of files with the same names.
    ##  - we have to check if each environmental data types has the same number of variables in the present data and in each scenarios of future data + make sure we can make they match properly

    ## First, simplify the various paths of available files : supress directories containing only directories (no files), in the end, the longest path you can have has 4 directories : One for the type of environmental data, one for present or future data, if on future data : one for each scenario and finally, when necessary, one for each variables
  
del_files <- function(file) {
  files <- list.files(".", recursive = TRUE) #list available files
 
    zipf <- grep("\\.zip$", files, value = TRUE)
    if(length(zipf) > 0) {stop(".zip files have been found inside the following archives :", paste0(unique(paste0("\n- ", na.omit(as.numeric(gsub("RESULTS+)/.*$", "\\1", zipf)))+1, ": Environmental data -> ", gsub("RESULTS+/.*$", "\\1", zipf), " archive(s)"))), "\nPlease don't use nested zip files")}

    lapply(c("present"), #Wether it's present or future data
           function(x) {
               lapply(n_env_data,
                      function(y) {
                          ## We start by deleting all unnecessary directories : with a path of more directories than the maximum 4 directories prevously described
                          from_dir_long <- grep(paste0(y, "/", x, "/.+/.+"), files, value = TRUE) #list files contained in more directories than only "env_data_y/x/*/" => path too long in all cases
                          to_dir_short <- gsub(paste0("(", y, "/", x, "/).+/([^/]+)"),"\\1\\2", from_dir_long) #write the shortened paths of those files
                          lapply(unique(gsub(paste0("(", y, "/", x, ")/.+(/[^/]+)"), "\\1\\2", from_dir_long)), dir.create) #create the shortened path directories
                          file.copy(from_dir_long, to_dir_short) #copy files from long to short paths
                          unlink(gsub(paste0("(", y, "/", x, "/[^/]+/).+"),"\\1", from_dir_long), recursive = TRUE) #delete unnecessary directories
                      })
           })
}

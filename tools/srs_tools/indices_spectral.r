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
}else {
    data_raster <- args[1]
    data_header <- args[2]
    data <- args[3]
    source(args[4])
    source(args[5])
    source(args[6])
    indice_choice <- strsplit(args[7], ",")[[1]]
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
  data_raster <- list.files("data_dir/results/Reflectance", pattern = "_Refl")
  data_raster <- file.path("data_dir/results/Reflectance", data_raster[1])
  refl <- raster::brick(data_raster)
  refl2 <- raster::raster(data_raster)
} else {
  # Read raster
  refl <- raster::brick(data_raster)
  refl2 <- raster::raster(data_raster)
}
# get raster band name and clean format. Expecting band name and wav
# reflFactor = 10000 when reflectance is coded as INT16
refl <- raster::aggregate(refl, fact = 10)

# Convert raster to SpatialPointsDataFrame
refl2 <- raster::aggregate(refl2, fact = 10)
r_pts <- convert_raster(refl2)
table_ind <- r_pts
# create directory for Spectralelength to be documented in image
hdr_refl <- read_envi_header(get_hdr_name(data_raster))
sensorbands <- hdr_refl$wavelength
# compute a set of spectral indices defined by indexlist from S2 data indices
si_path <- file.path("SpectralIndices")
dir.create(path = si_path, showWarnings = FALSE, recursive = TRUE)
# Save spectral indices

indices <- lapply(indice_choice, function(x) {
  indices_list <- computespectralindices_raster(refl = refl, sensorbands = sensorbands,
                                                  sel_indices = x,
                                                  reflfactor = 10000, stackout = FALSE)

  index_path <- file.path(si_path, paste(basename(data_raster), "_", x, sep = ""))
  spec_indices <- stars::write_stars(stars::st_as_stars(indices_list$spectralindices[[1]]), dsn = index_path, driver = "ENVI", type = "Float32")

  # write band name in HDR
  hdr <- read_envi_header(get_hdr_name(index_path))
  hdr$`band names` <- x
  write_envi_header(hdr = hdr, hdrpath = get_hdr_name(index_path))
  # Writting the tabular and the plot
  r_pts[, x] <- as.data.frame(sapply(spec_indices, c))
  plot_indices(data = r_pts, titre = x)
  return(r_pts)
})

new_table <- as.data.frame(indices)
new_table <- new_table[, !grepl("longitude", names(new_table))]
new_table <- new_table[, !grepl("latitude", names(new_table))]
new_table <- new_table[, !grepl(basename(data_raster), names(new_table))]

table_ind <- cbind(table_ind, new_table)
if (length(indice_choice) == 1) {
  colnames(table_ind) <- c(basename(data_raster), "longitude", "latitude", indice_choice)
}

write.table(table_ind, file = "Spec_Index.tabular", sep = "\t", dec = ".", na = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Get the raster layer of the indice as an output

#Rscript

###########################################
##     Preprocessing Sentinel 2 data     ##
###########################################

#####Packages : sen2r,
#    jqr,
#    protolite,
#    raster,
#    sf,
#    rgeos,
#    sp,
#    raster,
#    stars,
#    stringr,
#    progress,
#    rgdal,
#    R.utils,
#    gdalUtils,
#    fasterize,
#    XML,
#    XML2

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    data <- args[1]
    source(args[2])
    data_source <- as.character(args[3])
}

##____________________________________________________________________##
##        Define where data is stored and where to write results      ##
##--------------------------------------------------------------------##

#Create a directory where to unzip your folder of data
dir.create("data_dir")
unzip(data, exdir = "data_dir")

# Result directory
result_path <- "results"
dir.create(path = result_path, showWarnings = FALSE, recursive = TRUE)

#Csv file for output useless but needed for linter
write.csv(data_source, "Mission.csv")

# define raster path
if (data_source == "SAFE") {
    path_s2 <- file.path("data_dir", list.files("data_dir", pattern = ".SAFE"))
    #To define the level and know if a correction is needed (convert not ready yet)
    level_info <- get_s2_level(path_s2)
    if (level_info == "L1C") {
        stop("! This tool works for data of L2A level and NOT for the L1C level which is currently a work in progress !")
    }
}else {
    path_s2 <- file.path("data_dir")
}

##____________________________________________________________________##
##                  Extract, resample & stack data                    ##
##--------------------------------------------------------------------##
# define resolution
resolution <- 10
# define source of data
s2source <- data_source

s2obj <- extract_from_s2_l2a(path_dir_s2 = path_s2,
                                        path_vector = NULL,
                                        s2source = s2source,
                                        resolution = resolution)

##____________________________________________________________________##
##                        Write CLOUD MASK                            ##
##--------------------------------------------------------------------##

# directory for cloud mask
cloud_path <- file.path(result_path, "CloudMask")
dir.create(path = cloud_path, showWarnings = FALSE, recursive = TRUE)
# Filename for cloud mask
cloudmasks <- save_cloud_s2(s2_stars = s2obj$s2_stack,
                                       cloud_path = cloud_path,
                                       s2source = s2source, saveraw = TRUE)

zip_cloud <- file.path("Cloud.zip")
zip::zip(zip_cloud, cloud_path)
##____________________________________________________________________##
##                        Write REFLECTANCE                           ##
##--------------------------------------------------------------------##

# directory for Reflectance
refl_dir <- file.path(result_path, "Reflectance")
dir.create(path = refl_dir, showWarnings = FALSE, recursive = TRUE)

if (data_source == "SAFE") {
    # filename for Reflectance
    refl_path <- file.path(refl_dir, paste(basename(s2obj$s2_bands$GRANULE), "_Refl", sep = ""))

    # Save Reflectance file as ENVI image with BIL interleaves
    tile_s2 <- substring(strsplit(basename(s2obj$s2_bands$GRANULE), "_")[[1]][2], 2)
    dateacq_s2 <- as.Date(substring(strsplit(basename(s2obj$s2_bands$GRANULE), "_")[[1]][4], 1, 8), format = "%Y%m%d")
}else {
    # filename for Reflectance
    refl_path <- file.path(refl_dir, paste(basename(s2obj$s2_bands$path_tile_s2), "_Refl", sep = ""))

    # Save Reflectance file as ENVI image with BIL interleaves
    tile_s2 <- substring(strsplit(basename(s2obj$s2_bands$path_tile_s2), "_")[[1]][2], 2)
    dateacq_s2 <- as.Date(substring(strsplit(basename(s2obj$s2_bands$path_tile_s2), "_")[[1]][4], 1, 8), format = "%Y%m%d")
}

save_data <- save_reflectance_s2(s2_stars = s2obj$s2_stack, refl_path = refl_path,
                               s2sat = NULL, tile_s2 = tile_s2, dateacq_s2 = dateacq_s2,
                               format = "ENVI", datatype = "Int16", mtd = s2obj$s2_bands$metadata, mtd_msi = s2obj$s2_bands$metadata_MSI)

zip_files <- file.path("Refl.zip")
zip::zip(zip_files, refl_dir)

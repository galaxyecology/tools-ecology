# == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# preprocS2
# Lib_preprocess_S2.R
# == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# PROGRAMMERS:
# Jean-Baptiste FERET <jb.feret@teledetection.fr>
# Copyright 2021/08 Jean-Baptiste FERET
# == == == == == == == == == == == == == == == == == == == == == == == == == == ==
# This Library contains functions to preprocess Sentinel-2 images downloaded from
# different data hubs, such as THEIA, PEPS or SCIHUB
# == == == == == == == == == == == == == == == == == == == == == == == == == == ==

#" This function adjusts information from ENVI header
#"
#" @param dsn character. path where to store the stack
#" @param bands list. should include "bandname", and if possible "wavelength"
#" @param sensor character. Name of the sensor used to acquire the image
#" @param stretch boolean. Set TRUE to get 10% stretching at display for reflectance, mentioned in hdr only
#"
#" @return None
#" @importFrom utils read.table
#" @importFrom raster hdr raster
#" @export
adjust_envi_hdr <- function(dsn, bands, sensor = "Unknown", stretch = FALSE) {

  # Edit hdr file to add metadata
  hdr <- read_ENVI_header(get_hdr_name(dsn))
  hdr$`band names` <- bands$bandname
  if (length(bands$wavelength) == length(bands$bandname)) {
    hdr$wavelength <- bands$wavelength
  }else {
    hdr$wavelength <- NULL
  }
  if (stretch == TRUE) {
    hdr$`default stretch` <- "0.000000 1000.000000 linear"
  }
  hdr$`z plot range` <- NULL
  hdr$`data ignore value` <- "-Inf"
  hdr$`sensor type` <- sensor
  write_envi_header(hdr = hdr, hdrpath = get_hdr_name(dsn))

  # remove unnecessary files
  file2remove <- paste(dsn, ".aux.xml", sep = "")
  if (file.exists(file2remove)) file.remove(file2remove)
  file2remove <- paste(dsn, ".prj", sep = "")
  if (file.exists(file2remove)) file.remove(file2remove)
  file2remove <- paste(dsn, ".stx", sep = "")
  if (file.exists(file2remove)) file.remove(file2remove)
  return(invisible())
}

#" This function saves reflectance files
#"
#" @param s2sat character. Sentinel-2 mission ("2A" or "2B")
#" @param tile_s2 character. S2 tile name (2 numbers + 3 letters)
#" @param dateacq_s2 double. date of acquisition
#"
#" @return s2mission character. name of the S2 mission (2A or 2B)
#" @importFrom sen2r safe_getMetadata check_scihub_connection s2_list
#" @export
check_s2mission <- function(s2sat, tile_s2, dateacq_s2) {

  # is mission already defined by user?
  if (!is.null(s2sat)) {
    if (s2sat == "2A") {
      s2mission <- "2A"
    }else if (s2sat == "2B") {
      s2mission <- "2B"
    }else {
      message("Could not identify if image from Sentinel-2A or -2B")
      message("Defining central wavelength of spectral bands based on S2A")
      s2mission <- "2A"
    }
#  } else if (!is.null(tile_s2) & !is.null(dateacq_s2)){
#    if (sen2r::check_scihub_connection()==T){
#      tileOK <- sen2r::s2_list(tile = tile_s2,time_interval = as.Date(dateacq_s2))
#      s2mission <- sen2r::safe_getMetadata(tileOK, "mission")[[1]]
#      if (is.null(s2mission)){
#        message("Could not identify if image from Sentinel-2A or -2B")
#        message("Defining central wavelength of spectral bands based on S2A")
#        s2mission <- "2A"
#      }
#    } else {
#      message("Could not identify if image from Sentinel-2A or -2B")
#      message("Defining central wavelength of spectral bands based on S2A")
#      s2mission <- "2A"
#    }
  }else {
    message("Could not identify if image from Sentinel-2A or -2B")
    message("Defining central wavelength of spectral bands based on S2A")
    s2mission <- "2A"
  }
  return(s2mission)
}

#" this function aims at computing directory size
#" @param path character. path for directory
#" @param recursive boolean . set T if recursive
#"
#" @return size_files numeric. size in bytes
#" - image stack
#" - path for individual band files corresponding to the stack
#" - path for vector (reprojected if needed)
#"
#" @importFrom raster raster
#" @importFrom tools file_path_sans_ext file_ext
#" @export
dir_size <- function(path, recursive = TRUE) {
  stopifnot(is.character(path))
  files <- list.files(path, full.names = TRUE, recursive = recursive)
  vect_size <- sapply(files, function(x) file.size(x))
  size_files <- sum(vect_size)
  return(size_files)
}

#" This function reads S2 data from L2A directories downloaded from
#" various data hubs including THEIA, PEPS & SCIHUB (SAFE format & LaSRC)
#" @param path_dir_s2 character. path for S2 directory
#" @param path_vector character. path for vector file
#" @param s2source character. type of directory format (depends on atmospheric correction: SAFE produced from Sen2Cor)
#" @param resolution numeric. buffer applied to vector file (in meters)
#" @param interpolation character. method for resampling. default = "bilinear"
#" @param fre_sre character. SRE or FRE products from THEIA
#"
#" @return listout list.
#" - image stack
#" - path for individual band files corresponding to the stack
#" - path for vector (reprojected if needed)
#"
#" @importFrom raster raster
#" @importFrom tools file_path_sans_ext file_ext
#" @export
extract_from_s2_l2a <- function(path_dir_s2, path_vector = NULL, s2source = "SAFE",
                                resolution = 10, interpolation = "bilinear", fre_sre = "FRE") {
  # Get list of paths corresponding to S2 bands and depending on S2 directory
  s2_bands <- get_s2_bands(path_dir_s2 = path_dir_s2,
                           s2source = s2source,
                           resolution = resolution,
                           fre_sre = fre_sre)

  if (length(s2_bands$s2bands_10m) > 0) {
    rastmp <- raster::raster(s2_bands$s2bands_10m[[1]])
  } else if (length(s2_bands$s2bands_20m) > 0) {
    rastmp <- raster::raster(s2_bands$s2bands_20m[[1]])
  }
  # check if vector and raster share the same projection. if not, re-project vector
  if (!is.null(path_vector)) {
    raster_proj <- raster::projection(rastmp)
    path_vector_reproj <- paste(tools::file_path_sans_ext(path_vector), "_reprojected.shp", sep = "")
    path_vector <- reproject_shp(path_vector_init = path_vector,
                                 newprojection = raster_proj,
                                 path_vector_reproj = path_vector_reproj)
  }
  # Extract data corresponding to the vector footprint (if provided) & resample data if needed
  if (length(s2_bands$s2bands_10m) > 0) {
    stack_10m <- read_s2bands(s2_bands = s2_bands$s2bands_10m, path_vector = path_vector,
                              resampling = 1, interpolation = interpolation)
  }
  if (length(s2_bands$s2bands_20m) > 0) {
    if (resolution == 10 && s2source != "LaSRC") {
      resampling <- 2
    }else {
      resampling <- 1
    }
    stack_20m <- read_s2bands(s2_bands = s2_bands$s2bands_20m, path_vector = path_vector,
                              resampling = resampling, interpolation = interpolation)
  }
  # get full stack including 10m and 20m spatial resolution
  if (length(s2_bands$s2bands_10m) > 0 && length(s2_bands$s2bands_20m) > 0) {
    diffxstart <- attributes(stack_10m)$dimensions[[1]]$from - attributes(stack_20m)$dimensions[[1]]$from
    diffxstop <- attributes(stack_10m)$dimensions[[1]]$to - attributes(stack_20m)$dimensions[[1]]$to
    diffystart <- attributes(stack_10m)$dimensions[[2]]$from - attributes(stack_20m)$dimensions[[2]]$from
    diffystop <- attributes(stack_10m)$dimensions[[2]]$to - attributes(stack_20m)$dimensions[[2]]$to
    if (!diffxstop == 0) {
      # size of 20m > size of 10m --> reduce 20m
      # size of 10m > size of 20m --> reduce 10m
      if (diffxstop > 0) {
        stack_10m <- stack_10m[, 1:(dim(stack_10m)[1] - diffxstop), , ]
      }else if (diffxstop < 0) {
        stack_20m <- stack_20m[, 1:(dim(stack_20m)[1] + diffxstop), , ]
      }
    }
    if (!diffystop == 0) {
      if (diffystop > 0) {
        stack_10m <- stack_10m[, , 1:(dim(stack_10m)[2] - diffystop), ]
      }else if (diffystop < 0) {
        stack_20m <- stack_20m[, , 1:(dim(stack_20m)[2] + diffystop), ]
      }
    }
    if (!diffxstart == 0) {
      if (diffxstart > 0) {
        stack_20m <- stack_20m[, (1 + diffxstart):dim(stack_20m)[1], , ]
      }else if (diffxstart < 0) {
        stack_10m <- stack_10m[, (1 - diffxstart):dim(stack_10m)[1], , ]
      }
    }
    if (!diffystart == 0) {
      if (diffystart > 0) {
        stack_20m <- stack_20m[, , (1 + diffystart):dim(stack_20m)[2], ]
      }else if (diffystart < 0) {
        stack_10m <- stack_10m[, , (1 - diffystart):dim(stack_10m)[2], ]
      }
    }
    # reorder bands with increasing wavelength
    s2bands <- c("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12", "Cloud")
    namebands <- c(names(s2_bands$s2bands_10m), names(s2_bands$s2bands_20m))
    reorder_bands <- match(s2bands, namebands)
    namebands <- namebands[reorder_bands]
    listfiles <- c(stack_10m$attr, stack_20m$attr)[reorder_bands]

    # adjust size to initial vector footprint without buffer
    # --> buffer is needed in order to ensure that extraction following
    # footprint of vector matches for images of different spatial resolution
    # get bounding box corresponding to footprint of image or image subset
    bb_xycoords <- get_bb(path_raster = listfiles[1],
                          path_vector = path_vector, buffer = 0)

    # prepare reading data for extent defined by bounding box
    nxoff <- bb_xycoords$UL$col
    nyoff <- bb_xycoords$UL$row
    nxsize <- bb_xycoords$UR$col - bb_xycoords$UL$col + 1
    nysize <- bb_xycoords$LR$row - bb_xycoords$UR$row + 1
    nbufxsize <- nxsize
    nbufysize <- nysize
    s2_stack <- stars::read_stars(listfiles, along = "band",
                                  RasterIO = list(nXOff = nxoff, nYOff = nyoff,
                                                  nXSize = nxsize, nYSize = nysize,
                                                  nBufXSize = nbufxsize, nBufYSize = nbufysize,
                                                  resample = "nearest_neighbour"), proxy = TRUE)


    names(s2_stack$attr) <- namebands
  }else if (length(s2_bands$s2bands_10m) > 0) {
    s2_stack <- stack_10m
    namebands <- names(s2_bands$s2bands_10m)
    names(s2_stack$attr) <- namebands
  }else if (length(s2_bands$s2bands_20m) > 0) {
    s2_stack <- stack_20m
    namebands <- names(s2_bands$s2bands_20m)
    names(s2_stack$attr) <- namebands
  }

  listout <- list("s2_stack" = s2_stack, "s2_bands" = s2_bands, "path_vector" = path_vector,
                  "namebands" = namebands)
  return(listout)
}

#" This function gets coordinates of a bounding box defined by a vector (optional) and a raster
#"
#" @param path_raster character. path for raster file
#" @param path_vector character. path for vector file
#" @param buffer numeric. buffer applied to vector file (in meters)
#"
#" @return bb_xycoords list. Coordinates (in pixels) of the upper/lower right/left corners of bounding box
#" @export
get_bb <- function(path_raster, path_vector = NULL, buffer = 0) {

  if (!is.null(path_vector)) {
    # get bounding box with a 50m buffer in order to allow for interpolation
    bb_xycoords <- get_bb_from_vector(path_raster = path_raster,
                                      path_vector = path_vector,
                                      buffer = buffer)
  }else if (is.null(path_vector)) {
    bb_xycoords <- get_bb_from_fullimage(path_raster)
  }
  return(bb_xycoords)
}

#" This function gets extreme coordinates of a bounding box corresponding to a full image
#"
#" @param path_raster character. path for raster file
#"
#" @return bb_xycoords list. Coordinates (in pixels) of the upper/lower right/left corners of bounding box
#" @importFrom raster raster
#" @export
get_bb_from_fullimage <- function(path_raster) {
  # get raster coordinates corresponding to Full image
  rasterobj <- raster::raster(path_raster)
  bb_xycoords <- list()
  bb_xycoords[["UL"]] <- data.frame("row" = 1, "col" = 1)
  bb_xycoords[["UR"]] <- data.frame("row" = 1, "col" = dim(rasterobj)[2])
  bb_xycoords[["LL"]] <- data.frame("row" = dim(rasterobj)[1], "col" = 1)
  bb_xycoords[["LR"]] <- data.frame("row" = dim(rasterobj)[1], "col" = dim(rasterobj)[2])
  return(bb_xycoords)
}

#" This gets bounding box corresponding to a vector from a raster (UL, UR, LL, LR corners)
#"
#" @param path_raster character. path for raster file
#" @param path_vector character. path for vector file
#" @param buffer numeric. buffer applied to vector file (in meters)
#"
#" @return bb_xycoords list. Coordinates (in pixels) of the upper/lower right/left corners of bounding box
#" @importFrom sf st_read st_bbox st_crop
#" @importFrom rgeos gbuffer bbox2SP
#" @importFrom sp SpatialPoints bbox
#" @importFrom raster projection extract extent raster
#" @importFrom methods as
#" @export
get_bb_from_vector <- function(path_raster, path_vector, buffer = 0) {

  Raster <- raster::raster(path_raster)
  # extract BB coordinates from vector
  bb_vector <- rgeos::gbuffer(spgeom = as(sf::st_read(dsn = path_vector, quiet = TRUE), "Spatial"),
                              width = buffer, byid = TRUE)
  # extract BB coordinates from raster
  bb_raster <- rgeos::bbox2SP(bbox = bbox(Raster))
  # compute intersection
  Intersect <- rgeos::gIntersection(bb_vector, bb_raster)
  bbext <- raster::extent(Intersect)
  xmin <- bbext[1]
  xmax <- bbext[2]
  ymin <- bbext[3]
  ymax <- bbext[4]
  # get coordinates of bounding box corresponding to vector
  corners <- list()
  corners[["UR"]] <- sp::SpatialPoints(coords = cbind(xmax, ymax))
  corners[["LR"]] <- sp::SpatialPoints(coords = cbind(xmax, ymin))
  corners[["UL"]] <- sp::SpatialPoints(coords = cbind(xmin, ymax))
  corners[["LL"]] <- sp::SpatialPoints(coords = cbind(xmin, ymin))
  raster::projection(corners[["UL"]]) <- raster::projection(corners[["UR"]]) <-
    raster::projection(corners[["LL"]]) <- raster::projection(corners[["LR"]]) <-
    raster::projection(sf::st_read(dsn = path_vector, quiet = TRUE))
  # get coordinates for corners of bounding box
  bb_xycoords <- list()
  for (corner in names(corners)) {
    ex_df <- as.data.frame(raster::extract(Raster, corners[[corner]], cellnumbers = TRUE))
    colrow <- ind2sub(Raster, ex_df$cell)
    bb_xycoords[[corner]] <- data.frame("row" = colrow$row, "col" = colrow$col)
  }
  return(bb_xycoords)
}

#" get hdr name from image file name, assuming it is BIL format
#"
#" @param impath path of the image
#"
#" @return corresponding hdr
#" @import tools
#" @export
get_hdr_name <- function(impath) {
  if (tools::file_ext(impath) == "") {
    impathhdr <- paste(impath, ".hdr", sep = "")
  }else if (tools::file_ext(impath) == "bil") {
    impathhdr <- gsub(".bil", ".hdr", impath)
  }else if (tools::file_ext(impath) == "zip") {
    impathhdr <- gsub(".zip", ".hdr", impath)
  }else {
    impathhdr <- paste(tools::file_path_sans_ext(impath), ".hdr", sep = "")
  }

  if (!file.exists(impathhdr)) {
    message("WARNING : COULD NOT FIND hdr FILE")
    print(impathhdr)
    message("Process may stop")
  }
  return(impathhdr)
}

#" This function returns path for the spectral bands to be used
#"
#" @param path_dir_s2 character. Path for the directory containing S2 data. either L2A .SAFE S2 file or THEIA directory
#" @param s2source character. defines if data comes from SciHub as SAFE directory, from THEIA or from LaSRC
#" @param resolution numeric. spatial resolution of the final image: 10m or 20m
#" @param fre_sre character. SRE or FRE products from THEIA
#"
#" @return listbands list. contains path for spectral bands corresponding to 10m and 20m resolution
#" @export
get_s2_bands <- function(path_dir_s2, s2source = "SAFE", resolution = 10, fre_sre = "FRE") {

  if (s2source == "SAFE" || s2source == "Sen2Cor") {
    listbands <- get_s2_bands_from_sen2cor(path_dir_s2 = path_dir_s2, resolution = resolution)
  }else if (s2source == "THEIA") {
    listbands <- get_s2_bands_from_theia(path_dir_s2 = path_dir_s2, resolution = resolution,
                                         fre_sre = fre_sre)
  }else if (s2source == "LaSRC") {
    listbands <- get_s2_bands_from_lasrc(path_dir_s2 = path_dir_s2, resolution = resolution)
  }else {
    message("The data source (Atmospheric correction) for Sentinel-2 image is unknown")
    message("Please provide S2 images from one of the following data sources:")
    message("- LaSRC (atmospheric correction: LaSRC)")
    message("- THEIA (atmospheric correction: MAJA)")
    message("- SAFE (atmospheric correction: Sen2Cor)")
    s2bands_10m <- s2bands_20m <- granule <- mtdfile <- metadata_MSI <- metadata_lasrc <- NULL
    listbands <- list("s2bands_10m" = s2bands_10m, "s2bands_20m" = s2bands_20m, "GRANULE" = granule,
                      "metadata" = mtdfile, "metadata_MSI" = metadata_MSI,
                      "metadata_lasrc" = metadata_lasrc)
  }
  return(listbands)
}

#" This function returns path for the spectral bands in SAFE / sen2Cor directory
#"
#" @param path_dir_s2 character. Path for the SAFE directory containing S2 data
#" @param resolution numeric. spatial resolution of the final image: 10m or 20m
#"
#" @return listbands list. contains path for spectral bands corresponding to 10m and 20m resolution, as well name of as granule
#" @export
get_s2_bands_from_sen2cor <- function(path_dir_s2, resolution = 10) {
  # build path for all bands
  if (resolution == 10) {
    b10m <- c("B02", "B03", "B04", "B08")
    b20m <- c("B05", "B06", "B07", "B8A", "B11", "B12")
  }else {
    b10m <- c()
    b20m <- c("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")
  }
  # get granule directory & path for corresponding metadata XML file
  granule <- list.dirs(list.dirs(path_dir_s2, recursive = FALSE)[grep(pattern = "GRANULE",
                                                                     x = list.dirs(path_dir_s2, recursive = FALSE))], recursive = FALSE)
  mtdfile <- file.path(granule, "MTD_TL.xml")
  if (file.exists(file.path(path_dir_s2, "MTD_MSIL2A.xml"))) {
    mtd_msi_file <- file.path(path_dir_s2, "MTD_MSIL2A.xml")
  } else {
    mtd_msi_file <- NULL
  }

  # Define path for bands
  s2bands_20m_dir <- file.path(granule, "IMG_DATA", "R20m")
  s2bands_10m_dir <- file.path(granule, "IMG_DATA", "R10m")
  s2bands_10m <- s2bands_20m <- list()
  for (band in b20m) {
    s2bands_20m[[band]] <- file.path(s2bands_20m_dir, list.files(s2bands_20m_dir, pattern = band))
  }
  for (band in b10m) {
    s2bands_10m[[band]] <- file.path(s2bands_10m_dir, list.files(s2bands_10m_dir, pattern = band))
  }
  # get cloud mask
  cloud <- "MSK_CLDPRB_20m"
  cloud_20m_dir <- file.path(granule, "QI_DATA")
  s2bands_20m[["Cloud"]] <- file.path(cloud_20m_dir, list.files(cloud_20m_dir, pattern = cloud))
  listbands <- list("s2bands_10m" = s2bands_10m,
                    "s2bands_20m" = s2bands_20m,
                    "GRANULE" = granule,
                    "metadata" = mtdfile,
                    "metadata_MSI" = mtd_msi_file,
                    "metadata_lasrc" = NULL)
  return(listbands)
}

#" This function returns path for the spectral bands in LaSRC directory
#"
#" @param path_dir_s2 character. Path for the SAFE directory containing S2 data
#" @param resolution numeric. spatial resolution of the final image: 10m or 20m
#"
#" @return listbands list. contains path for spectral bands corresponding to 10m and 20m resolution, as well name of as granule
#" @importFrom stringr str_subset
#" @export
get_s2_bands_from_lasrc <- function(path_dir_s2, resolution = 10) {

  # get granule directory & path for corresponding metadata XML file
  granule <- path_dir_s2
  mtdfile <- file.path(granule, "MTD_TL.xml")
  if (file.exists(file.path(path_dir_s2, "MTD_MSIL1C.xml"))) {
    mtd_msi_file <- file.path(path_dir_s2, "MTD_MSIL1C.xml")
  } else {
    mtd_msi_file <- NULL
  }

  # build path for all bands
  b10m <- c("band2", "band3", "band4", "band5", "band6", "band7", "band8", "band8a", "band11", "band12")
  b10m_standard <- c("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")
  # Define path for bands
  s2bands_10m <- s2bands_20m <- list()
  for (i in 1:length(b10m)) {
    s2bands_10m[[b10m_standard[i]]] <- file.path(path_dir_s2,
                                                 list.files(path_dir_s2,
                                                            pattern = paste(b10m[i], ".tif", sep = "")))
  }

  # get metadata file containing offset
  mtd_lasrc <- str_subset(list.files(path_dir_s2, pattern = "S2"), ".xml$")
  if (file.exists(file.path(path_dir_s2, mtd_lasrc))) {
    metadata_lasrc <- file.path(path_dir_s2, mtd_lasrc)
  } else {
    metadata_lasrc <- NULL
  }
  # get cloud mask
  cloud <- "CLM"
  s2bands_10m[["Cloud"]] <- file.path(path_dir_s2, list.files(path_dir_s2, pattern = cloud))
  listbands <- list("s2bands_10m" = s2bands_10m,
                    "s2bands_20m" = s2bands_20m,
                    "GRANULE" = granule,
                    "metadata" = mtdfile,
                    "metadata_MSI" = mtd_msi_file,
                    "metadata_lasrc" = metadata_lasrc)
  return(listbands)
}

#" This function returns path for the spectral bands in THEIA directory
#"
#" @param path_dir_s2 character. Path for the SAFE directory containing S2 data
#" @param resolution numeric. spatial resolution of the final image: 10m or 20m
#" @param fre_sre character. SRE or FRE products from THEIA
#"
#" @return listbands list. contains path for spectral bands corresponding to 10m and 20m resolution, as well name of as granule
#" @export
get_s2_bands_from_theia <- function(path_dir_s2, resolution = 10, fre_sre = "FRE") {

  # build path for all bands
  if (resolution == 10) {
    b10m <- c("B02", "B03", "B04", "B08")
    b20m <- c("B05", "B06", "B07", "B8A", "B11", "B12")
  } else {
    b10m <- c()
    b20m <- c("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A", "B11", "B12")
  }

  # get path_tile_s2 directory & path for corresponding metadata XML file
  path_tile_s2 <- list.dirs(path_dir_s2, recursive = FALSE)
  files_tile_s2 <- list.files(path_tile_s2, recursive = FALSE)
  mtdfile <- file.path(path_tile_s2, files_tile_s2[grep(pattern = "MTD_ALL.xml", x = files_tile_s2)])

  # Define path for bands
  s2bands_10m_dir <- s2bands_20m_dir <- path_tile_s2
  s2bands_10m <- s2bands_20m <- list()
  for (band in b20m) {
    band_20m_pattern <- paste0(gsub("0", "", band), ".tif") # for THEAI band 2 is "B2" ("B02" for SAFE)
    list_files_20m <- list.files(s2bands_20m_dir, pattern = band_20m_pattern)
    s2bands_20m[[band]] <- file.path(s2bands_20m_dir, list_files_20m)[grep(pattern = fre_sre,
                                                                           x = file.path(s2bands_20m_dir, list_files_20m))]
  }
  for (band in b10m) {
    band_10m_pattern <- paste0(gsub("0", "", band), ".tif") # for THEAI band 2 is "B2" ("B02" for SAFE)
    list_files_10m <- list.files(s2bands_10m_dir, pattern = band_10m_pattern)
    s2bands_10m[[band]] <- file.path(s2bands_10m_dir, list_files_10m)[grep(pattern = fre_sre,
                                                                           x = file.path(s2bands_10m_dir, list_files_10m))]
  }

  # get cloud mask 10m
  cloud_10m <- "CLM_R1"
  cloud_10m_dir <- file.path(path_tile_s2, "MASKS")
  s2bands_10m[["Cloud"]] <- file.path(cloud_10m_dir, list.files(cloud_10m_dir, pattern = cloud_10m))

  # get cloud mask 20m
  cloud_20m <- "CLM_R2"
  cloud_20m_dir <- file.path(path_tile_s2, "MASKS")
  s2bands_20m[["Cloud"]] <- file.path(cloud_20m_dir, list.files(cloud_20m_dir, pattern = cloud_20m))

  # return list bands
  listbands <- list("s2bands_10m" = s2bands_10m,
                    "s2bands_20m" = s2bands_20m,
                    "path_tile_s2" = path_tile_s2,
                    "metadata" = mtdfile)
  return(listbands)
}

#" This function check S2 data level:
#" - L2A: already atmospherically corrected
#" - L1C: requires atmospheric corrections with sen2cor
#"
#" @param prodname character. original name for the S2 image
#"
#" @return s2level character. S2 level: L1C or L2A
#" @export
get_s2_level <- function(prodname) {
  prodname <- basename(prodname)
  if (length(grep(pattern = "L1C_", x = prodname)) == 1) {
    s2level <- "L1C"
  } else if (length(grep(pattern = "L2A_", x = prodname)) == 1) {
    s2level <- "L2A"
  }
  return(s2level)
}

#" This function gets tile from S2 image
#"
#" @param prodname character. original name for the S2 image
#"
#" @return tilename character
#" @importFrom tools file_path_sans_ext
#" @export
get_tile <- function(prodname) {
  prodname <- basename(prodname)
  tilename <- tools::file_path_sans_ext(gsub("_.*", "", gsub(".*_T", "", prodname)))
  return(tilename)
}

#" This function gets acquisition date from S2 image
#"
#" @param prodname character. original name for the S2 image
#"
#" @return dateacq character
#" @export
get_date <- function(prodname) {
  prodname <- basename(prodname)
  dateacq <- as.Date(gsub("T.*", "", gsub(".*_20", "20", prodname)), format = "%Y%m%d")
  return(dateacq)
}

#" download S2 L1C data from Copernicus hub or Google cloud
#"
#" @param list_safe safe object. produced with sen2r::s2_list
#" @param l1c_path character. path for storage of L1C image
#" @param path_vector path for a vector file
#" @param time_interval dates. time interval for S2 query
#" @param googlecloud boolean. set to TRUE if google cloud SDK is installed and
#" @param forcegoogle boolean. set to TRUE if only google requested
#" sen2r configured as an alternative hub for S2 download
#"
#" @return prodname character. S2 Product name
#" @importFrom sen2r safe_is_online s2_list s2_download s2_order check_gcloud
#" @export
get_s2_l1c_image <- function(list_safe, l1c_path, path_vector, time_interval,
                             googlecloud = FALSE, forcegoogle = FALSE) {
  # Check if available from Copernicus hub first
  copernicus_avail <- sen2r::safe_is_online(list_safe)
  # if available: download
  prodname <- attr(list_safe, which = "name")
  if (file.exists(file.path(l1c_path, prodname))) {
    message("L1C file already downloaded")
    message(file.path(l1c_path, prodname))
  } else {
    if (copernicus_avail == TRUE && forcegoogle == FALSE) {
      sen2r::s2_download(list_safe, outdir = l1c_path)
    } else if (copernicus_avail == FALSE || forcegoogle == TRUE) {
      # if not available and googlecloud==TRUE
      if (googlecloud == TRUE) {
        # check if google cloud SDK available from this computer
        ggc <- sen2r::check_gcloud()
        if (ggc == TRUE) {
          message("downloading from Google cloud")
          list_safe_ggc <- sen2r::s2_list(spatial_extent = sf::st_read(dsn = path_vector),
                                          time_interval = time_interval,
                                          server = "gcloud")
          prodname <- attr(list_safe_ggc, which = "name")
          if (file.exists(file.path(l1c_path, prodname))) {
            message("L1C file already downloaded")
            message(file.path(l1c_path, prodname))
          } else {
            sen2r::s2_download(list_safe_ggc, outdir = l1c_path)
            # check if QI_DATA exists in DATASTRIP, and create it if not the case
            datastrip_path <- file.path(l1c_path, prodname, "DATASTRIP")
            dsdir <- list.dirs(datastrip_path, recursive = FALSE)
            if (length(match(list.dirs(dsdir, recursive = FALSE, full.names = FALSE), "QI_DATA")) == 0) {
              dir.create(file.path(dsdir, "QI_DATA"))
            }
          }
        } else if (ggc == FALSE) {
          message("googlecloud set to TRUE but missing")
          message("Please install Google cloud SDK")
          message("https://cloud.google.com/sdk/docs/install")
          message("and/or set configuration of sen2r following instructions")
          message("https://www.r-bloggers.com/2021/06/downloading-sentinel-2-archives-from-google-cloud-with-sen2r/")
        }
      }
    }
    if (copernicus_avail == FALSE && googlecloud == FALSE) {
      message("S2 image in Long Term Archive (LTA)")
      message("Ordering image from  LTA")
      message("This may take 1 day, please run your script later")
      orders2 <- sen2r::s2_order(list_safe)
      message("An alternative is possible with Google cloud SDK")
      message("https://cloud.google.com/sdk/docs/install")
      message("and/or set configuration of sen2r following instructions")
      message("https://www.r-bloggers.com/2021/06/downloading-sentinel-2-archives-from-google-cloud-with-sen2r/")
    }
  }
  return(prodname)
}

#" download S2 L2A data from Copernicus hub or convert L1C to L2A
#"
#" @param l2a_path character. path for storage of L2A image
#" @param spatial_extent path for a vector file
#" @param dateacq character. date of acquisition
#" @param deletel1c Boolean. set TRUE to delete L1C images
#" @param Sen2Cor Boolean. set TRUE to automatically perform atmospheric corrections using sen2Cor
#" @param googlecloud boolean. set to TRUE if google cloud SDK is installed and
#" sen2r configured as an alternative hub for S2 download
#"
#" @return pathl2a character. Path for L2A image
#" @importFrom sen2r s2_list s2_download
#" @importFrom R.utils getAbsolutePath

#" @export
get_s2_l2a_image <- function(l2a_path, spatial_extent, dateacq,
                             deletel1c = FALSE, Sen2Cor = TRUE,
                             googlecloud = FALSE) {

  # Needs to be updated: define path for L1c data
  l1c_path <- l2a_path
  # define time interval
  time_interval <- as.Date(c(dateacq, dateacq))
  # get list S2 products corresponding to study area and date of interest using sen2r package
  if (googlecloud == TRUE) {
    server <- c("scihub", "gcloud")
  } else if (googlecloud == FALSE) {
    server <- "scihub"
  }
  list_safe <- sen2r::s2_list(spatial_extent = sf::st_read(dsn = spatial_extent),
                              time_interval = time_interval,
                              server = server, availability = "check")
  # download products
  sen2r::s2_download(list_safe, outdir = l2a_path)
  # name all products
  prodname <- attr(list_safe, which = "name")
  prodfullpath <- file.path(l2a_path, prodname)
  if (Sen2Cor == TRUE) {
    for (imgname in prodname) {
      s2level <- get_s2_level(imgname)
      if (s2level == "L1C") {
        # prodname <- get_s2_l1c_image(list_safe[whichimg], l1c_path,spatial_extent,time_interval,googlecloud=googlecloud)
        datepattern <- gsub(pattern = "-", replacement = "", x = dateacq)
        pathl2a <- s2_from_l1c_to_l2a(prodname = imgname, l1c_path = l2a_path, l2a_path = l2a_path,
                                      datepattern = datepattern, tmp_path = NULL)
        if (deletel1c == TRUE) {
          unlink(x = R.utils::getAbsolutePath(file.path(l1c_path, prodname)),
                 recursive = TRUE, force = TRUE)
          # delete from full path and add atmospherically corrected
          whichimg <- grep(x = prodfullpath, pattern = imgname)
          dateacq <- get_date(imgname)
          tilename <- get_tile(imgname)
          pathl2a <- list.files(path = l2a_path, pattern = tilename, full.names = TRUE)
          pathl2a <- pathl2a[grep(x = pathl2a, pattern = dateacq)]
          pathl2a <- pathl2a[grep(x = basename(pathl2a), pattern = "L2A")]
          prodfullpath[whichimg] <- pathl2a
        }
      }
    }
  }

  return(prodfullpath)
}

#" convert image coordinates from index to X-Y
#"
#" @param Raster image raster object
#" @param image_index coordinates corresponding to the raster
ind2sub <- function(Raster, image_index) {
  c <- ((image_index - 1) %% Raster@ncols) + 1
  r <- floor((image_index - 1) / Raster@ncols) + 1
  my_list <- list("col" = c, "row" = r)
  return(my_list)
}

#" mosaicing a set of rasters
#"
#" @param list_rasters character. list of paths corresponding to rasters to mosaic
#" @param dst_mosaic character. path and name of mosaic produced
#" @param stretch boolean. Set TRUE to get 10% stretching at display for reflectance, mentioned in hdr only
#"
#" @return None
#" @importFrom gdalUtils mosaic_rasters
#" @importFrom raster hdr raster
#" @export
mosaic_rasters <- function(list_rasters, dst_mosaic, stretch = FALSE) {

  # produce mosaic
  gdalUtils::mosaic_rasters(gdalfile = list_rasters, dst_dataset = dst_mosaic,
                            separate = FALSE, of = "Ehdr", verbose = TRUE)

  # convert hdr to ENVI format
  raster::hdr(raster(dst_mosaic), format = "ENVI")
  # add info to hdr based on initial rasters
  hdr_init <- read_ENVI_header(get_hdr_name(list_rasters[1]))
  hdr <- read_ENVI_header(get_hdr_name(dst_mosaic))
  hdr$`band names` <- hdr_init$`band names`
  hdr$wavelength <- hdr_init$wavelength
  if (stretch == TRUE) {
    hdr$`default stretch` <- "0.000000 1000.000000 linear"
  }
  hdr$`z plot range` <- NULL
  hdr$`data ignore value` <- "-Inf"
  hdr$`sensor type` <- hdr_init$`sensor type`
  hdr$`coordinate system string` <- read.table(paste(file_path_sans_ext(dst_mosaic), ".prj", sep = ""))
  write_envi_header(hdr = hdr, hdrpath = get_hdr_name(dst_mosaic))
  return(invisible())
}

#" Reads ENVI hdr file
#"
#" @param hdrpath Path of the hdr file
#"
#" @return list of the content of the hdr file
#" @export
read_ENVI_header <- function(hdrpath) {
  # header <- paste(header, collapse = "\n")
  if (!grepl(".hdr$", hdrpath)) {
    stop("File extension should be .hdr")
  }
  hdr <- readLines(hdrpath)
  ## check ENVI at beginning of file
  if (!grepl("ENVI", hdr[1])) {
    stop("Not an ENVI header (ENVI keyword missing)")
  } else {
    hdr <- hdr [-1]
  }
  ## remove curly braces and put multi-line key-value-pairs into one line
  hdr <- gsub("\\{([^}]*)\\}", "\\1", hdr)
  l <- grep("\\{", hdr)
  r <- grep("\\}", hdr)

  if (length(l) != length(r)) {
    stop("Error matching curly braces in header (differing numbers).")
  }

  if (any(r <= l)) {
    stop("Mismatch of curly braces in header.")
  }

  hdr[l] <- sub("\\{", "", hdr[l])
  hdr[r] <- sub("\\}", "", hdr[r])

  for (i in rev(seq_along(l))) {
    hdr <- c(
      hdr [seq_len(l [i] - 1)],
      paste(hdr [l [i]:r [i]], collapse = "\n"),
      hdr [-seq_len(r [i])]
    )
  }

  ## split key = value constructs into list with keys as names
  hdr <- sapply(hdr, split_line, "=", USE.NAMES = FALSE)
  names(hdr) <- tolower(names(hdr))

  ## process numeric values
  tmp <- names(hdr) %in% c(
    "samples", "lines", "bands", "header offset", "data type",
    "byte order", "default bands", "data ignore value",
    "wavelength", "fwhm", "data gain values"
  )
  hdr [tmp] <- lapply(hdr [tmp], function(x) {
    as.numeric(unlist(strsplit(x, ", ")))
  })

  return(hdr)
}

#" This function reads a list of files corresponding to S2 bands
#" S2 bands are expected to have uniform spatial resolution and footprint
#" @param s2_bands list. list of S2 bands obtained from get_s2_bands
#" @param path_vector path for a vector file
#" @param resampling numeric. resampling factor (default = 1, set to resampling = 2 to convert 20m into 10m resolution)
#" @param interpolation character. method for resampling. default = "bilinear"
#"
#" @return stack_s2 list. contains stack of S2 bands
#"
#" @importFrom stars read_stars
#" @importFrom sf st_bbox st_read st_crop
#" @export

read_s2bands <- function(s2_bands, path_vector = NULL,
                         resampling = 1, interpolation = "bilinear") {
  # get bounding box corresponding to footprint of image or image subset
  bb_xycoords <- get_bb(path_raster = s2_bands[[1]],
                        path_vector = path_vector, buffer = 50)

  # prepare reading data for extent defined by bounding box
  nxoff <- bb_xycoords$UL$col
  nyoff <- bb_xycoords$UL$row
  nxsize <- bb_xycoords$UR$col - bb_xycoords$UL$col + 1
  nysize <- bb_xycoords$LR$row - bb_xycoords$UR$row + 1
  nbufxsize <- resampling * nxsize
  nbufysize <- resampling * nysize
  if (resampling == 1) {
    interpolation <- "nearest_neighbour"
  }
  # write interpolated individual bands in temp directory
  tmpdir <- tempdir()
  tmpfile <- list()
  for (band in names(s2_bands)) {
    stack_s2_tmp <- stars::read_stars(s2_bands[[band]], along = "band",
                                      RasterIO = list(nXOff = nxoff, nYOff = nyoff,
                                                      nXSize = nxsize, nYSize = nysize,
                                                      nBufXSize = nbufxsize, nBufYSize = nbufysize,
                                                      resample = interpolation), proxy = FALSE)
    if (!is.null(path_vector)) {
      stack_s2_tmp <- sf::st_crop(x = stack_s2_tmp, y = st_bbox(st_read(dsn = path_vector, quiet = TRUE)))
    }
    tmpfile[[band]] <- file.path(tmpdir, tools::file_path_sans_ext(basename(s2_bands[[band]])))
    if (band == "Cloud") {
      stars::write_stars(obj = stack_s2_tmp, dsn = tmpfile[[band]],
                         driver =  "ENVI", type = "Byte", overwrite = TRUE)
    } else {
      stars::write_stars(obj = stack_s2_tmp, dsn = tmpfile[[band]],
                         driver =  "ENVI", type = "Int16", overwrite = TRUE)
    }
    gc()
  }

  stack_s2 <- stars::read_stars(tmpfile, along = "band", proxy = TRUE)
  return(stack_s2)
}

#" This function reads a raster stack, and gets footprint as pixel coordinates or vector file as input
#" @param path_raster character. path for raster file
#" @param path_vector character. path for vector file
#" @param bbpix list. coordinates of pixels corresponding to a bounding box
#"
#" @return starsobj stars object corresponding to raster or raster subset
#"
#" @importFrom stars read_stars
#" @importFrom sf st_bbox st_read st_crop
#" @export
read_raster <- function(path_raster, path_vector = NULL, bbpix = NULL) {
  # get bounding box corresponding to footprint of image or image subset
  if (is.null(bbpix)) {
    bb_xycoords <- get_bb(path_raster = path_raster,
                          path_vector = path_vector, buffer = 0)
  } else {
    bb_xycoords <- bbpix
  }
  # prepare reading data for extent defined by bounding box
  nxoff <- bb_xycoords$UL$col
  nyoff <- bb_xycoords$UL$row
  nxsize <- bb_xycoords$UR$col - bb_xycoords$UL$col + 1
  nysize <- bb_xycoords$LR$row - bb_xycoords$UR$row + 1
  nbufxsize <- nxsize
  nbufysize <- nysize
  starsobj <- stars::read_stars(path_raster, along = "band",
                                RasterIO = list(nXOff = nxoff, nYOff = nyoff,
                                                  nXSize = nxsize, nYSize = nysize,
                                                  nBufXSize = nbufxsize, nBufYSize = nbufysize),
                                proxy = FALSE)
  return(starsobj)
}

#" This function reprojects a shapefile and saves reprojected shapefile
#"
#" @param path_vector_init character. path for a shapefile to be reprojected
#" @param newprojection character. projection to be applied to path_vector_init
#" @param path_vector_reproj character. path for the reprojected shapefile
#"
#" @return path_vector character. path of the shapefile
#" - path_vector_init if the vector did not need reprojection
#" - path_vector_reproj if the vector needed reprojection
#"
#" @importFrom rgdal readOGR writeOGR
#" @importFrom sp spTransform
#" @importFrom raster projection
#" @export
reproject_shp <- function(path_vector_init, newprojection, path_vector_reproj) {

  dir_vector_init <- dirname(path_vector_init)
  # shapefile extension
  fileext <- file_ext(basename(path_vector_init))
  if (fileext == "shp") {
    name_vector_init <- file_path_sans_ext(basename(path_vector_init))
    vector_init_ogr <- rgdal::readOGR(dir_vector_init, name_vector_init, verbose = FALSE)
  } else if (fileext == "kml") {
    vector_init_ogr <- rgdal::readOGR(path_vector_init, verbose = FALSE)
  }
  vector_init_proj <- raster::projection(vector_init_ogr)

  if (!vector_init_proj == newprojection) {
    dir_vector_reproj <- dirname(path_vector_reproj)
    name_vector_reproj <- file_path_sans_ext(basename(path_vector_reproj))
    vector_reproj <- sp::spTransform(vector_init_ogr, newprojection)
    rgdal::writeOGR(obj = vector_reproj, dsn = dir_vector_reproj, layer = name_vector_reproj,
                    driver = "ESRI Shapefile", overwrite_layer = TRUE)
    path_vector <- path_vector_reproj
  } else {
    path_vector <- path_vector_init
  }
  return(path_vector)
}


#" perform atmospheric corrections to convert L1C to L2A data with Sen2cor
#"
#" @param prodname character. produced with sen2r::s2_list
#" @param l1c_path character. path of directory where L1C image is stored
#" @param l2a_path character. path of directory where L2A image is stored
#" @param datepattern character. pattern corresponding to date of acquisition to identify L2A directory
#" @param tmp_path character. path of temporary directory where L2A image is stored
#" sen2r configured as an alternative hub for S2 download
#"
#" @return pathl2a character. S2 Product name
#" @importFrom sen2r safe_is_online s2_list s2_download s2_order
#" @importFrom R.utils getAbsolutePath
#"
#" @export
s2_from_l1c_to_l2a <- function(prodname, l1c_path, l2a_path, datepattern, tmp_path = NULL) {

  # define path for tmp directory
  if (is.null(tmp_path)) {
    tmp_path <- tempdir(check = TRUE)
  }
  tmp_prodlist <- prodname
  # perform Sen2Cor atmospheric corrections
  binpath <- sen2r::load_binpaths()
  # 2- open a command prompt and directly run sen2cor with following command line
  cmd <- paste(binpath$sen2cor,
               "--output_dir", R.utils::getAbsolutePath(l2a_path),
               R.utils::getAbsolutePath(file.path(l1c_path, prodname)), sep = " ")
  system(cmd)
  pathl2a <- list.files(path = l2a_path, pattern = datepattern, full.names = TRUE)

  return(pathl2a)
}

#" This function saves cloud masks.
#" "cloudMask_Binary" is default binary mask with 0 where clouds are detected and 1 for clean pixels
#" "cloudMask_RAW" is the original cloud layer produced by atmospheric correction algorithm
#" --> may be useful to refine cloud mask
#"
#" @param s2_stars list. stars object containing raster data. Can be produced with function extract_from_s2_l2a
#" @param cloud_path character.
#" @param s2source character.
#" @param footprint character. path for vector file defining footprint of interest in the image
#" @param saveraw boolean. should the original cloud mask layer be saved?
#" @param maxchunk numeric. Size of individual chunks to be written (in Mb)
#"
#" @return list of cloudmasks (binary mask, and raw mask if required)
#" @importFrom sf st_read
#" @importFrom stars write_stars
#" @importFrom raster raster
#" @export
save_cloud_s2 <- function(s2_stars, cloud_path, s2source = "SAFE",
                          footprint = NULL, saveraw = FALSE, maxchunk = 256) {

  whichcloud <- which(names(s2_stars$attr) == "Cloud")
  # Save cloud mask
  if (saveraw == TRUE) {
    cloudraw <- file.path(cloud_path, "CloudMask_RAW")
    obj <- stars::read_stars(s2_stars$attr[whichcloud], proxy = TRUE)
    sizeobj <- dim(obj)[1] * dim(obj)[2] / (1024**2)
    nbchunks <- ceiling(sizeobj / maxchunk)
    stars::write_stars(obj,
                       dsn = cloudraw,
                       driver =  "ENVI",
                       type = "Byte",
                       chunk_size = c(dim(obj)[1], dim(obj)[2] / nbchunks),
                       progress = TRUE)
  } else {
    cloudraw <- NULL
  }
  # Save cloud mask as in biodivMapR (0 = clouds, 1 = pixel ok)
  cloudmask <- stars::read_stars(s2_stars$attr[whichcloud], proxy = FALSE)
  if (s2source == "SAFE" || s2source == "THEIA") {
    cloudy <- which(cloudmask[[1]] > 0)
    sunny <- which(cloudmask[[1]] == 0)
  } else if (s2source == "LaSRC") {
    cloudy <- which(is.na(cloudmask[[1]]))
    sunny <- which(cloudmask[[1]] == 1)
  }

  cloudmask[[1]][cloudy] <- 0
  cloudmask[[1]][sunny] <- 1
  cloudbin <- file.path(cloud_path, "CloudMask_Binary")
  stars::write_stars(cloudmask, dsn = cloudbin, driver =  "ENVI", type = "Byte", overwrite = TRUE)
  cloudmasks <- list("BinaryMask" = cloudbin, "RawMask" = cloudraw)
  # delete temporary file
  file.remove(s2_stars$attr[whichcloud])
  if (file.exists(paste(s2_stars$attr[whichcloud], ".hdr", sep = ""))) file.remove(paste(s2_stars$attr[whichcloud], ".hdr", sep = ""))
  gc()
  return(cloudmasks)
}

#" This function saves reflectance files
#"
#" @param s2_stars list. stars object containing raster data. Can be produced with function extract_from_s2_l2a
#" @param refl_path character. path for reflectance file to be stored
#" @param format character. file format for reflectance data
#" @param datatype character. data type (integer, float, 16bits, 32bits...)
#" @param s2sat character. Sentinel-2 mission ("2A" or "2B")
#" @param tile_s2 character. S2 tile name (2 numbers + 3 letters)
#" @param dateacq_s2 double. date of acquisition
#" @param MTD character. path for metadata file
#" @param MTD_MSI character. path for metadata MSI file
#" @param mtd_lasrc character. path for metadata LaSRC file
#" @param maxchunk numeric. Size of individual chunks to be written (in Mb)
#"
#" @return None
#" @importFrom stars write_stars st_apply
#" @importFrom XML xml
#" @export
save_reflectance_s2 <- function(s2_stars, refl_path, format = "ENVI", datatype = "Int16",
                                s2sat = NULL, tile_s2 = NULL, dateacq_s2 = NULL,
                                MTD = NULL, MTD_MSI = NULL, mtd_lasrc = NULL,
                                maxchunk = 256) {
  # identify if S2A or S2B, if possible
  s2mission <- check_s2mission(s2sat = s2sat, tile_s2 = tile_s2, dateacq_s2 = dateacq_s2)

  # define central wavelength corresponding to each spectral band
  if (s2mission == "2A") {
    wl_s2 <- list("B02" = 496.6, "B03" = 560.0, "B04" = 664.5,
                  "B05" = 703.9, "B06" = 740.2, "B07" = 782.5, "B08" = 835.1,
                  "B8A" = 864.8, "B11" = 1613.7, "B12" = 2202.4)
  } else if (s2mission == "2B") {
    wl_s2 <- list("B02" = 492.1, "B03" = 559.0, "B04" = 665.0,
                  "B05" = 703.8, "B06" = 739.1, "B07" = 779.7, "B08" = 833.0,
                  "B8A" = 864.0, "B11" = 1610.4, "B12" = 2185.7)
  }
  if (s2mission == "2A") {
    sensor <- "Sentinel_2A"
  } else if (s2mission == "2B") {
    sensor <- "Sentinel_2B"
  }

  # apply offset when necessary
  listbands_bis <- c("B2", "B3", "B4", "B5", "B6", "B7", "B8", "B8A", "B11", "B12")
  if (!is.null(MTD_MSI) && is.null(mtd_lasrc)) {
    # read XML file containing info about geometry of acquisition
    s2xml <- XML::xmlToList(MTD_MSI)
    xml_offset <- s2xml$General_Info$Product_Image_Characteristics$BOA_ADD_offset_VALUES_LIST
    bands <- lapply(s2xml$General_Info$Product_Image_Characteristics$Spectral_Information_List, "[[", 4)
    if (!is.null(xml_offset) && !is.null(bands)) {
      bandid  <- lapply(bands, "[[", 1)
      bandname  <- lapply(bands, "[[", 2)
      offset <- data.frame("bandname" = unlist(bandname),
                           "bandid" = unlist(bandid),
                           "offset" = unlist(lapply(xml_offset, "[[", 1)))
      selbands <- match(listbands_bis, offset$bandname)
      offset <- offset[selbands, ]
      boa_quantval <- as.numeric(s2xml$General_Info$Product_Image_Characteristics$QUANTIFICATION_VALUES_LIST$BOA_QUANTIFICATION_VALUE[1])
    } else {
      offset <- data.frame("bandname" = listbands_bis,
                           "bandid" = c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12),
                           "offset" = 0)
      boa_quantval <- 10000
    }
  } else if (!is.null(mtd_lasrc)) {
    # read XML file containing info about geometry of acquisition
    s2xml <- XML::xmlToList(mtd_lasrc)
    attributes_lasrc <- s2xml$bands[[14]]$.attrs
    attributes_lasrc_df <- data.frame(attributes_lasrc)
    if (match("add_offset", rownames(attributes_lasrc_df)) > 0 && match("scale_factor", rownames(attributes_lasrc_df)) > 0) {
      xml_offset <- as.numeric(attributes_lasrc[["add_offset"]])
      boa_quantval <- 1 / as.numeric(attributes_lasrc[["scale_factor"]])
      offset <- data.frame("bandname" = listbands_bis,
                           "bandid" = c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12),
                           "offset" = xml_offset)
    } else {
      offset <- data.frame("bandname" = listbands_bis,
                           "bandid" = c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12),
                           "offset" = 0)
      boa_quantval <- 10000
    }
  } else {
    offset <- data.frame("bandname" = listbands_bis,
                         "bandid" = c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12),
                         "offset" = 0)
    boa_quantval <- 10000
  }

  # identify where spectral bands are in the stars object
  stars_spectral <- list()
  starsnames <- names(s2_stars$attr)
  stars_spectral$bandname <- starsnames[which(!starsnames == "Cloud")]
  stars_spectral$wavelength <- wl_s2[stars_spectral$bandname]

  sortedwl <- names(wl_s2)
  reorder <- match(sortedwl, stars_spectral$bandname)
  elim <- which(is.na(reorder))
  if (length(elim) > 0) {
    reorder <- reorder[-elim]
  }
  pathr <- s2_stars$attr[reorder]

  names(pathr) <- NULL
  s2_stars2 <- stars::read_stars(pathr, along = "band", proxy = TRUE)
  stars_spectral$bandname <- stars_spectral$bandname[reorder]
  stars_spectral$wavelength <- stars_spectral$wavelength[reorder]

  uniqueoffset <- as.numeric(unique(offset$offset))
  if (length(uniqueoffset) > 1) {
    message("Warning: BOA offset differs between bands.")
    message("offset will not be applied to the final S2 reflectance raster")
    message("check metadata file to identify the offset applied on each band")
    print(MTD_MSI)
  } else {
    message("applying offset to reflectance data")
    if (is.null(mtd_lasrc) || uniqueoffset == 0) {
      offsets2 <- function(x) (round(x + uniqueoffset) * (10000 / boa_quantval))
      s2_stars2 <- stars::st_apply(X = s2_stars2, MARGIN = "band", FUN = offsets2)
    } else {
      offsets2 <- function(x) (round(10000 * ((x + uniqueoffset * boa_quantval) / boa_quantval)))
      s2_stars2 <- stars::st_apply(X = s2_stars2, MARGIN = "band", FUN = offsets2)
    }
  }
  write_stack_s2(stars_s2 = s2_stars2, stars_spectral = stars_spectral, refl_path = refl_path,
                 format = format, datatype = datatype, sensor = sensor, maxchunk = maxchunk)
  # save metadata file as well if available
  if (!is.null(MTD)) {
    if (file.exists(MTD)) {
      file.copy(from = MTD, to = file.path(dirname(refl_path), basename(MTD)), overwrite = TRUE)
    }
  }
  # save metadata file as well if available
  if (!is.null(MTD_MSI)) {
    if (file.exists(MTD_MSI)) {
      file.copy(from = MTD_MSI, to = file.path(dirname(refl_path), basename(MTD_MSI)), overwrite = TRUE)
    }
  }
  # save LaSRC metadata file as well if available
  if (!is.null(mtd_lasrc)) {
    if (file.exists(mtd_lasrc)) {
      file.copy(from = mtd_lasrc, to = file.path(dirname(refl_path), basename(mtd_lasrc)), overwrite = TRUE)
    }
  }
  # delete temporary file
  for (pathtemp in pathr) {
    file.remove(pathtemp)
    if (file.exists(paste(pathtemp, ".hdr", sep = ""))) file.remove(paste(pathtemp, ".hdr", sep = ""))
  }
  gc()
  return(invisible())
}

#" ENVI functions
#"
#" based on https://github.com/cran/hyperSpec/blob/master/R/read.ENVI.R
#" added wavelength, fwhm, ... to header reading
#" Title
#"
#" @param x character.
#" @param separator character
#" @param trim_blank boolean.
#"
#" @return list.
#" @export
split_line <- function(x, separator, trim_blank = TRUE) {
  tmp <- regexpr(separator, x)
  key <- substr(x, 1, tmp - 1)
  value <- substr(x, tmp + 1, nchar(x))
  if (trim_blank) {
    blank_pattern <- "^[[:blank:]]*([^[:blank:]]+.*[^[:blank:]]+)[[:blank:]]*$"
    key <- sub(blank_pattern, "\\1", key)
    value <- sub(blank_pattern, "\\1", value)
  }
  value <- as.list(value)
  names(value) <- key
  return(value)
}

#" save raster footprint as vector file
#"
#" @param path_raster character. path for a raster file
#" @param path_vector character. path for a vector file
#" @param driver character. driver for vector
#"
#" @return None
#" @importFrom raster raster extent projection
#" @importFrom sf st_as_sf st_write
#" @export
vectorize_raster_extent <- function(path_raster, path_vector, driver = "ESRI Shapefile") {
  rast <- raster(path_raster)
  e <- extent(rast)
  # coerce to a SpatialPolygons object
  p <- as(e, "SpatialPolygons")
  projection(p) <- projection(rast)
  p <- sf::st_as_sf(p)
  sf::st_write(obj = p, path_vector, driver = driver)  # create to a shapefile
  return(invisible())
}

#" writes ENVI hdr file
#"
#" @param hdr content to be written
#" @param hdrpath Path of the hdr file
#"
#" @return None
#" @importFrom stringr str_count
#" @export
write_envi_header <- function(hdr, hdrpath) {
  h <- lapply(hdr, function(x) {
    if (length(x) > 1 || (is.character(x) && stringr::str_count(x, "\\w+") > 1)) {
      x <- paste0("{", paste(x, collapse = ", "), "}")
    }
    # convert last numerics
    x <- as.character(x)
  })
  writeLines(c("ENVI", paste(names(hdr), h, sep = " = ")), con = hdrpath)
  return(invisible())
}

#" This function writes a raster Stack object into a ENVI raster file
#"
#" @param stackobj list. raster stack
#" @param stackpath character. path where to store the stack
#" @param bands list. should include "bandname", and if possible "wavelength"
#" @param datatype character. should be INT2S or FLT4S for example
#" @param sensor character. Name of the sensor used to acquire the image
#" @param stretch boolean. Set TRUE to get 10% stretching at display for reflectance, mentioned in hdr only
#"
#" @return None
#" @importFrom utils read.table
#" @export
write_rasterstack_envi <- function(stackobj, stackpath, bands, datatype = "INT2S",
                                   sensor = "Unknown", stretch = FALSE) {

  r <- raster::writeRaster(x = stackobj, filename = stackpath, format = "Ehdr", overwrite = TRUE, datatype = datatype)
  raster::hdr(r, format = "ENVI")
  # Edit hdr file to add metadata
  hdr <- read_ENVI_header(get_hdr_name(stackpath))
  hdr$`band names` <- bands$bandname
  if (length(bands$wavelength) == length(bands$bandname)) {
    hdr$wavelength <- bands$wavelength
  } else {
    hdr$wavelength <- NULL
  }
  if (stretch == TRUE) {
    hdr$`default stretch` <- "0.000000 1000.000000 linear"
  }
  hdr$`z plot range` <- NULL
  hdr$`data ignore value` <- "-Inf"
  hdr$`coordinate system string` <- read.table(paste(stackpath, ".prj", sep = ""))
  proj <- strsplit(x = strsplit(x = projection(stackobj), split = " ")[[1]][1], split = "=")[[1]][2]
  zone <- strsplit(x = strsplit(x = projection(stackobj), split = " ")[[1]][2], split = "=")[[1]][2]
  datum <- strsplit(x = strsplit(x = projection(stackobj), split = " ")[[1]][3], split = "=")[[1]][2]
  oldproj <- hdr$`map info`
  newproj <- gsub(pattern = "projection", replacement = proj, x = oldproj)
  newproj <- paste(newproj, zone, datum, sep = ", ")
  hdr$`map info` <- newproj
  hdr$`sensor type` <- sensor
  write_envi_header(hdr = hdr, hdrpath = get_hdr_name(stackpath))

  # remove unnecessary files
  file2remove <- paste(stackpath, ".aux.xml", sep = "")
  file.remove(file2remove)
  file2remove <- paste(stackpath, ".prj", sep = "")
  file.remove(file2remove)
  file2remove <- paste(stackpath, ".stx", sep = "")
  file.remove(file2remove)
  return(invisible())
}


#" This function writes a stars object into a raster file
#"
#" @param stars_s2 list. stars object containing raster data. Can be produced with function Crop_n_resample_S2
#" @param stars_spectral list. band name to be saved in the stack and spectral bands corresponding to the image
#" @param refl_path character. path where to store the image
#" @param format character. default = ENVI BSQ. otherwise use gdal drivers
#" @param datatype character. should be Int16 or Float64 for example
#" @param sensor character. Name of the sensor used to acquire the image
#" @param maxchunk numeric. Size of individual chunks to be written (in Mb)
#"
#" @return None
#" @export
write_stack_s2 <- function(stars_s2, stars_spectral, refl_path, format = "ENVI",
                           datatype = "Int16", sensor = "Unknown", maxchunk = 256) {

  # write raster file from proxy using chunks
  sizeobj <- 2 * dim(stars_s2)[1] * dim(stars_s2)[2] * dim(stars_s2)[3] / (1024**2)
  nbchunks <- ceiling(sizeobj / maxchunk)
  stars::write_stars(obj = stars_s2,
                     dsn = refl_path,
                     driver =  format,
                     type = datatype,
                     chunk_size = c(dim(stars_s2)[1], ceiling(dim(stars_s2)[2] / nbchunks)),
                     progress = TRUE)

  if (format == "ENVI") {
    adjust_envi_hdr(dsn = refl_path, bands = stars_spectral,
                    sensor = sensor, stretch = TRUE)
  }
  return(invisible())
}

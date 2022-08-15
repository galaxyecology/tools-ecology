# ============================================================================== =
# prosail
# Lib_spectralindices.R
# ============================================================================== =
# PROGRAMMERS:
# Jean-Baptiste FERET <jb.feret@teledetection.fr>
# Florian de BOISSIEU <fdeboiss@gmail.com>
# Copyright 2019/11 Jean-Baptiste FERET
# ============================================================================== =
# This Library includes aims at computing spectral indices from reflectance data
# ============================================================================== =

#" This function computes Area under curve for continuum removed reflectances
#" See Malenovský et al. (2013) for details
#" http://dx.doi.org/10.1016/j.rse.2012.12.015
#"
#" @param refl RasterBrick, RasterStack or list. Raster bands in the order of sensorbands.
#" @param sensorbands numeric. vector containing central wavelength for each spectral band in the image
#" @param aucminmax list. wavelengths of lower and upper boundaries ("CRmin" and "CRmax")
#" @param reflfactor numeric. multiplying factor used to write reflectance in image (==10000 for S2)
#"
#" @return aucval raster
#" @export
auc <- function(refl, sensorbands, aucminmax, reflfactor = 1) {

  aucbands <- list()
  aucbands[["CRmin"]] <- sensorbands[get_closest_bands(sensorbands, aucminmax[["CRmin"]])]
  aucbands[["CRmax"]] <- sensorbands[get_closest_bands(sensorbands, aucminmax[["CRmax"]])]
  bands <- get_closest_bands(sensorbands, aucbands)
  for (i in bands[["CRmin"]]:bands[["CRmax"]]) {
    if (is.na(match(i, bands))) {
      aucbands[[paste("B", i, sep = "")]] <- sensorbands[i]
    }
  }
  # compute continuum removal for all spectral bands
  CR <- cr_wl(refl = refl, sensorbands = sensorbands,
              crbands = aucbands, reflfactor = reflfactor)

  wl <- sort(unlist(aucbands), decreasing = FALSE)
  aucval <- 0.5 * (1 - CR[[1]]) * (wl[2] - wl[1])
  for (i in 2:length(CR)) {
    aucval <- aucval + 0.5 * (2 - CR[[i - 1]] - CR[[i]]) * (wl[i + 1] - wl[i])
  }
  aucval <- aucval + 0.5 * (1 - CR[[length(CR)]]) * (wl[i + 2] - wl[i + 1])
  return(aucval)
}

#" This function extracts boundaries to be used to compute continuum from reflectance data
#"
#" @param refl RasterBrick, RasterStack or list. Raster bands in the order of sensorbands.
#" @param sensorbands numeric. vector containing central wavelength for each spectral band in the image
#" @param crbands list. list of spectral bands (central wavelength) including CRmin and CRmax
#" @param reflfactor numeric. multiplying factor used to write reflectance in image ( == 10000 for S2)
#"
#" @return crminmax list. list of rasters corresponding to minimum and maximum wavelengths
#" @export
crbound <- function(refl,  sensorbands,  crbands,  reflfactor = 1) {

  # get closest spectral bands from CR1 and CR2
  bands <- get_closest_bands(sensorbands, list(crbands[["CRmin"]], crbands[["CRmax"]]))
  wl <- sensorbands[bands]
  # get equation for line going from CR1 to CR2
  crminmax <- readrasterbands(refl = refl,  bands = bands,  reflfactor = reflfactor)
  names(crminmax) <- paste("wl_", as.character(wl), sep = "")
  return(crminmax)
}

#" This function extracts boundaries to be used to compute continuum from reflectance data
#"
#" @param refl RasterBrick,  RasterStack or list. Raster bands in the order of sensorbands.
#" @param sensorbands numeric. vector containing central wavelength for each spectral band in the image
#" @param crbands list. list of spectral bands (central wavelength) including CRmin and CRmax
#" @param reflfactor numeric. multiplying factor used to write reflectance in image ( == 10000 for S2)
#"
#" @return outlier_iqr numeric. band numbers of original sensor corresponding to S2
#" @importFrom progress progress_bar
#" @export
cr_wl <- function(refl,  sensorbands,  crbands,  reflfactor = 1) {

  # Make sure CRmin and CRmax are correctly defined
  if (is.na(match("CRmin", names(crbands))) || is.na(match("CRmax", names(crbands)))) {
    stop("Please define CRmin and CRmax (CRmin<CRmax) as spectral bands in crbands")
  }
  if (crbands[["CRmax"]] < crbands[["CRmin"]]) {
    stop("Please define CRmin < CRmax in crbands")
  }
  # extract CRmin and CRmax
  crminmax <- crbound(refl,  sensorbands,  crbands,  reflfactor = reflfactor)
  # extract other bands and compute CR
  CRmin <- sensorbands[get_closest_bands(sensorbands, crbands[["CRmin"]])]
  CRmax <- sensorbands[get_closest_bands(sensorbands, crbands[["CRmax"]])]
  crbands[["CRmin"]] <- NULL
  crbands[["CRmax"]] <- NULL
  CR <- list()
  # initiate progress bar
  pgbarlength <- length(crbands)
  pb <- progress_bar$new(
    format = "Computing continuum removal [:bar] :percent in :elapsedfull, estimated time remaining :eta",
    total = pgbarlength,  clear = FALSE,  width = 100)
  # computation for each band
  for (band in crbands) {
    pb$tick()
    bandrank <- get_closest_bands(sensorbands, band)
    raster2cr <- readrasterbands(refl = refl,  bands = bandrank,  reflfactor = reflfactor)
    CR[[as.character(band)]] <- computecr(wlmin = CRmin,  wlmax = CRmax,
                                          wltarget = band,  boundaries = crminmax,
                                          target = raster2cr)
  }
  return(CR)
}

#" This function computes continuum removal value for a spectral band of interest,
#" based on lower and upper wavelengths corresponding to boundaries of the continuum
#"
#" @param wlmin numeric. wavelength of the spectral band corresponding to minimum boundary
#" @param wlmax numeric. wavelength of the spectral band corresponding to maximum boundary
#" @param wltarget numeric. wavelength of the spectral band for which CR is computed
#" @param boundaries list. raster objects corresponding to minimum and maximum wavelengths
#" @param target list. raster object corresponding target wavelength
#"
#" @return CR list. raster object corresponding to continuum removed value
#" @export
computecr <- function(wlmin,  wlmax,  wltarget,  boundaries,  target) {

  CR <- target / (boundaries[[1]] + (wltarget - wlmin) * (boundaries[[2]] - boundaries[[1]]) / (wlmax - wlmin))
  return(CR)
}

#" this function produces a spectral index from an expression defining a spectral index
#"
#" @param refl RasterBrick,  RasterStack or list. Raster bands in the order of sensorbands.
#" @param sensorbands numeric. wavelength in nanometers of the spectral bands of refl.
#" @param expressindex  character. expression corresponding to the spectral index to compute
#" @param listbands list. list of spectral bands defined in the "expressindex" variable
#" @param reflfactor numeric. multiplying factor used to write reflectance in image ( == 10000 for S2)
#" @param nameindex character. name for the index to be computed,  provided in the raster layer
#"
#" @return numeric. band numbers of original sensor corresponding to S2
#" @export
computespectralindices_fromexpression <- function(refl,  sensorbands,  expressindex,  listbands,  reflfactor = 1, nameindex = NULL) {

  # define which bands to be used in the spectral index
  bands <- get_closest_bands(sensorbands, listbands)

  classraster <- class(refl)
  if (classraster == "RasterBrick" || classraster == "RasterStack" || classraster == "stars") {
    # if !reflfactor  ==  1 then apply a reflectance factor
    if (classraster == "stars") {
      refl <- refl[bands]
    } else {
      refl <- raster::subset(refl, bands)
    }
    if (!reflfactor == 1) {
      refl <- refl / reflfactor
    }
  } else if (is.list(refl)) {
    refl <- raster::stack(refl[bands]) # checks that all rasters have same crs/extent
    if (!reflfactor == 1) {
      refl <- refl / reflfactor
    }
  } else {
    stop("refl is expected to be a RasterStack,  RasterBrick,  Stars object or a list of rasters")
  }
  names(refl) <- gsub(pattern = "B", replacement = "Band", x = names(bands))

  nbbands <- unique(as.numeric(gsub(pattern = "B",
                                    replacement = "",
                                    x =  unlist(regmatches(expressindex,
                                                           gregexpr("B[[:digit:]] + ",
                                                                    expressindex))))))
  sortband <- sort(nbbands, index.return = TRUE, decreasing = TRUE)
  matches <- unique(unlist(regmatches(expressindex,  gregexpr("B[[:digit:]] + ",  expressindex))))[sortband$ix]
  replaces <- paste("refl[['Band", gsub(pattern = "B", replacement = "", x = matches), "']]", sep = "")
  expressindex_final <- expressindex
  for (bb in 1:length(matches)) {
    expressindex_final <- gsub(pattern = matches[bb],  replacement = replaces[bb],  x = expressindex_final)
  }
  SI <- eval(parse(text = expressindex_final))
  if (!is.null(nameindex)) {
    names(SI) <- nameindex
  }
  return(SI)
}

#" this function aims at computing spectral indices from Sensor reflectance data in raster object
#" it computes the spectral indices based on their computation with Sentinel-2
#" and assumes that the bands of the S2 data follow this order
#" wavelength	= {496.6,  560.0,  664.5,  703.9,  740.2,  782.5,  835.1,  864.8,  1613.7,  2202.4}
#" Full description of the indices can be found here:
#" https://www.sentinel-hub.com/eotaxonomy/indices
#"
#" @param refl RasterBrick,  RasterStack or list. Raster bands in the order of sensorbands.
#" @param sensorbands numeric. wavelength in nanometers of the spectral bands of refl.
#" @param sel_indices  list. list of spectral indices to be computed
#" @param stackout logical. If TRUE returns a stack,  otherwise a list of rasters.
#" @param reflfactor numeric. multiplying factor used to write reflectance in image ( == 10000 for S2)
#"
#" @return list. includes
#" - spectralindices: List of spectral indices computed from the reflectance initially provided
#" - listindices: list of spectral indices computable with the function
#" @importFrom methods is
#" @importFrom raster stack brick
#" @export

computespectralindices_raster <- function(refl,  sensorbands,  sel_indices = "ALL",  stackout = TRUE, reflfactor = 1) {

  s2bands <- c("B2" = 496.6, "B3" = 560.0, "B4" = 664.5, "B5" = 703.9, "B6" = 740.2,
               "B7" = 782.5, "B8" = 835.1, "B8A" = 864.8, "B11" = 1613.7, "B12" = 2202.4)

  spectralindices <- list()
  sen2s2 <- get_closest_bands(sensorbands, s2bands)
  classraster <- class(refl)
  if (classraster == "RasterBrick" || classraster == "RasterStack" || classraster == "stars") {
    # if !reflfactor  ==  1 then apply a reflectance factor
    if (classraster == "stars") {
      refl <- refl[sen2s2]
    } else {
      refl <- raster::subset(refl, sen2s2)
    }
    if (!reflfactor == 1) {
      refl <- refl / reflfactor
    }
  } else if (is.list(refl)) {
    refl <- raster::stack(refl[sen2s2]) # checks that all rasters have same crs/extent
    if (!reflfactor == 1) {
      refl <- refl / reflfactor
    }
  } else {
    stop("refl is expected to be a RasterStack, RasterBrick, Stars object or a list of rasters")
  }
  names(refl) <- names(sen2s2)

  indexall <- list()

  # inelegant but meeeeh
  listindices <- list("ARI1", "ARI2", "ARVI", "BAI", "BAIS2", "CCCI", "CHL_RE", "CRI1", "CRI2", "EVI", "EVI2",
                      "GRVI1", "GNDVI", "IRECI", "LAI_SAVI", "MCARI", "mNDVI705", "MSAVI2",
                      "MSI", "mSR705", "MTCI", "nBR_RAW", "NDI_45", "NDII", "NDSI", "NDVI", "NDVI_G",
                      "NDVI705", "NDWI1", "NDWI2", "PSRI", "PSRI_NIR", "RE_NDVI", "RE_NDWI", "S2REP",
                      "SAVI", "SIPI", "SR", "CR_SWIR")
  if (sel_indices[1] == "ALL") {
    sel_indices <- listindices
  }
  if ("ARI1" %in% sel_indices) {
    ARI1 <- (1 / refl[["B3"]]) - (1 / refl[["B5"]])
    spectralindices$ARI1 <- ARI1
  }
  if ("ARI2" %in% sel_indices) {
    ARI2 <- (refl[["B8"]] / refl[["B2"]]) - (refl[["B8"]] / refl[["B3"]])
    spectralindices$ARI2 <- ARI2
  }
  if ("ARVI" %in% sel_indices) {
    ARVI <- (refl[["B8"]] - (2 * refl[["B4"]] - refl[["B2"]])) / (refl[["B8"]] + (2 * refl[["B4"]] - refl[["B2"]]))
    spectralindices$ARVI <- ARVI
  }
  if ("BAI" %in% sel_indices) {
    BAI <- (1 / ((0.1 - refl[["B4"]])**2 + (0.06 - refl[["B8"]])**2))
    spectralindices$BAI <- BAI
  }
  if ("BAIS2" %in% sel_indices) {
    BAIS2 <-  (1 - ((refl[["B6"]] * refl[["B7"]] * refl[["B8A"]]) / refl[["B4"]])**0.5) * ((refl[["B12"]] - refl[["B8A"]]) / ((refl[["B12"]] + refl[["B8A"]])**0.5) + 1)
    spectralindices$BAIS2 <- BAIS2
  }
  if ("CCCI" %in% sel_indices) {
    CCCI <- ((refl[["B8"]] - refl[["B5"]]) / (refl[["B8"]] + refl[["B5"]])) / ((refl[["B8"]] - refl[["B4"]]) / (refl[["B8"]] + refl[["B4"]]))
    spectralindices$CCCI <- CCCI
  }
  if ("CHL_RE" %in% sel_indices) {
    CHL_RE <- refl[["B5"]] / refl[["B8"]]
    spectralindices$CHL_RE <- CHL_RE
  }
  if ("CRI1" %in% sel_indices) {
    CRI1 <- (1 / refl[["B2"]]) - (1 / refl[["B3"]])
    spectralindices$CRI1 <- CRI1
  }
  if ("CRI2" %in% sel_indices) {
    CRI2 <- (1 / refl[["B2"]]) - (1 / refl[["B5"]])
    spectralindices$CRI2 <- CRI2
  }
  if ("EVI" %in% sel_indices) {
    EVI <- 2.5 * (refl[["B8"]] - refl[["B4"]]) / ((refl[["B8"]] + 6 * refl[["B4"]] - 7.5 * refl[["B2"]] + 1))
    spectralindices$EVI <- EVI
  }
  if ("EVI2" %in% sel_indices) {
    EVI2 <- 2.5 * (refl[["B8"]] - refl[["B4"]]) / (refl[["B8"]] + 2.4 * refl[["B4"]] + 1)
    spectralindices$EVI2 <- EVI2
  }
  if ("GRVI1" %in% sel_indices) {
    GRVI1 <- (refl[["B4"]] - refl[["B3"]]) / (refl[["B4"]] + refl[["B3"]])
    spectralindices$GRVI1 <- GRVI1
  }
  if ("GNDVI" %in% sel_indices) {
    GNDVI <- (refl[["B8"]] - refl[["B3"]]) / (refl[["B8"]] + refl[["B3"]])
    spectralindices$GNDVI <- GNDVI
  }
  if ("IRECI" %in% sel_indices) {
    IRECI <- (refl[["B7"]] - refl[["B4"]]) * (refl[["B6"]]/(refl[["B5"]]))
    spectralindices$IRECI <- IRECI
  }
  if ("LAI_SAVI" %in% sel_indices) {
    LAI_SAVI <- - log(0.371  +  1.5 * (refl[["B8"]] - refl[["B4"]]) / (refl[["B8"]] +  refl[["B4"]] +  0.5)) / 2.4
    spectralindices$LAI_SAVI <- LAI_SAVI
  }
  if  ("MCARI" %in% sel_indices) {
    MCARI <- (1 - 0.2 * (refl[["B5"]] - refl[["B3"]]) / (refl[["B5"]] - refl[["B4"]]))
    spectralindices$MCARI <- MCARI
  }
  if ("mNDVI705" %in% sel_indices) {
    mNDVI705 <- (refl[["B6"]] - refl[["B5"]]) / (refl[["B6"]] + refl[["B5"]] - 2 * refl[["B2"]])
    spectralindices$mNDVI705 <- mNDVI705
  }
  if ("MSAVI2" %in% sel_indices) {
    MSAVI2 <- ((refl[["B8"]] + 1) - 0.5 * sqrt(((2 * refl[["B8"]]) - 1)**2 + 8 * refl[["B4"]]))
    spectralindices$MSAVI2 <- MSAVI2
  }
  if ("MSI" %in% sel_indices) {
    MSI <- refl[["B11"]] / refl[["B8A"]]
    spectralindices$MSI <- MSI
  }
  if ("mSR705" %in% sel_indices) {
    mSR705 <- (refl[["B6"]] - refl[["B2"]]) / (refl[["B5"]] - refl[["B2"]])
    spectralindices$mSR705 <- mSR705
  }
  if ("MTCI" %in% sel_indices) {
    MTCI <- (refl[["B6"]] - refl[["B5"]]) / (refl[["B5"]] + refl[["B4"]])
    spectralindices$MTCI <- MTCI
  }
  if ("nBR_RAW" %in% sel_indices) {
    nBR_RAW <- (refl[["B8"]] - refl[["B12"]]) / (refl[["B8"]] + refl[["B12"]])
    spectralindices$nBR_RAW <- nBR_RAW
  }
  if ("NDI_45" %in% sel_indices) {
    NDI_45 <- (refl[["B5"]] - refl[["B4"]]) / (refl[["B5"]] + refl[["B4"]])
    spectralindices$NDI_45 <- NDI_45
  }
  if ("NDII" %in% sel_indices) {
    NDII <- (refl[["B8A"]] - refl[["B11"]]) / (refl[["B8A"]] + refl[["B11"]])
    spectralindices$NDII <- NDII
  }
  if ("NDSI" %in% sel_indices) {
    NDSI <- (refl[["B3"]] - refl[["B11"]]) / (refl[["B3"]] + refl[["B11"]])
    spectralindices$NDSI <- NDSI
  }
  if ("NDVI" %in% sel_indices) {
    NDVI <- (refl[["B8"]] - refl[["B4"]]) / (refl[["B8"]] + refl[["B4"]])
    spectralindices$NDVI <- NDVI
  }
  if ("NDVI_G" %in% sel_indices) {
    NDVI_G <- refl[["B3"]] * (refl[["B8"]] - refl[["B4"]]) / (refl[["B8"]] + refl[["B4"]])
    spectralindices$NDVI_G <- NDVI_G
  }
  if ("NDVI705" %in% sel_indices) {
    NDVI705 <- (refl[["B6"]] - refl[["B5"]]) / (refl[["B6"]] + refl[["B5"]])
    spectralindices$NDVI705 <- NDVI705
  }
  if ("NDWI" %in% sel_indices) {
    NDWI <- (refl[["B3"]] - refl[["B8"]]) / (refl[["B3"]] + refl[["B8"]])
    spectralindices$NDWI <- NDWI
  }
  if ("NDWI1" %in% sel_indices) {
    NDWI1 <- (refl[["B8A"]] - refl[["B11"]]) / (refl[["B8A"]] + refl[["B11"]])
    spectralindices$NDWI1 <- NDWI1
  }
  if ("NDWI2" %in% sel_indices) {
    NDWI2 <- (refl[["B8A"]] - refl[["B12"]]) / (refl[["B8A"]] + refl[["B12"]])
    spectralindices$NDWI2 <- NDWI2
  }
  if ("PSRI" %in% sel_indices) {
    PSRI <- (refl[["B4"]] - refl[["B2"]]) / (refl[["B5"]])
    spectralindices$PSRI <- PSRI
  }
  if ("PSRI_NIR" %in% sel_indices) {
    PSRI_NIR <- (refl[["B4"]] - refl[["B2"]]) / (refl[["B8"]])
    spectralindices$PSRI_NIR <- PSRI_NIR
  }
  if ("RE_NDVI" %in% sel_indices) {
    RE_NDVI <- (refl[["B8"]] - refl[["B6"]]) / (refl[["B8"]] + refl[["B6"]])
    spectralindices$RE_NDVI <- RE_NDVI
  }
  if ("RE_NDWI" %in% sel_indices) {
    RE_NDWI <- (refl[["B4"]] - refl[["B6"]]) / (refl[["B4"]] + refl[["B6"]])
    spectralindices$RE_NDWI <- RE_NDWI
  }
  if ("S2REP" %in% sel_indices) {
    S2REP <- 705 + 35 * (0.5 * (refl[["B8"]] + refl[["B5"]]) - refl[["B6"]]) / (refl[["B7"]] - refl[["B6"]])
    spectralindices$S2REP <- S2REP
  }
  if ("SAVI" %in% sel_indices) {
    SAVI <- 1.5 * (refl[["B8"]] - refl[["B5"]]) / (refl[["B8"]] + refl[["B5"]] + 0.5)
    spectralindices$SAVI <- SAVI
  }
  if ("SIPI" %in% sel_indices) {
    SIPI <- (refl[["B8"]] - refl[["B2"]]) / (refl[["B8"]] - refl[["B4"]])
    spectralindices$SIPI <- SIPI
  }
  if ("SR" %in% sel_indices) {
    SR <- refl[["B8"]] / refl[["B4"]]
    spectralindices$SR <- SR
  }
  if ("TCARI" %in% sel_indices) {
    SR <- refl[["B8"]] / refl[["B4"]]
    spectralindices$SR <- SR
  }
  if ("CR_SWIR" %in% sel_indices) {
    CR_SWIR <- refl[["B11"]] / (refl[["B8A"]] + (s2bands["B11"] - s2bands["B8A"]) * (refl[["B12"]] - refl[["B8A"]]) / (s2bands["B12"] - s2bands["B8A"]))
    spectralindices$CR_SWIR <- CR_SWIR
  }

  if (stackout)
    spectralindices <- raster::stack(spectralindices)

  res <- list("spectralindices" = spectralindices, "listindices" = listindices)
  return(res)
}

#" this function aims at computing spectral indices from Sensor reflectance data.
#" it computes the spectral indices based on their computation with Sentinel-2
#" and assumes that the bands of the S2 data follow this order
#" wavelength	= {496.6,  560.0,  664.5,  703.9,  740.2,  782.5,  835.1,  864.8,  1613.7,  2202.4}
#" Full description of the indices can be found here:
#" https://www.sentinel-hub.com/eotaxonomy/indices
#"
#" @param refl numeric. reflectance dataset defined in matrix
#" @param sel_indices list. list of spectral indices to be computed
#" @param sensorbands numeric. wavelength of the spectral bands corresponding to the spectral index
#"
#" @return list. includes
#" - spectralindices: List of spectral indices computed from the reflectance initially provided
#" - listindices: list of spectral indices computable with the function
#" @export

computespectralindices_hs <- function(refl, sensorbands, sel_indices = "ALL") {

  spectralindices <- list()
  s2bands <- data.frame("B2" = 496.6, "B3" = 560.0, "B4" = 664.5, "B5" = 703.9, "B6" = 740.2,
               "B7" = 782.5, "B8" = 835.1, "B8A" = 864.8, "B11" = 1613.7, "B12" = 2202.4)

  sen2s2 <- get_closest_bands(sensorbands, s2bands)
  indexall <- list()
  # set zero vaues to >0 in order to avoid problems
  selzero <- which(refl == 0)
  refl[selzero] <- 0.005
  if (dim(refl)[1] == length(sensorbands)) {
    refl <- t(refl)
  }

  # inelegant but meeeeh
  listindices <- list("ARI1", "ARI2", "ARVI", "BAI", "BAIS2", "CHL_RE", "CRI1", "CRI2", "EVI", "EVI2",
                      "GRVI1", "GNDVI", "IRECI", "LAI_SAVI", "MCARI", "mNDVI705", "MSAVI2",
                      "MSI", "mSR705", "MTCI", "nBR_RAW", "NDI_45", "NDII", "NDVI", "NDVI_G",
                      "NDVI705", "NDWI1", "NDWI2", "PSRI", "PSRI_NIR", "RE_NDVI", "RE_NDWI", "S2REP",
                      "SAVI", "SIPI", "SR", "CR_SWIR")
  if (sel_indices == "ALL") {
    sel_indices = listindices
  }
  if ("ARI1" %in% sel_indices) {
    ARI1 <- (1 / refl[, sen2s2[["B3"]]]) - (1 / refl[, sen2s2[["B5"]]])
    spectralindices$ARI1 <- ARI1
  }
  if ("ARI2" %in% sel_indices) {
    ARI2 <- (refl[, sen2s2[["B8"]]] / refl[, sen2s2[["B2"]]]) - (refl[, sen2s2[["B8"]]] / refl[, sen2s2[["B3"]]])
    spectralindices$ARI2 <- ARI2
  }
  if ("ARVI" %in% sel_indices) {
    ARVI <- (refl[, sen2s2[["B8"]]] - (2 * refl[, sen2s2[["B4"]]] - refl[, sen2s2[["B2"]]])) / (refl[, sen2s2[["B8"]]] + (2 * refl[, sen2s2[["B4"]]] - refl[, sen2s2[["B2"]]]))
    spectralindices$ARVI <- ARVI
  }
  if ("BAI" %in% sel_indices) {
    BAI <- (1 / ((0.1 - refl[, sen2s2[["B4"]]])**2 + (0.06 - refl[, sen2s2[["B8"]]])**2))
    spectralindices$BAI <- BAI
  }
  if ("BAIS2" %in% sel_indices) {
    BAIS2 <-  (1 - ((refl[, sen2s2[["B6"]]] * refl[, sen2s2[["B7"]]] * refl[, sen2s2[["B8A"]]]) / refl[, sen2s2[["B4"]]])**0.5) * ((refl[, sen2s2[["B12"]]] - refl[, sen2s2[["B8A"]]]) / ((refl[, sen2s2[["B12"]]] + refl[, sen2s2[["B8A"]]])**0.5) + 1)
    spectralindices$BAIS2 <- BAIS2
  }
  if ("CCCI" %in% sel_indices) {
    CCCI <- ((refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B5"]]]) / (refl[, sen2s2[["B8"]]] + refl[, sen2s2[["B5"]]])) / ((refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B4"]]]) / (refl[, sen2s2[["B8"]]] + refl[, sen2s2[["B4"]]]))
    spectralindices$CCCI <- CCCI
  }
  if ("CHL_RE" %in% sel_indices) {
    CHL_RE <- refl[, sen2s2[["B5"]]] / refl[, sen2s2[["B8"]]]
    spectralindices$CHL_RE <- CHL_RE
  }
  if ("CRI1" %in% sel_indices) {
    CRI1 <- (1 / refl[, sen2s2[["B2"]]]) - (1 / refl[, sen2s2[["B3"]]])
    spectralindices$CRI1 <- CRI1
  }
  if ("CRI2" %in% sel_indices) {
    CRI2 <- (1 / refl[, sen2s2[["B2"]]]) - (1 / refl[, sen2s2[["B5"]]])
    spectralindices$CRI2 <- CRI2
  }
  if ("EVI" %in% sel_indices) {
    EVI <- 2.5 * (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B4"]]]) / ((refl[, sen2s2[["B8"]]] + 6 * refl[, sen2s2[["B4"]]] - 7.5 * refl[, sen2s2[["B2"]]] + 1))
    spectralindices$EVI <- EVI
  }
  if ("EVI2" %in% sel_indices) {
    EVI2 <- 2.5 * (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B4"]]]) / (refl[, sen2s2[["B8"]]] + 2.4 * refl[, sen2s2[["B4"]]] + 1)
    spectralindices$EVI2 <- EVI2
  }
  if ("GRVI1" %in% sel_indices) {
    GRVI1 <- (refl[, sen2s2[["B4"]]] - refl[, sen2s2[["B3"]]]) / (refl[, sen2s2[["B4"]]] + refl[, sen2s2[["B3"]]])
    spectralindices$GRVI1 <- GRVI1
  }
  if ("GNDVI" %in% sel_indices) {
    GNDVI <- (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B3"]]]) / (refl[, sen2s2[["B8"]]] + refl[, sen2s2[["B3"]]])
    spectralindices$GNDVI <- GNDVI
  }
  if ("IRECI" %in% sel_indices) {
    IRECI <- (refl[, sen2s2[["B7"]]] - refl[, sen2s2[["B4"]]]) * (refl[, sen2s2[["B6"]]] / (refl[, sen2s2[["B5"]]]))
    spectralindices$IRECI <- IRECI
  }
  if ("LAI_SAVI" %in% sel_indices) {
    LAI_SAVI <- - log(0.371  +  1.5 * (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B4"]]]) / (refl[, sen2s2[["B8"]]] +  refl[, sen2s2[["B4"]]] +  0.5)) / 2.4
    spectralindices$LAI_SAVI <- LAI_SAVI
  }
  if  ("MCARI" %in% sel_indices) {
    MCARI <- (1 - 0.2 * (refl[, sen2s2[["B5"]]] - refl[, sen2s2[["B3"]]]) / (refl[, sen2s2[["B5"]]] - refl[, sen2s2[["B4"]]]))
    spectralindices$MCARI <- MCARI
  }
  if ("mNDVI705" %in% sel_indices) {
    mNDVI705 <- (refl[, sen2s2[["B6"]]] - refl[, sen2s2[["B5"]]]) / (refl[, sen2s2[["B6"]]] + refl[, sen2s2[["B5"]]] - 2 * refl[, sen2s2[["B2"]]])
    spectralindices$mNDVI705 <- mNDVI705
  }
  if ("MSAVI2" %in% sel_indices) {
    MSAVI2 <- ((refl[, sen2s2[["B8"]]] + 1) - 0.5 * sqrt(((2 * refl[, sen2s2[["B8"]]]) - 1)**2 + 8 * refl[, sen2s2[["B4"]]]))
    spectralindices$MSAVI2 <- MSAVI2
  }
  if ("MSI" %in% sel_indices) {
    MSI <- refl[, sen2s2[["B11"]]] / refl[, sen2s2[["B8"]]]
    spectralindices$MSI <- MSI
  }
  if ("mSR705" %in% sel_indices) {
    mSR705 <- (refl[, sen2s2[["B6"]]] - refl[, sen2s2[["B2"]]]) / (refl[, sen2s2[["B5"]]] - refl[, sen2s2[["B2"]]])
    spectralindices$mSR705 <- mSR705
  }
  if ("MTCI" %in% sel_indices) {
    MTCI <- (refl[, sen2s2[["B6"]]] - refl[, sen2s2[["B5"]]]) / (refl[, sen2s2[["B5"]]] + refl[, sen2s2[["B4"]]])
    spectralindices$MTCI <- MTCI
  }
  if ("nBR_RAW" %in% sel_indices) {
    nBR_RAW <- (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B12"]]]) / (refl[, sen2s2[["B8"]]] + refl[, sen2s2[["B12"]]])
    spectralindices$nBR_RAW <- nBR_RAW
  }
  if ("NDI_45" %in% sel_indices) {
    NDI_45 <- (refl[, sen2s2[["B5"]]] - refl[, sen2s2[["B4"]]]) / (refl[, sen2s2[["B5"]]] + refl[, sen2s2[["B4"]]])
    spectralindices$NDI_45 <- NDI_45
  }
  if ("NDII" %in% sel_indices) {
    NDII <- (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B11"]]]) / (refl[, sen2s2[["B8"]]] + refl[, sen2s2[["B11"]]])
    spectralindices$NDII <- NDII
  }
  if ("NDSI" %in% sel_indices) {
    NDSI <- (refl[, sen2s2[["B3"]]] - refl[, sen2s2[["B11"]]]) / (refl[, sen2s2[["B3"]]] + refl[, sen2s2[["B11"]]])
    spectralindices$NDSI <- NDSI
  }
  if ("NDVI" %in% sel_indices) {
    NDVI <- (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B4"]]]) / (refl[, sen2s2[["B8"]]] + refl[, sen2s2[["B4"]]])
    spectralindices$NDVI <- NDVI
  }
  if ("NDVI_G" %in% sel_indices) {
    NDVI_G <- refl[, sen2s2[["B3"]]] * (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B4"]]]) / (refl[, sen2s2[["B8"]]] + refl[, sen2s2[["B4"]]])
    spectralindices$NDVI_G <- NDVI_G
  }
  if ("NDVI705" %in% sel_indices) {
    NDVI705 <- (refl[, sen2s2[["B6"]]] - refl[, sen2s2[["B5"]]]) / (refl[, sen2s2[["B6"]]] + refl[, sen2s2[["B5"]]])
    spectralindices$NDVI705 <- NDVI705
  }
  if ("NDWI1" %in% sel_indices) {
    NDWI1 <- (refl[, sen2s2[["B8A"]]] - refl[, sen2s2[["B11"]]]) / (refl[, sen2s2[["B8A"]]] + refl[, sen2s2[["B11"]]])
    spectralindices$NDWI1 <- NDWI1
  }
  if ("NDWI2" %in% sel_indices) {
    NDWI2 <- (refl[, sen2s2[["B8A"]]] - refl[, sen2s2[["B12"]]]) / (refl[, sen2s2[["B8A"]]] + refl[, sen2s2[["B12"]]])
    spectralindices$NDWI2 <- NDWI2
  }
  if ("PSRI" %in% sel_indices) {
    PSRI <- (refl[, sen2s2[["B4"]]] - refl[, sen2s2[["B2"]]]) / (refl[, sen2s2[["B5"]]])
    spectralindices$PSRI <- PSRI
  }
  if ("PSRI_NIR" %in% sel_indices) {
    PSRI_NIR <- (refl[, sen2s2[["B4"]]] - refl[, sen2s2[["B2"]]]) / (refl[, sen2s2[["B8"]]])
    spectralindices$PSRI_NIR <- PSRI_NIR
  }
  if ("RE_NDVI" %in% sel_indices) {
    RE_NDVI <- (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B6"]]]) / (refl[, sen2s2[["B8"]]] + refl[, sen2s2[["B6"]]])
    spectralindices$RE_NDVI <- RE_NDVI
  }
  if ("RE_NDWI" %in% sel_indices) {
    RE_NDWI <- (refl[, sen2s2[["B4"]]] - refl[, sen2s2[["B6"]]]) / (refl[, sen2s2[["B4"]]] + refl[, sen2s2[["B6"]]])
    spectralindices$RE_NDWI <- RE_NDWI
  }
  if ("S2REP" %in% sel_indices) {
    S2REP <- 705 + 35 * (0.5 * (refl[, sen2s2[["B8"]]] + refl[, sen2s2[["B5"]]]) - refl[, sen2s2[["B6"]]]) / (refl[, sen2s2[["B7"]]] - refl[, sen2s2[["B6"]]])
    spectralindices$S2REP <- S2REP
  }
  if ("SAVI" %in% sel_indices) {
    SAVI <- 1.5 * (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B5"]]]) / (refl[, sen2s2[["B8"]]] + refl[, sen2s2[["B5"]]] + 0.5)
    spectralindices$SAVI <- SAVI
  }
  if ("SIPI" %in% sel_indices) {
    SIPI <- (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B2"]]]) / (refl[, sen2s2[["B8"]]] - refl[, sen2s2[["B4"]]])
    spectralindices$SIPI <- SIPI
  }
  if ("SR" %in% sel_indices) {
    SR <- refl[, sen2s2[["B8"]]] / refl[, sen2s2[["B4"]]]
    spectralindices$SR <- SR
  }
  if ("CR_SWIR" %in% sel_indices) {
    CR_SWIR <- refl[, sen2s2[["B11"]]] / (refl[, sen2s2[["B8A"]]] + (s2bands$B11 - s2bands$B8A) * (refl[, sen2s2[["B12"]]] - refl[, sen2s2[["B8A"]]]) / (s2bands$B12 - s2bands$B8A))
    spectralindices$CR_SWIR <- CR_SWIR
  }
  res <- list("spectralindices" = spectralindices, "listindices" = listindices)
  return(res)
}

#" this function identifies the bands of a given sensor with closest match to its spectral characteristics
#"
#" @param sensorbands numeric. wavelength in nanometer of the sensor of interest
#" @param listbands numeric or list. Named vector or list of spectral bands corresponding to sensor
#"
#" @return numeric. band numbers of original sensor
#" @export
get_closest_bands <- function(sensorbands, listbands) {
  sapply(listbands,  function(x) {
    b <- which.min(abs(sensorbands - x)); names(b) = ""; b
  })
}

#" This function computes interquartile range (IQR) criterion,  which can be used
#" as a criterion for outlier detection
#"
#" @param distval numeric. vector of distribution of values
#" @param weightirq numeric. weighting factor appplied to IRQ to define lower and upper boudaries for outliers
#"
#" @return outlier_iqr numeric. band numbers of original sensor corresponding to S2
#" @importFrom stats IQR quantile
#" @export
iqr_outliers <- function(distval, weightirq = 1.5) {
  iqr <- IQR(distval,  na.rm = TRUE)
  range_iqr <- c(quantile(distval,  0.25, na.rm = TRUE), quantile(distval,  0.75, na.rm = TRUE))
  outlier_iqr <- c(range_iqr[1] - weightirq * iqr, range_iqr[2] + weightirq * iqr)
  return(outlier_iqr)
}

#" This function selects bands from a raster or stars object
#"
#" @param refl RasterBrick,  RasterStack or list. Raster bands in the order of sensorbands.
#" @param bands numeric. rank of bands to be read in refl
#" @param reflfactor numeric. multiplying factor used to write reflectance in image ( == 10000 for S2)
#"
#" @return robj list. R object (default is raster,  stars if refl is stars object)
#" @importFrom raster subset stack
#" @export
readrasterbands <- function(refl,  bands,  reflfactor = 1) {

  # get equation for line going from CR1 to CR2
  classraster <- class(refl)
  if (classraster == "RasterBrick" || classraster == "RasterStack" || classraster == "stars") {
    # if !reflfactor  ==  1 then apply a reflectance factor
    if (classraster == "stars") {
      robj <- refl[bands]
    } else {
      robj <- raster::subset(refl, bands)
    }
    if (!reflfactor == 1) {
      robj <- robj / reflfactor
    }
  } else if (is.list(refl)) {
    robj <- raster::stack(refl[bands]) # checks that all rasters have same crs/extent
    if (!reflfactor == 1) {
      robj <- robj / reflfactor
    }
  } else {
    stop("refl is expected to be a RasterStack, RasterBrick, Stars object or a list of rasters")
  }
  return(robj)
}

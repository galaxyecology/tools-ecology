# ============================================================================= =
# prosail
# Lib_PROSAIL_HybridInversion.R
# ============================================================================= =
# PROGRAMMERS:
# Jean-Baptiste FERET <jb.feret@teledetection.fr>
# Florian de BOISSIEU <fdeboiss@gmail.com>
# Copyright 2019 / 11 Jean-Baptiste FERET
# ============================================================================= =
# This Library includes functions dedicated to PROSAIL inversion using hybrid
# approach based on SVM regression
# ============================================================================= =


#" This function applies SVR model on raster data in order to estimate
#" vegetation biophysical properties
#"
#" @param raster_path character. path for a raster file
#" @param hybridmodel list. hybrid models produced from train_prosail_inversion
#" each element of the list corresponds to a set of hybrid models for a vegetation parameter
#" @param pathout character. path for directory where results are written
#" @param selectedbands list. list of spectral bands to be selected from raster (identified by name of vegetation parameter)
#" @param bandname character. spectral bands corresponding to the raster
#" @param maskraster character. path for binary mask defining ON (1) and OFF (0) pixels in the raster
#" @param multiplyingfactor numeric. multiplying factor used to write reflectance in the raster
#" --> PROSAIL simulates reflectance between 0 and 1,  and raster data expected in the same range
#"
#" @return None
#" @importFrom progress progress_bar
#" @importFrom stars read_stars
#" @importFrom raster raster brick blockSize readStart readStop getValues writeStart writeStop writeValues
#" @import rgdal
#" @export
apply_prosail_inversion <- function(raster_path,  hybridmodel,  pathout, 
                                    selectedbands,  bandname, 
                                    maskraster = FALSE, multiplyingfactor = 10000) {

  # explain which biophysical variables will be computed
  bpvar <- names(hybridmodel)
  print("The following biophysical variables will be computed")
  print(bpvar)

  # get image dimensions
  if (attr(rgdal::GDALinfo(raster_path, returnStats = FALSE),  "driver") == "ENVI") {
    hdr <- read_envi_header(get_hdr_name(raster_path))
    dimsraster <- list("rows" = hdr$lines, "cols" = hdr$samples, "bands" = hdr$bands)
  } else {
    dimsraster <- dim(read_stars(raster_path))
    dimsraster <- list("rows" = as.numeric(dimsraster[2]), "cols" = as.numeric(dimsraster[1]), "bands" = as.numeric(dimsraster[3]))
  }

  # Produce a map for each biophysical property
  for (parm in bpvar) {
    print(paste("Computing", parm, sep = " "))
    # read by chunk to avoid memory problem
    blk <- blockSize(brick(raster_path))
    # reflectance file
    r_in <- readStart(brick(raster_path))
    # mask file
    r_inmask <- FALSE
    if (maskraster == FALSE) {
      selectpixels <- "ALL"
    } else if (!maskraster == FALSE) {
      if (file.exists(maskraster)) {
        r_inmask <- readStart(raster(maskraster))
      } else if (!file.exists(maskraster)) {
        message("WARNING: Mask file does not exist:")
        print(maskraster)
        message("Processing all image")
        selectpixels <- "ALL"
      }
    }
    # initiate progress bar
    pgbarlength <- length(hybridmodel[[parm]]) * blk$n
    pb <- progress_bar$new(
      format = "Hybrid inversion on raster [:bar] :percent in :elapsedfull, estimated time remaining :eta",
      total = pgbarlength,  clear = FALSE,  width = 100)

    # output files
    bpvarpath <- file.path(pathout, paste(basename(raster_path), parm, sep = "_"))
    bpvarsdpath <- file.path(pathout, paste(basename(raster_path), parm, "STD", sep = "_"))
    r_outmean <- writeStart(raster(raster_path),  filename = bpvarpath, format = "ENVI",  overwrite = TRUE)
    r_outsd <- writeStart(raster(raster_path),  filename = bpvarsdpath, format = "ENVI",  overwrite = TRUE)
    selbands <- match(selectedbands[[parm]], bandname)

    # loop over blocks
    for (i in seq_along(blk$row)) {
      # read values for block
      # format is a matrix with rows the cells values and columns the layers
      blockval <- getValues(r_in,  row = blk$row[i],  nrows = blk$nrows[i])
      fulllength <- dim(blockval)[1]

      if (typeof(r_inmask) == "logical") {
        blockval <- blockval[, selbands]
        # automatically filter pixels corresponding to negative values
        selectpixels <- which(blockval[, 1] > 0)
        blockval <- blockval[selectpixels, ]
      } else if (typeof(r_inmask) == "S4") {
        maskval <- getValues(r_inmask,  row = blk$row[i],  nrows = blk$nrows[i])
        selectpixels <- which(maskval  == 1)
        blockval <- blockval[selectpixels, selbands]
      }
      mean_estimatefull <- NA * vector(length = fulllength)
      std_estimatefull <- NA * vector(length = fulllength)
      if (length(selectpixels) > 0) {
        blockval <- blockval / multiplyingfactor
        modelsvr_estimate <- list()
        for (modind in 1:length(hybridmodel[[parm]])) {
          # print(c(i, modind))
          pb$tick()
          modelsvr_estimate[[modind]] <- predict(hybridmodel[[parm]][[modind]],  blockval)
        }
        modelsvr_estimate <- do.call(cbind, modelsvr_estimate)
        # final estimated value = mean parm value for all models
        mean_estimate <- rowMeans(modelsvr_estimate)
        # "uncertainty" = STD value for all models
        std_estimate <- rowSds(modelsvr_estimate)
        mean_estimatefull[selectpixels] <- mean_estimate
        std_estimatefull[selectpixels] <- std_estimate
      } else {
        for (modind in 1:length(hybridmodel[[parm]])) {
          pb$tick()
        }
      }
      r_outmean <- writeValues(r_outmean,  mean_estimatefull,  blk$row[i], format = "ENVI",  overwrite = TRUE)
      r_outsd <- writeValues(r_outsd,  std_estimatefull,  blk$row[i], format = "ENVI",  overwrite = TRUE)
    }
    # close files
    r_in <- readStop(r_in)
    if (typeof(r_inmask) == "S4") {
      r_inmask <- readStop(r_inmask)
    }
    r_outmean <- writeStop(r_outmean)
    r_outsd <- writeStop(r_outsd)
    # write biophysical variable name in headers
    hdr <- read_envi_header(get_hdr_name(bpvarpath))
    hdr$`band names` <- paste("{", parm, "}", sep = "")
    write_envi_header(hdr,  get_hdr_name(bpvarpath))
  }
  print("processing completed")
  return(invisible())
}

#" get hdr name from image file name,  assuming it is BIL format
#"
#" @param impath path of the image
#"
#" @return corresponding hdr
#" @importFrom tools file_ext file_path_sans_ext
#" @export
get_hdr_name <- function(impath) {
  if (tools::file_ext(impath)  ==  "") {
    impathhdr <- paste(impath,  ".hdr",  sep = "")
  } else if (tools::file_ext(impath)  ==  "bil") {
    impathhdr <- gsub(".bil",  ".hdr",  impath)
  } else if (tools::file_ext(impath)  ==  "zip") {
    impathhdr <- gsub(".zip",  ".hdr",  impath)
  } else {
    impathhdr <- paste(tools::file_path_sans_ext(impath),  ".hdr",  sep = "")
  }

  if (!file.exists(impathhdr)) {
    message("WARNING : COULD NOT FIND hdr FILE")
    print(impathhdr)
    message("Process may stop")
  }
  return(impathhdr)
}

#" This function applies the regression models trained with prosail_hybrid_train
#"
#" @param regressionmodels list. List of regression models produced by prosail_hybrid_train
#" @param refl numeric. LUT of bidirectional reflectances factors used for training
#"
#" @return hybridres list. Estimated values corresponding to refl. Includes
#" - meanestimate = mean value for the ensemble regression model
#" - stdestimate = std value for the ensemble regression model
#" @importFrom stats predict
#" @importFrom matrixStats rowSds
#" @importFrom progress progress_bar
#" @export

prosail_hybrid_apply <- function(regressionmodels, refl) {

  # make sure refl is right dimensions
  refl <- t(refl)
  nbfeatures <- regressionmodels[[1]]$dim
  if (!ncol(refl) == nbfeatures && nrow(refl) == nbfeatures) {
    refl <- t(refl)
  }
  nbensemble <- length( regressionmodels)
  estimatedval <- list()
  pb <- progress_bar$new(
    format = "Applying SVR models [:bar] :percent in :elapsed", 
    total = nbensemble,  clear = FALSE,  width = 100)
  for (i in 1:nbensemble) {
    pb$tick()
    estimatedval[[i]] <- predict(regressionmodels[[i]],  refl)
  }
  estimatedval <- do.call(cbind, estimatedval)
  meanestimate <- rowMeans(estimatedval)
  stdestimate <- rowSds(estimatedval)
  hybridres <- list("meanestimate" = meanestimate, "stdestimate" = stdestimate)
  return(hybridres)
}

#" This function trains a suppot vector regression for a set of variables based on spectral data
#"
#" @param brf_lut numeric. LUT of bidirectional reflectances factors used for training
#" @param inputvar numeric. biophysical parameter corresponding to the reflectance
#" @param figplot Boolean. Set to TRUE if you want a scatterplot
#" @param nbensemble numeric. Number of individual subsets should be generated from brf_lut
#" @param withreplacement Boolean. should subsets be generated with or without replacement?
#"
#" @return modelssvr list. regression models trained for the retrieval of inputvar based on brf_lut
#" @importFrom liquidSVM svmRegression
#" @importFrom stats predict
#" @importFrom progress progress_bar
#" @importFrom graphics par
#" @importFrom expandFunctions reset.warnings
#" @importFrom stringr str_split
#" @importFrom simsalapar tryCatch.W.E
#" @import dplyr
#" @import ggplot2
# @" @import caret
#" @export

prosail_hybrid_train <- function(brf_lut, inputvar, figplot = FALSE, nbensemble = 20, withreplacement = FALSE) {

  x <- y <- ymean <- ystdmin <- ystdmax <- NULL
  # library(dplyr)
  # split the LUT into nbensemble subsets
  nbsamples <- length(inputvar)
  if (dim(brf_lut)[2] == nbsamples) {
    brf_lut <- t(brf_lut)
  }

  # if subsets are generated from brf_lut with replacement
  if (withreplacement == TRUE) {
    subsets <- list()
    samples_per_run <- round(nbsamples / nbensemble)
    for (run in (1:nbensemble)) {
      subsets[[run]] <- sample(seq(1, nbsamples),  samples_per_run,  replace = TRUE)
    }
  # if subsets are generated from brf_lut without replacement
  } else if (withreplacement == FALSE) {
    subsets <- split(sample(seq(1, nbsamples, by = 1)), seq(1, nbensemble, by = 1))
  }

  # run training for each subset
  modelssvr <- list()
  predictedyall <- list()
  tunedmodelyall <- list()
  pb <- progress_bar$new(
    format = "Training SVR on subsets [:bar] :percent in :elapsed", 
    total = nbensemble,  clear = FALSE,  width = 100)
  for (i in 1:nbensemble) {
    pb$tick()
    Sys.sleep(1  /  100)
    trainingset <- list()
    trainingset$X <- brf_lut[subsets[i][[1]], ]
    trainingset$Y <- inputvar[subsets[i][[1]]]
    # liquidSVM
    r1 <- tryCatch.W.E(tunedmodel <- liquidSVM::svmRegression(trainingset$X,  trainingset$Y))
    # reset.warnings()
    # tunedmodel <- liquidSVM::svmRegression(trainingset$X,  trainingset$Y)
    if (!is.null(r1$warning)) {
      msg <- r1$warning$message
      valgamma <- str_split(string = msg, pattern = "gamma=")[[1]][2]
      vallambda <- str_split(string = msg, pattern = "lambda=")[[1]][2]
      if (!is.na(as.numeric(valgamma))) {
        message("Adjusting Gamma accordingly")
        valgamma <- as.numeric(valgamma)
        tunedmodel <- liquidSVM::svmRegression(trainingset$X,  trainingset$Y, min_gamma = valgamma)
      }
      if (!is.na(as.numeric(vallambda))) {
        message("Adjusting Lambda accordingly")
        vallambda <- as.numeric(vallambda)
        tunedmodel <- liquidSVM::svmRegression(trainingset$X,  trainingset$Y, min_lambda = vallambda)
      }
    }
    modelssvr[[i]] <- tunedmodel
  }

  # if scatterplots needed
  if (figplot == TRUE) {
    # predict for full brf_lut
    for (i in 1:nbensemble) {
      tunedmodely <- stats::predict(modelssvr[[i]],  brf_lut)
      tunedmodelyall = cbind(tunedmodelyall, matrix(tunedmodely, ncol = 1))
    }
    # plot prediction
    df <- data.frame(x = rep(1:nbsamples, nbensemble),  y = as.numeric(matrix(tunedmodelyall, ncol = 1)))
    df_summary <- df %>% dplyr::group_by(x) %>%
      summarize( ymin = min(y), ystdmin = mean(y) - sd(y), 
                 ymax = max(y), ystdmax = mean(y) + sd(y), 
                 ymean = mean(y))
    par(mar = rep(.1,  4))
    p <- ggplot(df_summary,  aes(x = inputvar,  y = ymean))  + 
      geom_point(size = 2)  + 
      geom_errorbar(aes(ymin = ystdmin,  ymax = ystdmax))
    meanpredict <- rowMeans(matrix(as.numeric(tunedmodelyall), ncol = nbensemble))
    print(p)
  }
  return(modelssvr)
}

#" Reads ENVI hdr file
#"
#" @param hdrpath Path of the hdr file
#"
#" @return list of the content of the hdr file
#" @export
read_envi_header <- function(hdrpath) {
  # header <- paste(header,  collapse = "\n")
  if (!grepl(".hdr$",  hdrpath)) {
    stop("File extension should be .hdr")
  }
  hdr <- readLines(hdrpath)
  ## check ENVI at beginning of file
  if (!grepl("ENVI",  hdr[1])) {
    stop("Not an ENVI header (ENVI keyword missing)")
  } else {
    hdr <- hdr [-1]
  }
  ## remove curly braces and put multi-line key-value-pairs into one line
  hdr <- gsub("\\{([^}]*)\\}",  "\\1",  hdr)
  l <- grep("\\{",  hdr)
  r <- grep("\\}",  hdr)

  if (length(l) != length(r)) {
    stop("Error matching curly braces in header (differing numbers).")
  }

  if (any(r <= l)) {
    stop("Mismatch of curly braces in header.")
  }

  hdr[l] <- sub("\\{",  "",  hdr[l])
  hdr[r] <- sub("\\}",  "",  hdr[r])

  for (i in rev(seq_along(l))) {
    hdr <- c(
      hdr [seq_len(l [i] - 1)], 
      paste(hdr [l [i]:r [i]],  collapse = "\n"), 
      hdr [-seq_len(r [i])]
    )
  }

  ## split key = value constructs into list with keys as names
  hdr <- sapply(hdr,  split_line,  " = ",  USE.NAMES = FALSE)
  names(hdr) <- tolower(names(hdr))

  ## process numeric values
  tmp <- names(hdr) %in% c(
    "samples",  "lines",  "bands",  "header offset",  "data type", 
    "byte order",  "default bands",  "data ignore value", 
    "wavelength",  "fwhm",  "data gain values"
  )
  hdr [tmp] <- lapply(hdr [tmp],  function(x) {
    as.numeric(unlist(strsplit(x,  ",")))
  })

  return(hdr)
}

#" ENVI functions
#"
#" based on https: /  / github.com / cran / hyperSpec / blob / master / R / read.ENVI.R
#" added wavelength,  fwhm,  ... to header reading
#" Title
#"
#" @param x character.
#" @param separator character
#" @param trim_blank  boolean.
#"
#" @return list.
#" @export
split_line <- function(x,  separator,  trim_blank  = TRUE) {
  tmp <- regexpr(separator,  x)
  key <- substr(x,  1,  tmp - 1)
  value <- substr(x,  tmp  +  1,  nchar(x))
  if (trim_blank ) {
    blank_pattern <- "^[[:blank:]]*([^[:blank:]] + .*[^[:blank:]] + )[[:blank:]]*$"
    key <- sub(blank_pattern,  "\\1",  key)
    value <- sub(blank_pattern,  "\\1",  value)
  }
  value <- as.list(value)
  names(value) <- key
  return(value)
}

#" This function performs full training for hybrid invrsion using SVR with
#" values for default parameters
#"
#" @param minval list. minimum value for input parameters sampled to produce a training LUT
#" @param maxval list. maximum value for input parameters sampled to produce a training LUT
#" @param typedistrib  list. Type of distribution. Either "Uniform" or "Gaussian"
#" @param gaussiandistrib  list. Mean value and STD corresponding to the parameters sampled with gaussian distribution
#" @param parmset list. list of input parameters set to a specific value
#" @param nbsamples numeric. number of samples in training LUT
#" @param nbsamplesperrun numeric. number of training sample per individual regression model
#" @param nbmodels numeric. number of individual models to be run for ensemble
#" @param replacement bolean. is there replacement in subsampling?
#" @param sailversion character. Either 4SAIL or 4SAIL2
#" @param parms2estimate list. list of input parameters to be estimated
#" @param bands2select list. list of bands used for regression for each input parameter
#" @param noiselevel list. list of noise value added to reflectance (defined per input parm)
#" @param specprospect list. Includes optical constants required for PROSPECT
#" @param specsoil list. Includes either dry soil and wet soil,  or a unique soil sample if the psoil parameter is not inverted
#" @param specatm list. Includes direct and diffuse radiation for clear conditions
#" @param path_results character. path for results
#" @param figplot boolean. Set TRUE to get scatterplot of estimated biophysical variable during training step
#" @param force4lowlai boolean. Set TRUE to artificially reduce leaf chemical constituent content for low LAI
#"
#"
#" @return modelssvr list. regression models trained for the retrieval of inputvar based on brf_lut
#" @export

train_prosail_inversion <- function(minval = NULL, maxval = NULL, 
                                    typedistrib = NULL, gaussiandistrib = NULL, parmset = NULL, 
                                    nbsamples = 2000, nbsamplesperrun = 100, nbmodels = 20, replacement = TRUE, 
                                    sailversion = "4SAIL", 
                                    parms2estimate = "lai", bands2select = NULL, noiselevel = NULL, 
                                    specprospect = NULL,  specsoil = NULL,  specatm = NULL, 
                                    path_results = "./", figplot = FALSE, force4lowlai = TRUE) {

  ###===================================================================###
  ###           1- PRODUCE A LUT TO TRAIN THE HYBRID INVERSION          ###
  ###===================================================================###
  # Define sensor characteristics
  if (is.null(specprospect)) {
    specprospect <- prosail::specprospect
  }
  if (is.null(specsoil)) {
    specsoil <- prosail::specsoil
  }
  if (is.null(specprospect)) {
    specatm <- prosail::specatm
  }
  # define distribution for parameters to be sampled
  if (is.null(typedistrib)) {
    typedistrib <- data.frame("CHL" = "Uniform",  "CAR" = "Uniform", "EWT" = "Uniform", "ANT" = "Uniform", "LMA" = "Uniform", "N" = "Uniform",  "BROWN" = "Uniform", 
                              "psoil" = "Uniform", "LIDFa" = "Uniform",  "lai" = "Uniform", "q" = "Uniform", "tto" = "Uniform", "tts" = "Uniform",  "psi" = "Uniform")
  }
  if (is.null(gaussiandistrib)) {
    gaussiandistrib <- list("Mean" = NULL, "Std" = NULL)
  }
  if (is.null(minval)) {
    minval <- data.frame("CHL" = 10, "CAR" = 0, "EWT" = 0.01, "ANT" = 0, "LMA" = 0.005, "N" = 1.0, "psoil" = 0.0,  "BROWN" = 0.0, 
                         "LIDFa" = 20,  "lai" = 0.5, "q"=0.1, "tto" = 0, "tts" = 20,  "psi" = 80)
  }
  if (is.null(maxval)) {
    maxval <- data.frame("CHL" = 75, "CAR" = 15, "EWT" = 0.03, "ANT" = 2, "LMA" = 0.03, "N" = 2.0,  "psoil" = 1.0,  "BROWN" = 0.5, 
                         "LIDFa" = 70,  "lai" = 7, "q"=0.2, "tto" = 5, "tts" = 30,  "psi" = 110)
  }
  # define min and max values
  # fixed parameters
  if (is.null(parmset)) {
    parmset <- data.frame("TypeLidf" = 2,  "alpha" = 40)
  }
  # produce input parameters distribution
  if (sailversion == "4SAIL") {
    inputprosail <- get_distribution_input_prosail(minval, maxval, parmset, nbsamples, 
                                                   typedistrib = typedistrib, 
                                                   Mean = gaussiandistrib$Mean, Std = gaussiandistrib$Std, 
                                                   force4lowlai = force4lowlai)
  } else if (sailversion == "4SAIL2") {
    inputprosail <- get_distribution_input_prosail2(minval, maxval, parmset, nbsamples, 
                                                    typedistrib = typedistrib, 
                                                    Mean = gaussiandistrib$Mean, Std = gaussiandistrib$Std, 
                                                    force4lowlai = force4lowlai)
  }
  if (sailversion == "4SAIL2") {
    # Definition of Cv && update LAI
    maxlai <- min(c(maxval$lai), 4)
    inputprosail$Cv <- NA * inputprosail$lai
    inputprosail$Cv[which(inputprosail$lai > maxlai)] <- 1
    inputprosail$Cv[which(inputprosail$lai <= maxlai)] <- (1 / maxlai) + inputprosail$lai[which(inputprosail$lai <= maxlai)] / (maxlai + 1)
    inputprosail$Cv <- inputprosail$Cv * matrix(rnorm(length(inputprosail$Cv), mean = 1, sd = 0.1))
    inputprosail$Cv[which(inputprosail$Cv < 0)] <- 0
    inputprosail$Cv[which(inputprosail$Cv > 1)] <- 1
    inputprosail$Cv[which(inputprosail$lai > maxlai)] <- 1
    inputprosail$fraction_brown <- 0 + 0 * inputprosail$lai
    inputprosail$diss <- 0 + 0 * inputprosail$lai
    inputprosail$Zeta <- 0.2 + 0 * inputprosail$lai
    inputprosail$lai <- inputprosail$lai * inputprosail$Cv
  }

  # generate LUT of BRF corresponding to inputprosail,  for a sensor
  brf_lut <- Generate_LUT_BRF(sailversion = sailversion, inputprosail = inputprosail, 
                              specprospect = specprospect, specsoil = specsoil, specatm = specatm)

  # write parameters LUT
  output <- matrix(unlist(inputprosail),  ncol = length(inputprosail),  byrow = FALSE)
  filename <- file.path(path_results, "PROSAIL_LUT_InputParms.txt")
  write.table(x = format(output,  digits = 3), file = filename, append = FALSE,  quote = FALSE, 
              col.names = names(inputprosail),  row.names = FALSE, sep = "\t")
  # Write BRF LUT corresponding to parameters LUT
  filename <- file.path(path_results, "PROSAIL_LUT_reflectance.txt")
  write.table(x = format(t(brf_lut),  digits = 5), file = filename, append = FALSE,  quote = FALSE, 
              col.names = specprospect$lambda,  row.names = FALSE, sep = "\t")

  # Which bands will be used for inversion?
  if (is.null(bands2select)) {
    bands2select <- list()
    for (parm in parms2estimate) {
      bands2select[[parm]] <- seq(1, length(specprospect$lambda))
    }
  }
  # Add gaussian noise to reflectance LUT: one specific LUT per parameter
  if (is.null(noiselevel)) {
    noiselevel <- list()
    for (parm in parms2estimate) {
      noiselevel[[parm]] <- 0.01
    }
  }

  # produce LIT with noise
  brf_lut_Noise <- list()
  for (parm in parms2estimate) {
    brf_lut_Noise[[parm]] <- brf_lut[bands2select[[parm]], ] + brf_lut[bands2select[[parm]], ] * matrix(rnorm(nrow(brf_lut[bands2select[[parm]], ]) * ncol(brf_lut[bands2select[[parm]], ]), 
                                                                                                        0, noiselevel[[parm]]), nrow = nrow(brf_lut[bands2select[[parm]], ]))
  }

  ###===================================================================###
  ###                     PERFORM HYBRID INVERSION                      ###
  ###===================================================================###
  # train SVR for each variable and each run
  modelsvr = list()
  for (parm in parms2estimate) {
    colparm <- which(parm == names(inputprosail))
    inputvar <- inputprosail[[colparm]]
    modelsvr[[parm]] <- prosail_hybrid_train(brf_lut = brf_lut_Noise[[parm]], inputvar = inputvar, 
                                             figplot = figplot, nbensemble = nbmodels, withreplacement = replacement)
  }
  return(modelsvr)
}

#" writes ENVI hdr file
#"
#" @param hdr content to be written
#" @param hdrpath Path of the hdr file
#"
#" @return None
#" @importFrom stringr str_count
#" @export
write_envi_header <- function(hdr,  hdrpath) {
  h <- lapply(hdr,  function(x) {
    if (length(x) > 1 || (is.character(x) && stringr::str_count(x,  "\\w + ") > 1)) {
      x <- paste0("{",  paste(x,  collapse = ","),  "}")
    }
    # convert last numerics
    x <- as.character(x)
  })
  writeLines(c("ENVI",  paste(names(hdr),  h,  sep = "=")),  con = hdrpath)
  return(invisible())
}

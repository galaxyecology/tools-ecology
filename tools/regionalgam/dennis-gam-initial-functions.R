### R-Script Adapted from script provided by the CEH, UK BY: Reto Schmucki [ reto.schmucki@mail.mcgill.ca]
### DATE: 14 July 2014 function to run two stage model in DENNIS et al. 2013


.onAttach <- function(libname, pkgname) { # nolint
  packageStartupMessage(" The regionalGAM package that is no longer maintained, \n use the new rbms package instead. \n
   devtools::install_github(\"RetoSchmucki/rbms\", force=TRUE)")
}


#' year_day_func Function
#' This function generate the full sequence of days, months and include the observation to that file.
#' @param sp_data A data.frame with your observation.
#' @keywords year days
#' @export
#' @author Reto Schmucki
#' @examples
#' year_day_func()


# FUNCTIONS

year_day_func <- function(sp_data) {

    year <- unique(sp_data$YEAR)

    origin_d <- paste(year, "01-01", sep = "-")
    if ((year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))) {
        nday <- 366
    } else {
        nday <- 365
    }

    date_serie <- as.POSIXlt(seq(as.Date(origin_d), length = nday, by = "day"), format = "%Y-%m-%d")

    dayno <- as.numeric(julian(date_serie, origin = as.Date(origin_d)) + 1)
    month <- as.numeric(strftime(date_serie, format = "%m"))
    week <- as.numeric(strftime(date_serie, format = "%W"))
    week_day <- as.numeric(strftime(date_serie, format = "%u"))
    day <- as.numeric(strftime(date_serie, format = "%d"))

    site_list <- sp_data[!duplicated(sp_data$SITE), c("SITE")]

    all_day_site <- data.frame(SPECIES = sp_data$SPECIES[1], SITE = rep(site_list, rep(nday, length(site_list))),
        YEAR = sp_data$YEAR[1], MONTH = month, WEEK = week, DAY = day, DAY_WEEK = week_day, DAYNO = dayno,
        COUNT = NA)

    count_index <- match(paste(sp_data$SITE, sp_data$DAYNO, sep = "_"), paste(all_day_site$SITE, all_day_site$DAYNO,
        sep = "_"))
    all_day_site$COUNT[count_index] <- sp_data$COUNT
    site_count_length <- aggregate(sp_data$COUNT, by = list(sp_data$SITE), function(x) list(seq_along(x)))
    names(site_count_length$x) <- as.character(site_count_length$Group.1)
    site_countno <- utils::stack(site_count_length$x)
    all_day_site$COUNTNO <- NA # nolint
    all_day_site$COUNTNO[count_index] <- site_countno$values  # add count number to ease extraction of single count

    # Add zero to close observation season two weeks before and after the first and last
    first_obs <- min(all_day_site$DAYNO[!is.na(all_day_site$COUNT)])
    last_obs <- max(all_day_site$DAYNO[!is.na(all_day_site$COUNT)])

    closing_season <- c((first_obs - 11):(first_obs - 7), (last_obs + 7):(last_obs + 11))

    # If closing season is before day 1 or day 365, simply set the first and last 5 days to 0
    if (min(closing_season) < 1)
        closing_season[1:5] <- c(1:5)
    if (max(closing_season) > nday)
        closing_season[6:10] <- c((nday - 4):nday)

    all_day_site$COUNT[all_day_site$DAYNO %in% closing_season] <- 0
    all_day_site$ANCHOR <- 0 # nolint
    all_day_site$ANCHOR[all_day_site$DAYNO %in% closing_season] <- 1

    all_day_site <- all_day_site[order(all_day_site$SITE, all_day_site$DAYNO), ]

    return(all_day_site)
}


#' trap_area Function
#'
#' This function compute the area under the curve using the trapezoid method.
#' @param x A vector or a two columns matrix.
#' @param y A vector, Default is NULL
#' @keywords trapezoid
#' @export
#' @examples
#' trap_area()


trap_area <- function(x, y = NULL) {
    # If y is null and x has multiple columns then set y to x[,2] and x to x[,1]
    if (is.null(y)) {
        if (length(dim(x)) == 2) {
            y <- x[, 2]
            x <- x[, 1]
        } else {
            stop("ERROR: need to either specifiy both x and y or supply a two column data.frame/matrix to x")
        }
    }

    # Check x and y are same length
    if (length(x) != length(y)) {
        stop("ERROR: x and y need to be the same length")
    }

    # Need to exclude any pairs that are NA for either x or y
    rm_inds <- which(is.na(x) | is.na(y))
    if (length(rm_inds) > 0) {
        x <- x[-rm_inds]
        y <- y[-rm_inds]
    }

    # Determine values of trapezoids under curve Get inds
    inds <- 1:(length(x) - 1)
    # Determine area using trapezoidal rule Area = ( (b1 + b2)/2 ) * h where b1 and b2 are lengths of bases
    # (the parallel sides) and h is the height (the perpendicular distance between two bases)
    areas <- ((y[inds] + y[inds + 1]) / 2) * diff(x)

    # total area is sum of all trapezoid areas
    tot_area <- sum(areas)

    # Return total area
    return(tot_area)
}


#' trap_index Function
#'
#' This function compute the area under the curve (Abundance Index) across species, sites and years
#' @param sp_data A data.frame containing species count data generated from the year_day_func()
#' @param y A vector, Default is NULL
#' @keywords Abundance index
#' @export
#' @examples
#' trap_index()



trap_index <- function(sp_data, data_col = "IMP", time_col = "DAYNO", by_col = c("SPECIES", "SITE", "YEAR")) {

    # Build output data.frame
    out_obj <- unique(sp_data[, by_col])
    # Set row.names to be equal to collapsing of output rows (will be unique, you need them to make uploading
    # values back to data.frame will be easier)
    row.names(out_obj) <- apply(out_obj, 1, paste, collapse = "_")

    # Using this row.names from out_obj above as index in by function to loop through values all unique combs
    # of by_cols and fit trap_area to data
    ind_dat <- by(sp_data[, c(time_col, data_col)], apply(sp_data[, by_col], 1, paste, collapse = "_"), trap_area)

    # Add this data to output object
    out_obj[names(ind_dat), "SINDEX"] <- round(ind_dat / 7, 1)

    # Set row.names to defaults
    row.names(out_obj) <- NULL

    # Return output object
    return(out_obj)
}


#' flight_curve Function
#' This function compute the flight curve across and years
#' @param your_dataset A data.frame containing original species count data. The data format is a csv (comma "',"' separated) with 6 columns with headers, namely "species","transect_id","visit_year","visit_month","visit_day","sp_count"
#' @keywords standardize flight curve
#' @export
#' @examples
#' flight_curve()


flight_curve <- function(your_dataset, GamFamily = "nb", MinVisit = 2, MinOccur = 1) { # nolint

    if ("mgcv" %in% installed.packages() == "FALSE") {
        print("mgcv package is not installed.")
        x <- readline("Do you want to install it? Y/N")
        if (x == "Y") {
            install.packages("mgcv")
        }
        if (x == "N") {
            stop("flight curve can not be computed without the mgcv package, sorry")
        }
    }

    flight_pheno <- data.frame()

    your_dataset$DAYNO <- strptime(paste(your_dataset$DAY, your_dataset$MONTH, # nolint
        your_dataset$YEAR, sep = "/"), "%d/%m/%Y")$yday + 1
    dataset <- your_dataset[, c("SPECIES", "SITE", "YEAR", "MONTH",
        "DAY", "DAYNO", "COUNT")]
    sample_year <- unique(dataset$YEAR)
    sample_year <- sample_year[order(sample_year)]

    if (length(sample_year) > 1) {
        for (y in sample_year) {
            dataset_y <- dataset[dataset$YEAR == y, ]

            # subset sites with enough visit and occurence
            occ <- aggregate(dataset_y$COUNT, by = list(SITE = dataset_y$SITE), function(x) sum(x > 0))
            vis <- aggregate(dataset_y$COUNT, by = list(SITE = dataset_y$SITE), function(x) length(x))
            dataset_y <- dataset_y[dataset_y$SITE %in% occ$SITE[occ$x >= MinOccur], ]
            dataset_y <- dataset_y[dataset_y$SITE %in% vis$SITE[vis$x >= MinVisit], ]
            nsite <- length(unique(dataset_y$SITE))
            if (nsite < 1) {
              print(paste("No sites with sufficient visits and occurence, MinOccur:", MinOccur, " MinVisit: ", MinVisit, " for ", dataset$SPECIES[1], "at year", y))
              next
            }
            # Determine missing days and add to dataset
            sp_data_all <- year_day_func(dataset_y)
            if (nsite > 200) {
                sp_data_all <- sp_data_all[as.character(sp_data_all$SITE) %in% as.character(unique(dataset_y$SITE)[sample(1:nsite,
                    200, replace = F)]), ]
                sp_data_all <- sp_data_all
            }
            sp_data_all$trimDAYNO <- sp_data_all$DAYNO - min(sp_data_all$DAYNO) + 1 # nolint
            print(paste("Fitting the GAM for", as.character(sp_data_all$SPECIES[1]), "and year", y, "with", length(unique(sp_data_all$SITE)), "sites :", Sys.time()))
            if (length(unique(sp_data_all$SITE)) > 1) {
                gam_obj_site <- try(mgcv::gam(COUNT ~ s(trimDAYNO, bs = "cr") + as.factor(SITE) - 1,
                    data = sp_data_all, family = GamFamily), silent = TRUE)
            }
            else {
                gam_obj_site <- try(mgcv::gam(COUNT ~ s(trimDAYNO, bs = "cr")  - 1,
                    data = sp_data_all, family = GamFamily), silent = TRUE)
            }
            # Give a second try if the GAM does not converge the first time
            if (class(gam_obj_site)[1] == "try-error") {
            # Determine missing days and add to dataset
                sp_data_all <- year_day_func(dataset_y)
                if (nsite > 200) {
                   sp_data_all <- sp_data_all[as.character(sp_data_all$SITE) %in% as.character(unique(dataset_y$SITE)[sample(1:nsite,
                    200, replace = F)]), ]
                }
                else {
                    sp_data_all <- sp_data_all
                }
                sp_data_all$trimDAYNO <- sp_data_all$DAYNO - min(sp_data_all$DAYNO) + 1 # nolint
                print(paste("Fitting the GAM for", sp_data_all$SPECIES[1], "at year", y, "with", length(unique(sp_data_all$SITE)), "sites :", Sys.time(), "second try"))
                if (length(unique(sp_data_all$SITE)) > 1) {
                    gam_obj_site <- try(mgcv::gam(COUNT ~ s(trimDAYNO, bs = "cr") + as.factor(SITE) - 1,
                       data = sp_data_all, family = GamFamily), silent = TRUE)
                }
                else {
                    gam_obj_site <- try(mgcv::gam(COUNT ~ s(trimDAYNO, bs = "cr")  - 1,
                        data = sp_data_all, family = GamFamily), silent = TRUE)
                }
                if (class(gam_obj_site)[1] == "try-error") {
                    print(paste("Error in fitting the flight period for", sp_data_all$SPECIES[1], "at year", y, "no convergence after two trial"))
                    sp_data_all[, "FITTED"] <- NA
                    sp_data_all[, "COUNT_IMPUTED"] <- NA
                    sp_data_all[is.na(sp_data_all$COUNT), "COUNT_IMPUTED"] <- NA
                    sp_data_all[, "NM"] <- NA
                }
                else {
                    # Generate a list of values for all days from the additive model and use
                    # these value to fill the missing observations
                    sp_data_all[, "FITTED"] <- mgcv::predict.gam(gam_obj_site, newdata = sp_data_all[,
                        c("trimDAYNO", "SITE")], type = "response")
                    # force zeros at the beginning end end of the year
                    sp_data_all[sp_data_all$trimDAYNO < 60, "FITTED"] <- 0
                    sp_data_all[sp_data_all$trimDAYNO > 305, "FITTED"] <- 0
                    # if infinite number in predict replace with NA.
                    if (sum(is.infinite(sp_data_all[, "FITTED"])) > 0) {
                        print(paste("Error in the flight period for", sp_data_all$SPECIES[1], "at year", y, "weird predicted values"))
                        sp_data_all[, "FITTED"] <- NA
                        sp_data_all[, "COUNT_IMPUTED"] <- NA
                        sp_data_all[is.na(sp_data_all$COUNT), "COUNT_IMPUTED"] <- NA
                        sp_data_all[, "NM"] <- NA
                    }
                    else {
                    sp_data_all[, "COUNT_IMPUTED"] <- sp_data_all$COUNT
                    sp_data_all[is.na(sp_data_all$COUNT), "COUNT_IMPUTED"] <- sp_data_all$FITTED[is.na(sp_data_all$COUNT)]
                    # Define the flight curve from the fitted values and append them over
                    # years (this is one flight curve per year for all site)
                    site_sums <- aggregate(sp_data_all$FITTED, by = list(SITE = sp_data_all$SITE),
                        FUN = sum)
                    # Rename sum column
                    names(site_sums)[names(site_sums) == "x"] <- "SITE_YR_FSUM"
                    # Add data to sp_data data.frame (ensure merge does not sort the data!)
                    sp_data_all <- merge(sp_data_all, site_sums, by <- c("SITE"),
                        all = TRUE, sort = FALSE)
                    # Calculate normalized values
                    sp_data_all[, "NM"] <- sp_data_all$FITTED / sp_data_all$SITE_YR_FSUM
                    }
                }
            }
            else {
                # Generate a list of values for all days from the additive model and use
                # these value to fill the missing observations
                sp_data_all[, "FITTED"] <- mgcv::predict.gam(gam_obj_site, newdata = sp_data_all[,
                    c("trimDAYNO", "SITE")], type = "response")
                # force zeros at the beginning end end of the year
                sp_data_all[sp_data_all$trimDAYNO < 60, "FITTED"] <- 0
                sp_data_all[sp_data_all$trimDAYNO > 305, "FITTED"] <- 0
                # if infinite number in predict replace with NA.
                if (sum(is.infinite(sp_data_all[, "FITTED"])) > 0) {
                    print(paste("Error in the flight period for", sp_data_all$SPECIES[1], "at year", y, "weird predicted values"))
                    sp_data_all[, "FITTED"] <- NA
                    sp_data_all[, "COUNT_IMPUTED"] <- NA
                    sp_data_all[is.na(sp_data_all$COUNT), "COUNT_IMPUTED"] <- NA
                    sp_data_all[, "NM"] <- NA
                }
                else {
                sp_data_all[, "COUNT_IMPUTED"] <- sp_data_all$COUNT
                sp_data_all[is.na(sp_data_all$COUNT), "COUNT_IMPUTED"] <- sp_data_all$FITTED[is.na(sp_data_all$COUNT)]
                # Define the flight curve from the fitted values and append them over
                # years (this is one flight curve per year for all site)
                site_sums <- aggregate(sp_data_all$FITTED, by = list(SITE = sp_data_all$SITE),
                    FUN = sum)
                # Rename sum column
                names(site_sums)[names(site_sums) == "x"] <- "SITE_YR_FSUM"
                # Add data to sp_data data.frame (ensure merge does not sort the data!)
                sp_data_all <- merge(sp_data_all, site_sums, by = c("SITE"), all = TRUE,
                    sort = FALSE)
                # Calculate normalized values
                sp_data_all[, "NM"] <- sp_data_all$FITTED / sp_data_all$SITE_YR_FSUM
                }
            }
            sp_data_filled <- sp_data_all
            flight_curve <- data.frame(species = sp_data_filled$SPECIES, year = sp_data_filled$YEAR,
                week = sp_data_filled$WEEK, DAYNO = sp_data_filled$DAYNO, DAYNO_adj = sp_data_filled$trimDAYNO,
                nm = sp_data_filled$NM)[!duplicated(paste(sp_data_filled$YEAR,
                sp_data_filled$DAYNO, sep = "_")), ]
            flight_curve <- flight_curve[order(flight_curve$DAYNO), ]
            # bind if exist else create
            if (is.na(flight_curve$nm[1])) next() # nolint

            flight_pheno <- rbind(flight_pheno, flight_curve)

        }  # end of year loop
    }
    else {
        y <- unique(dataset$YEAR)
        dataset_y <- dataset[dataset$YEAR == y, ]
        # subset sites with enough visit and occurence
        occ <- aggregate(dataset_y$COUNT, by = list(SITE = dataset_y$SITE), function(x) sum(x > 0))
        vis <- aggregate(dataset_y$COUNT, by = list(SITE = dataset_y$SITE), function(x) length(x))
        dataset_y <- dataset_y[dataset_y$SITE %in% occ$SITE[occ$x >= MinOccur], ]
        dataset_y <- dataset_y[dataset_y$SITE %in% vis$SITE[vis$x >= MinVisit], ]
        nsite <- length(unique(dataset_y$SITE))
        if (nsite < 1) {
          stop(paste("No sites with sufficient visits and occurence, MinOccur:", MinOccur, " MinVisit: ", MinVisit, " for ", dataset$SPECIES[1], "at year", y))
        }
        # Determine missing days and add to dataset
        sp_data_all <- year_day_func(dataset_y)
        if (nsite > 200) {
            sp_data_all <- sp_data_all[as.character(sp_data_all$SITE) %in% as.character(unique(dataset_y$SITE)[sample(1:nsite,
            200, replace = F)]), ]
        }
        else {
            sp_data_all <- sp_data_all
        }
        sp_data_all$trimDAYNO <- sp_data_all$DAYNO - min(sp_data_all$DAYNO) + 1 # nolint
        print(paste("Fitting the GAM for", sp_data_all$SPECIES[1], "at year", y, ":", Sys.time()))
        if (length(unique(sp_data_all$SITE)) > 1) {
            gam_obj_site <- try(mgcv::gam(COUNT ~ s(trimDAYNO, bs = "cr") + as.factor(SITE) - 1,
            data = sp_data_all, family = GamFamily), silent = TRUE)
        }
        else {
            gam_obj_site <- try(mgcv::gam(COUNT ~ s(trimDAYNO, bs = "cr")  - 1,
            data = sp_data_all, family = GamFamily), silent = TRUE)
        }
        # Give a second try if the GAM does not converge the first time
        if (class(gam_obj_site)[1] == "try-error") {
            # Determine missing days and add to dataset
            sp_data_all <- year_day_func(dataset_y)
            if (nsite > 200) {
                sp_data_all <- sp_data_all[as.character(sp_data_all$SITE) %in% as.character(unique(dataset_y$SITE)[sample(1:nsite,
                200, replace = F)]), ]
            }
            else {
                sp_data_all <- sp_data_all
            }
            sp_data_all$trimDAYNO <- sp_data_all$DAYNO - min(sp_data_all$DAYNO) + 1 # nolint
            print(paste("Fitting the GAM for", sp_data_all$SPECIES[1], "at year", y, "with", length(unique(sp_data_all$SITE)), "sites :", Sys.time(), "second try"))
            if (length(unique(sp_data_all$SITE)) > 1) {
                gam_obj_site <- try(mgcv::bam(COUNT ~ s(trimDAYNO, bs = "cr") + as.factor(SITE) - 1,
                data = sp_data_all, family = GamFamily), silent = TRUE)
            }
            else {
                gam_obj_site <- try(mgcv::gam(COUNT ~ s(trimDAYNO, bs = "cr") - 1,
                data = sp_data_all, family = GamFamily), silent = TRUE)
            }
            if (class(gam_obj_site)[1] == "try-error") {
                print(paste("Error in fitting the flight period for", sp_data_all$SPECIES[1], "at year", y, "no convergence after two trial"))
                sp_data_all[, "FITTED"] <- NA
                sp_data_all[, "COUNT_IMPUTED"] <- NA
                sp_data_all[is.na(sp_data_all$COUNT), "COUNT_IMPUTED"] <- NA
                sp_data_all[, "NM"] <- NA
            }
            else {
                # Generate a list of values for all days from the additive model and use
                # these value to fill the missing observations
                sp_data_all[, "FITTED"] <- mgcv::predict.gam(gam_obj_site, newdata = sp_data_all[,
                c("trimDAYNO", "SITE")], type = "response")
                # force zeros at the beginning end end of the year
                sp_data_all[sp_data_all$trimDAYNO < 60, "FITTED"] <- 0
                sp_data_all[sp_data_all$trimDAYNO > 305, "FITTED"] <- 0
                # if infinite number in predict replace with NA.
                if (sum(is.infinite(sp_data_all[, "FITTED"])) > 0) {
                    print(paste("Error in the flight period for", sp_data_all$SPECIES[1], "at year", y, "weird predicted values"))
                    sp_data_all[, "FITTED"] <- NA
                    sp_data_all[, "COUNT_IMPUTED"] <- NA
                    sp_data_all[is.na(sp_data_all$COUNT), "COUNT_IMPUTED"] <- NA
                    sp_data_all[, "NM"] <- NA
                }
                else {
                    sp_data_all[, "COUNT_IMPUTED"] <- sp_data_all$COUNT
                    sp_data_all[is.na(sp_data_all$COUNT), "COUNT_IMPUTED"] <- sp_data_all$FITTED[is.na(sp_data_all$COUNT)]
                    # Define the flight curve from the fitted values and append them over
                    # years (this is one flight curve per year for all site)
                    site_sums <- aggregate(sp_data_all$FITTED, by = list(SITE = sp_data_all$SITE),
                    FUN = sum)
                    # Rename sum column
                    names(site_sums)[names(site_sums) == "x"] <- "SITE_YR_FSUM"
                    # Add data to sp_data data.frame (ensure merge does not sort the data!)
                    sp_data_all <- merge(sp_data_all, site_sums, by <- c("SITE"),
                    all = TRUE, sort = FALSE)
                    # Calculate normalized values
                    sp_data_all[, "NM"] <- sp_data_all$FITTED / sp_data_all$SITE_YR_FSUM
                }
            }
        }
        else {
            # Generate a list of values for all days from the additive model and use
            # these value to fill the missing observations
            sp_data_all[, "FITTED"] <- mgcv::predict.gam(gam_obj_site, newdata = sp_data_all[,
            c("trimDAYNO", "SITE")], type = "response")
            # force zeros at the beginning end end of the year
            sp_data_all[sp_data_all$trimDAYNO < 60, "FITTED"] <- 0
            sp_data_all[sp_data_all$trimDAYNO > 305, "FITTED"] <- 0
            # if infinite number in predict replace with NA.
            if (sum(is.infinite(sp_data_all[, "FITTED"])) > 0) {
                print(paste("Error in the flight period for", sp_data_all$SPECIES[1], "at year", y, "weird predicted values"))
                sp_data_all[, "FITTED"] <- NA
                sp_data_all[, "COUNT_IMPUTED"] <- NA
                sp_data_all[is.na(sp_data_all$COUNT), "COUNT_IMPUTED"] <- NA
                sp_data_all[, "NM"] <- NA
            }
            else {
                sp_data_all[, "COUNT_IMPUTED"] <- sp_data_all$COUNT
                sp_data_all[is.na(sp_data_all$COUNT), "COUNT_IMPUTED"] <- sp_data_all$FITTED[is.na(sp_data_all$COUNT)]
                # Define the flight curve from the fitted values and append them over
                # years (this is one flight curve per year for all site)
                site_sums <- aggregate(sp_data_all$FITTED, by = list(SITE = sp_data_all$SITE),
                FUN = sum)
                # Rename sum column
                names(site_sums)[names(site_sums) == "x"] <- "SITE_YR_FSUM"
                # Add data to sp_data data.frame (ensure merge does not sort the data!)
                sp_data_all <- merge(sp_data_all, site_sums, by = c("SITE"), all = TRUE,
                sort = FALSE)
                # Calculate normalized values
                sp_data_all[, "NM"] <- sp_data_all$FITTED / sp_data_all$SITE_YR_FSUM
            }
        }
        sp_data_filled <- sp_data_all
        flight_curve <- data.frame(species = sp_data_filled$SPECIES, year = sp_data_filled$YEAR,
        week = sp_data_filled$WEEK, DAYNO = sp_data_filled$DAYNO, DAYNO_adj = sp_data_filled$trimDAYNO,
        nm = sp_data_filled$NM)[!duplicated(paste(sp_data_filled$YEAR,
        sp_data_filled$DAYNO, sep = "_")), ]
        flight_curve <- flight_curve[order(flight_curve$DAYNO), ]

        flight_pheno <- rbind(flight_pheno, flight_curve)

    }
    return(flight_pheno)
} # nolint


#' abundance_index Function
#'
#' This function compute the Abundance Index across sites and years from your dataset and the regional flight curve
#' @param your_dataset A data.frame containing original species count data. The data format is a csv (comma "," separated) with 6 columns with headers, namely	"species","transect_id","visit_year","visit_month","visit_day","sp_count"
#' @param flight_pheno A data.frame for the regional flight curve computed with the function flight_curve
#' @keywords standardize flight curve
#' @export
#' @examples
#' abundance_index()

abundance_index <- function(your_dataset, flight_pheno) {

your_dataset$DAYNO <- strptime(paste(your_dataset$DAY, your_dataset$MONTH, # nolint
    your_dataset$YEAR, sep = "/"), "%d/%m/%Y")$yday + 1

dataset <- your_dataset[, c("SPECIES", "SITE", "YEAR", "MONTH",
    "DAY", "DAYNO", "COUNT")]

sample_year <- unique(dataset$YEAR)
sample_year <- sample_year[order(sample_year)]


if (length(sample_year) > 1) {

for (y in sample_year) {

    year_pheno <- flight_pheno[flight_pheno$year == y, ]

    dataset_y <- dataset[dataset$YEAR == y, ]

    sp_data_site <- year_day_func(dataset_y)
    sp_data_site$trimDAYNO <- sp_data_site$DAYNO - min(sp_data_site$DAYNO) + 1 # nolint

    sp_data_site <- merge(sp_data_site, year_pheno[, c("DAYNO", "nm")],
        by = c("DAYNO"), all.x = TRUE, sort = FALSE)

    # compute proportion of the flight curve sampled due to missing visits
    pro_missing_count <- data.frame(SITE = sp_data_site$SITE, WEEK = sp_data_site$WEEK,
        NM = sp_data_site$nm, COUNT = sp_data_site$COUNT, ANCHOR = sp_data_site$ANCHOR)
    pro_missing_count$site_week <- paste(pro_missing_count$SITE, pro_missing_count$WEEK,
        sep = "_")
    siteweeknocount <- aggregate(pro_missing_count$COUNT, by = list(pro_missing_count$site_week),
        function(x) sum(!is.na(x)) == 0)
    pro_missing_count <- pro_missing_count[pro_missing_count$site_week %in%
        siteweeknocount$Group.1[siteweeknocount$x == TRUE], ]
    pro_count_agg <- aggregate(pro_missing_count$NM, by = list(pro_missing_count$SITE),
        function(x) 1 - sum(x, na.rm = T))
    names(pro_count_agg) <- c("SITE", "PROP_PHENO_SAMPLED")

    # remove samples outside the monitoring window
    sp_data_site$COUNT[sp_data_site$nm == 0] <- NA

    # Compute the regional GAM index

    if (length(unique(sp_data_site$SITE)) > 1) {
        glm_obj_site <- glm(COUNT ~ factor(SITE) + offset(log(nm)) - 1, data = sp_data_site,
            family = quasipoisson(link = "log"), control = list(maxit = 100))
    } else {
       glm_obj_site <- glm(COUNT ~ offset(log(nm)) - 1, data = sp_data_site,
           family = quasipoisson(link = "log"), control = list(maxit = 100))
    }

    sp_data_site[, "FITTED"] <- predict.glm(glm_obj_site, newdata = sp_data_site,
        type = "response")
    sp_data_site[, "COUNT_IMPUTED"] <- sp_data_site$COUNT
    sp_data_site[is.na(sp_data_site$COUNT), "COUNT_IMPUTED"] <- sp_data_site$FITTED[is.na(sp_data_site$COUNT)]

   ## add fitted value for missing mid-week data
    sp_data_site <- sp_data_site[!paste(sp_data_site$DAY_WEEK, sp_data_site$COUNT) %in%
        c("1 NA", "2 NA", "3 NA", "5 NA", "6 NA", "7 NA"), ]

    ## remove all added mid-week values for weeks with real count
    ## (observation)
    sp_data_site$site_week <- paste(sp_data_site$SITE, sp_data_site$WEEK,
        sep = "_")
    siteweekcount <- aggregate(sp_data_site$COUNT, by = list(sp_data_site$site_week),
        function(x) sum(!is.na(x)) > 0)
    sp_data_site <- sp_data_site[!(is.na(sp_data_site$COUNT) & (sp_data_site$site_week %in%
        siteweekcount$Group.1[siteweekcount$x == TRUE])), names(sp_data_site) !=
        "site_week"]

    ## Compute the regional GAM index
    print(paste("Compute index for", sp_data_site$SPECIES[1], "at year", y, "for", length(unique(sp_data_site$SITE)), "sites:", Sys.time()))
    regional_gam_index <- trap_index(sp_data_site, data_col = "COUNT_IMPUTED",
        time_col = "DAYNO", by_col = c("SPECIES", "SITE", "YEAR"))

    cumu_index <- merge(regional_gam_index, pro_count_agg, by = c("SITE"),
        all.x = TRUE, sort = FALSE)
    names(cumu_index) <- c("SITE", "SPECIES", "YEAR", "regional_gam", "prop_pheno_sampled")

    cumu_index <- cumu_index[order(cumu_index$SITE), ]

    # bind if exist else create
    if ("cumullated_indices" %in% ls()) {
        cumullated_indices <- rbind(cumullated_indices, cumu_index)
    } else {
        cumullated_indices <- cumu_index
    }

}  # end of year loop

} else {

    y <- unique(dataset$YEAR)
    year_pheno <- flight_pheno[flight_pheno$year == y, ]

    dataset_y <- dataset[dataset$YEAR == y, ]

    sp_data_site <- year_day_func(dataset_y)
    sp_data_site$trimDAYNO <- sp_data_site$DAYNO - min(sp_data_site$DAYNO) + 1 # nolint

    sp_data_site <- merge(sp_data_site, year_pheno[, c("DAYNO", "nm")],
        by = c("DAYNO"), all.x = TRUE, sort = FALSE)

    # compute proportion of the flight curve sampled due to missing visits
    pro_missing_count <- data.frame(SITE = sp_data_site$SITE, WEEK = sp_data_site$WEEK,
        NM = sp_data_site$nm, COUNT = sp_data_site$COUNT, ANCHOR = sp_data_site$ANCHOR)
    pro_missing_count$site_week <- paste(pro_missing_count$SITE, pro_missing_count$WEEK,
        sep = "_")
    siteweeknocount <- aggregate(pro_missing_count$COUNT, by = list(pro_missing_count$site_week),
        function(x) sum(!is.na(x)) == 0)
    pro_missing_count <- pro_missing_count[pro_missing_count$site_week %in%
        siteweeknocount$Group.1[siteweeknocount$x == TRUE], ]
    pro_count_agg <- aggregate(pro_missing_count$NM, by = list(pro_missing_count$SITE),
        function(x) 1 - sum(x, na.rm = T))
    names(pro_count_agg) <- c("SITE", "PROP_PHENO_SAMPLED")

    # remove samples outside the monitoring window
    sp_data_site$COUNT[sp_data_site$nm == 0] <- NA

    # Compute the regional GAM index
    if (length(unique(sp_data_site$SITE)) > 1) {
        glm_obj_site <- glm(COUNT ~ factor(SITE) + offset(log(nm)) - 1, data = sp_data_site,
            family = quasipoisson(link = "log"), control = list(maxit = 100))
    } else {
       glm_obj_site <- glm(COUNT ~  offset(log(nm)) - 1, data = sp_data_site,
           family = quasipoisson(link = "log"), control = list(maxit = 100))
    }

    sp_data_site[, "FITTED"] <- predict.glm(glm_obj_site, newdata = sp_data_site,
        type = "response")
    sp_data_site[, "COUNT_IMPUTED"] <- sp_data_site$COUNT
    sp_data_site[is.na(sp_data_site$COUNT), "COUNT_IMPUTED"] <- sp_data_site$FITTED[is.na(sp_data_site$COUNT)]

    # add fitted value for missing mid-week data
    sp_data_site <- sp_data_site[!paste(sp_data_site$DAY_WEEK, sp_data_site$COUNT) %in%
        c("1 NA", "2 NA", "3 NA", "5 NA", "6 NA", "7 NA"), ]

    # remove all added mid-week values for weeks with real count
    # (observation)
    sp_data_site$site_week <- paste(sp_data_site$SITE, sp_data_site$WEEK,
        sep = "_")
    siteweekcount <- aggregate(sp_data_site$COUNT, by = list(sp_data_site$site_week),
        function(x) sum(!is.na(x)) > 0)
    sp_data_site <- sp_data_site[!(is.na(sp_data_site$COUNT) & (sp_data_site$site_week %in%
        siteweekcount$Group.1[siteweekcount$x == TRUE])), names(sp_data_site) !=
        "site_week"]

    # Compute the regional gam index
    print(paste("Compute index for", sp_data_site$SPECIES[1], "at year", y, "for", length(unique(sp_data_site$SITE)), "sites:", Sys.time()))
    regional_gam_index <- trap_index(sp_data_site, data_col = "COUNT_IMPUTED",
        time_col = "DAYNO", by_col = c("SPECIES", "SITE", "YEAR"))

    cumu_index <- merge(regional_gam_index, pro_count_agg, by = c("SITE"),
        all.x = TRUE, sort = FALSE)
    names(cumu_index) <- c("SITE", "SPECIES", "YEAR", "regional_gam", "prop_pheno_sampled")

    cumu_index <- cumu_index[order(cumu_index$SITE), ]

    # bind if exist else create
    if ("cumullated_indices" %in% ls()) {
        cumullated_indices <- rbind(cumullated_indices, cumu_index)
    } else {
        cumullated_indices <- cumu_index
    }

}

return(cumullated_indices)

}

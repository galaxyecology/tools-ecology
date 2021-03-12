#Rscript


##################################################################################################################################
####################### PAMPA Galaxy tools functions : Calculate metrics, compute GLM and plot   #################################
##################################################################################################################################

#### Based on Yves Reecht R script
#### Modified by Coline ROYAUX for integrating within Galaxy-E

######################################### start of the function fact.def.f called by FunctExeCalcCommIndexesGalaxy.r and FunctExeCalcPresAbsGalaxy.r
####### Define the finest aggregation with the observation table

fact_det_f <- function(obs,
                       size_class = "size.class",
                       code_species = "species.code",
                       unitobs = "observation.unit") {
    if (any(is.element(c(size_class), colnames(obs))) && all(! is.na(obs[, size_class]))) {
            factors <- c(unitobs, code_species, size_class)
        }else{
            factors <- c(unitobs, code_species)
        }
    return(factors)
}

######################################### end of the function fact.def.f

######################################### start of the function def_typeobs_f called by FunctExeCalcCommIndexesGalaxy.r and FunctExeCalcPresAbsGalaxy.r
####### Define observation type from colnames

def_typeobs_f <- function(obs) {
    if (any(is.element(c("rotation", "rot", "rotate"), colnames(obs)))) {
        obs_type <- "SVR"
    }else{
        obs_type <- "other"
    }
    return(obs_type)
}
######################################### end of the function fact.def.f

######################################### start of the function create_unitobs called by FunctExeCalcCommIndexesGalaxy.r and FunctExeCalcPresAbsGalaxy.r
####### Create unitobs column when inexistant
create_unitobs <- function(data, year = "year", location = "location", unitobs = "observation.unit") {
    if (is.element(paste(unitobs), colnames(data))) {
            unitab <- data
    }else{

        unitab <- tidyr::unite(data, col = "observation.unit", c(year, location))
    }
    return(unitab)
}
######################################### start of the function create_unitobs

######################################### start of the function create_year_location called by FunctExeCalcCommIndexesGalaxy.r and FunctExeCalcPresAbsGalaxy.r
####### separate unitobs column when existant
create_year_location <- function(data, year = "year", location = "location", unitobs = "observation.unit") {
    if (all(grepl("[1-2][0|8|9][0-9]{2}_.*", data[, unitobs])) == TRUE) {
        tab <- tidyr::separate(data, col = unitobs, into = c(year, location), sep = "_")
    }else{
        if (all(grepl("[A-Z]{2}[0-9]{2}.*", data[, unitobs]) == TRUE)) {
            tab <- tidyr::separate(data, col = unitobs, into = c("site1", year, "obs"), sep = c(2, 4))
            tab <- tidyr::unite(tab, col = location, c("site1", "obs"))
        }else{
            tab <- data
        }
    }

    tab <- cbind(tab, observation.unit = data[, unitobs])

    return(tab)
}
######################################### start of the function create_year_location

######################################### start of the function check_file called by every Galaxy Rscripts

check_file <- function(dataset, err_msg, vars, nb_vars) {

    ## Purpose: General function to check integrity of input file. Will
    ##          check numbers and contents of variables(colnames).
    ##          return an error message and exit if mismatch detected
    ## ----------------------------------------------------------------------
    ## Arguments: dataset : dataset name
    ##            err_msg : output error
    ##            vars : expected name of variables
    ##            nb_vars : expected number of variables
    ## ----------------------------------------------------------------------
    ## Author: Alan Amosse, Benjamin Yguel

    if (ncol(dataset) < nb_vars) { #checking for right number of columns in the file if not = error message
        cat("\nerr nb var\n")
        stop(err_msg, call. = FALSE)
    }

    for (i in vars) {
        if (!(i %in% names(dataset))) { #checking colnames
            stop(err_msg, call. = FALSE)
        }
    }
}

######################################### end of the function check_file


######################################### start of the function stat_rotations_nb_f called by calc_numbers_f

stat_rotations_nb_f <- function(factors, obs) {
    ## Purpose: Computing abundance statistics by rotation (max, sd)
    ##          on SVR data
    ## ----------------------------------------------------------------------
    ## Arguments: factors : Names of aggregation factors
    ##            obs : observation data
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2012, 16:01 modified by Coline ROYAUX 04 june 2020

    ## Identification of valid rotations :
    if (is.element("observation.unit", factors)) {
        ## valid rotations (empty must be there as well) :
        rotations <- tapply(obs$rotation,
                            as.list(obs[, c("observation.unit", "rotation"), drop = FALSE]),
                            function(x)length(x) > 0)

        ## Changing NA rotations in FALSE :
        rotations[is.na(rotations)] <- FALSE
    }

    ## ###########################################################
    ## Abundance per rotation at chosen aggregation factors :
    nombres_rot <- tapply(obs$number,
                       as.list(obs[, c(factors, "rotation"), drop = FALSE]),
                       function(x, ...) {
                       ifelse(all(is.na(x)), NA, sum(x, ...))
                                        },
                       na.rm = TRUE)

    ## If valid rotation NA are considered 0 :
    nombres_rot <- sweep(nombres_rot,
                      match(names(dimnames(rotations)), names(dimnames(nombres_rot)), nomatch = NULL),
                      rotations,        # Tableau des secteurs valides (booléens).
                      function(x, y) {
                          x[is.na(x) & y] <- 0 # Lorsque NA et secteur valide => 0.
                          return(x)
                                     })

    ## ##################################################
    ## Statistics :

    ## Means :
    nb_mean <- apply(nombres_rot, which(is.element(names(dimnames(nombres_rot)), factors)),
                         function(x, ...) {
                             ifelse(all(is.na(x)), NA, mean(x, ...))
                                          }, na.rm = TRUE)

    ## Maxima :
    nb_max <- apply(nombres_rot, which(is.element(names(dimnames(nombres_rot)), factors)),
                        function(x, ...) {
                            ifelse(all(is.na(x)), NA, max(x, ...))
                                         }, na.rm = TRUE)

    ## SD :
    nb_sd <- apply(nombres_rot, which(is.element(names(dimnames(nombres_rot)), factors)),
                       function(x, ...) {
                           ifelse(all(is.na(x)), NA, sd(x, ...))
                                        }, na.rm = TRUE)

    ## Valid rotations count :
    nombres_rotations <- apply(rotations, 1, sum, na.rm = TRUE)

    ## Results returned as list :
    return(list(nb_mean = nb_mean, nb_max = nb_max, nb_sd = nb_sd,
                nombres_rotations = nombres_rotations, nombresTot = nombres_rot))
}

######################################### end of the function stat_rotations_nb_f

######################################### start of the function calc_nb_default_f called by calc_numbers_f

calc_nb_default_f <- function(obs,
                                 factors = c("observation.unit", "species.code", "size.class"),
                                 nb_name = "number") {
    ## Purpose : Compute abundances at finest aggregation
    ## ---------------------------------------------------------------------
    ## Arguments: obs : observation table
    ##            factors : aggregation factors
    ##            nb_name : name of abundance column.
    ##
    ## Output: array with ndimensions = nfactors.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 13:38 modified by Coline ROYAUX 04 june 2020

    ## Sum individuals number :
    nbr <- tapply(obs[, nb_name],
                  as.list(obs[, factors]),
                  sum, na.rm = TRUE)

    ## Absences as "true zero" :
    nbr[is.na(nbr)] <- 0

    return(nbr)
}

######################################### end of the function calc_nb_default_f

######################################### start of the function calc_numbers_f

calc_numbers_f <- function(obs, obs_type = "", factors = c("observation.unit", "species.code", "size.class"), nb_name = "number") {
    ## Purpose: Produce data.frame used as table from output of calc_nb_default_f().
    ## ----------------------------------------------------------------------
    ## Arguments: obs : observation table
    ##            obs_type : Type of observation (SVR, LIT, ...)
    ##            factors : aggregation factors
    ##            nb_name : name of abundance column
    ##
    ## Output: data.frame with (N aggregation factors + 1) columns
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 13:46 modified by Coline ROYAUX 04 june 2020

    if (obs_type == "SVR") {
         ## Compute SVR abundances statistics :
         stat_rotations <- stat_rotations_nb_f(factors = factors,
                                                  obs = obs)

         ## Mean for rotating videos (3 rotations at most times) :
         nbr <- stat_rotations[["nb_mean"]]

    }else{

         nbr <- calc_nb_default_f(obs, factors, nb_name)
    }

    res <- as.data.frame(as.table(nbr), responseName = nb_name)

    if (is.element("size.class", colnames(res))) {
        res$size.class[res$size.class == ""] <- NA
    }

    ## If integer abundances :
    if (isTRUE(all.equal(res[, nb_name], as.integer(res[, nb_name])))) {
        res[, nb_name] <- as.integer(res[, nb_name])
    }

    if (obs_type == "SVR") {
        ## statistics on abundances :
        res[, "number.max"] <- as.vector(stat_rotations[["nb_max"]])
        res[, "number.sd"] <- as.vector(stat_rotations[["nb_sd"]])

    }

    return(res)
}

######################################### end of the function calc_numbers_f

######################################### start of the function pres_abs_f called by calc_biodiv_f

pres_abs_f <- function(nombres, logical = FALSE) {
    ## Purpose: Compute presence absence from abundances
    ## ----------------------------------------------------------------------
    ## Arguments: nombres : vector of individuals count.
    ##            logical : (boolean) results as boolean or 0/1 ?
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2010, 10:20 modified by Coline ROYAUX 04 june 2020

    if (any(nombres < 0, na.rm = TRUE)) {
        stop("Negative abundances!")
    }

    if (logical) {
        return(nombres > 0)
    }else{
        nombres[nombres > 0] <- 1
        return(nombres)
    }
}

######################################### end of the function pres_abs_f

######################################### start of the function bettercbind called by agregations_generic_f

bettercbind <- function(..., df_list = NULL, deparse.level = 1) {
    ## Purpose: Apply cbind to data frame with mathcing columns but without
    ##          redundancies.
    ## ----------------------------------------------------------------------
    ## Arguments: same as cbind...
    ##            df_list : data.frames list
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 janv. 2012, 21:10 modified by Coline ROYAUX 04 june 2020

    if (is.null(df_list)) {
        df_list <- list(...)
    }

    return(do.call(cbind,
                   c(list(df_list[[1]][, c(tail(colnames(df_list[[1]]), -1),
                                           head(colnames(df_list[[1]]), 1))]),
                     lapply(df_list[- 1],
                            function(x, coldel) {
                            return(x[, !is.element(colnames(x),
                                                    coldel),
                                     drop = FALSE])
                        },
                            coldel = colnames(df_list[[1]])),
                     deparse.level = deparse.level)))
}

######################################### end of the function bettercbind

######################################### start of the function agregation_f called by agregations_generic_f

agregation_f <- function(metric, d_ata, factors, cas_metric,
                         nb_name = "number") {
    ## Purpose: metric aggregation
    ## ----------------------------------------------------------------------
    ## Arguments: metric: colnames of chosen metric
    ##            d_ata: Unaggregated data table
    ##            factors: aggregation factors vector
    ##            cas_metric: named vector of observation types depending
    ##                         on chosen metric
    ##            nb_name : abundance column name
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 14:29 modified by Coline ROYAUX 04 june 2020

    switch(cas_metric[metric],
           "sum" = {
               res <- tapply(d_ata[, metric],
                             as.list(d_ata[, factors, drop = FALSE]),
                             function(x) {
                             ifelse(all(is.na(x)),
                                    NA,
                                    sum(x, na.rm = TRUE))
                         })
           },
           "w.mean" = {
               res <- tapply(seq_len(nrow(d_ata)),
                             as.list(d_ata[, factors, drop = FALSE]),
                             function(ii) {
                             ifelse(all(is.na(d_ata[ii, metric])),
                                    NA,
                                    weighted.mean(d_ata[ii, metric],
                                                  d_ata[ii, nb_name],
                                                  na.rm = TRUE))
                         })
           },
           "w.mean.colonies" = {
               res <- tapply(seq_len(nrow(d_ata)),
                             as.list(d_ata[, factors, drop = FALSE]),
                             function(ii) {
                             ifelse(all(is.na(d_ata[ii, metric])),
                                    NA,
                                    weighted.mean(d_ata[ii, metric],
                                                  d_ata[ii, "colonies"],
                                                  na.rm = TRUE))
                         })
           },
           "w.mean.prop" = {
               res <- tapply(seq_len(nrow(d_ata)),
                             as.list(d_ata[, factors, drop = FALSE]),
                             function(ii) {
                             ifelse(all(is.na(d_ata[ii, metric])) || sum(d_ata[ii, "nombre.tot"], na.rm = TRUE) == 0,
                                    NA,
                                    ifelse(all(na.omit(d_ata[ii, metric]) == 0),
                                           0,
                                           (sum(d_ata[ii, nb_name][!is.na(d_ata[ii, metric])], na.rm = TRUE) /
                                            sum(d_ata[ii, "nombre.tot"], na.rm = TRUE)) *
                                           ## Correction if size class isn't an aggregation factor
                                           ## (otherwise value divided by number of present classes) :
                                           ifelse(is.element("size.class", factors),
                                                  100,
                                                  100 * length(unique(d_ata$size.class)))))
                         })

           },
           "w.mean.prop.bio" = {
               res <- tapply(seq_len(nrow(d_ata)),
                             as.list(d_ata[, factors, drop = FALSE]),
                             function(ii) {
                             ifelse(all(is.na(d_ata[ii, metric])) || sum(d_ata[ii, "tot.biomass"], na.rm = TRUE) == 0,
                                    NA,
                                    ifelse(all(na.omit(d_ata[ii, metric]) == 0),
                                           0,
                                           (sum(d_ata[ii, "biomass"][!is.na(d_ata[ii, metric])], na.rm = TRUE) /
                                            sum(d_ata[ii, "tot.biomass"], na.rm = TRUE)) *
                                           ## Correction if size class isn't an aggregation factor
                                           ## (otherwise value divided by number of present classes) :
                                           ifelse(is.element("size.class", factors),
                                                  100,
                                                  100 * length(unique(d_ata$size.class)))))
                         })

           },
           "pres" = {
               res <- tapply(d_ata[, metric],
                             as.list(d_ata[, factors, drop = FALSE]),
                             function(x) {
                             ifelse(all(is.na(x)), # When only NAs.
                                    NA,
                                    ifelse(any(x > 0, na.rm = TRUE), # Otherwise...
                                           1, # ... presence if at least one observation in the group.
                                           0))
                         })
           },
           "nbMax" = {

              ## Sum by factor cross / rotation :
               nb_tmp2 <- apply(nb_tmp,
                             which(is.element(names(dimnames(nb_tmp)), c(factors, "rotation"))),
                             function(x) {
                             ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
                         })

               ## Sum by factor cross :
               res <- as.array(apply(nb_tmp2,
                                     which(is.element(names(dimnames(nb_tmp)), factors)),
                                     function(x) {
                                     ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
                                 }))
           },
           "nbSD" = {

               ## Sum by factor cross / rotation :
               nb_tmp2 <- apply(nb_tmp,
                             which(is.element(names(dimnames(nb_tmp)), c(factors, "rotation"))),
                             function(x) {
                             ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
                         })

               ## Sum by factor cross :
               res <- as.array(apply(nb_tmp2,
                                     which(is.element(names(dimnames(nb_tmp)), factors)),
                                     function(x) {
                                     ifelse(all(is.na(x)), NA, sd(x, na.rm = TRUE))
                                 }))
           },
           "densMax" = {

               ## Sum by factor cross / rotation :
               dens_tmp2 <- apply(dens_tmp,
                                 which(is.element(names(dimnames(dens_tmp)), c(factors, "rotation"))),
                                 function(x) {
                                 ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
                             })

               ## Sum by factor cross :
               res <- as.array(apply(dens_tmp2,
                                     which(is.element(names(dimnames(dens_tmp)), factors)),
                                     function(x) {
                                     ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
                                 }))
           },
           "densSD" = {

               ## Sum by factor cross / rotation :
               dens_tmp2 <- apply(dens_tmp,
                                 which(is.element(names(dimnames(dens_tmp)), c(factors, "rotation"))),
                                 function(x) {
                                 ifelse(all(is.na(x)), NA, sum(x, na.rm = TRUE))
                             })

               ## Sum by factor cross :
               res <- as.array(apply(dens_tmp2,
                                     which(is.element(names(dimnames(dens_tmp)), factors)),
                                     function(x) {
                                     ifelse(all(is.na(x)), NA, sd(x, na.rm = TRUE))
                                 }))
           },
           "%.nesting" = {
               res <- tapply(seq_len(nrow(d_ata)),
                             as.list(d_ata[, factors, drop = FALSE]),
                             function(ii) {
                             ifelse(all(is.na(d_ata[ii, metric])),
                                    NA,
                                    weighted.mean(d_ata[ii, metric],
                                                  d_ata[ii, "readable.tracks"],
                                                  na.rm = TRUE))
                         })
           },
           stop("Not implemented!")
           )

    ## dimension names
    names(dimnames(res)) <- c(factors)

    ## Transformation to long format :
    reslong <- as.data.frame(as.table(res), responseName = metric)
    reslong <- reslong[, c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # metric first

    return(reslong)
}

######################################### end of the function agregation_f

######################################### start of the function agregations_generic_f called y calc_biodiv_f in FucntExeCalcCommIndexesGalaxy.r

agregations_generic_f <- function(d_ata, metrics, factors, list_fact = NULL, unit_sp_sz = NULL, unit_sp = NULL,
                                  nb_name = "number") {
    ## Purpose: Aggregate data
    ## ----------------------------------------------------------------------
    ## Arguments: d_ata : data set
    ##            metrics : aggregated metric
    ##            factors : aggregation factors
    ##            list_fact : other factors to aggregate and add to output
    ##            unit_sp_sz : Metrics table by unitobs/species/Size Class
    ##            unit_sp : Metrics table by unitobs/species
    ##            nb_name : abundance colname
    ##
    ## Output : aggregated data frame
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 oct. 2010, 15:47 modified by Coline ROYAUX 04 june 2020

    ## trt depending on metric type :
    cas_metric <- c("number" = "sum",
                     "mean.length" = "w.mean",
                     "taille_moy" = "w.mean",
                     "biomass" = "sum",
                     "Biomass" = "sum",
                     "weight" = "sum",
                     "mean.weight" = "w.mean",
                     "density" = "sum",
                     "Density" = "sum",
                     "CPUE" = "sum",
                     "CPUE.biomass" = "sum",
                     "presence_absence" = "pres",
                     "abundance.prop.SC" = "w.mean.prop", # Not OK [!!!] ?
                     "biomass.prop.SC" = "w.mean.prop.bio",  # Not OK [!!!] ?
                     ## Benthos :
                     "colonies" = "sum",
                     "coverage" = "sum",
                     "mean.size.colonies" = "w.mean.colonies",
                     ## SVR (expérimental) :
                     "number.max" = "nbMax",
                     "number.sd" = "nbSD",
                     "density.max" = "densMax",
                     "density.sd" = "densSD",
                     "biomass.max" = "sum",
                     "spawning.success" = "%.nesting",
                     "spawnings" = "sum",
                     "readable.tracks" = "sum",
                     "tracks.number" = "sum")

    ## add "readable.tracks" for egg laying percentage :
    if (any(cas_metric[metrics] == "%.nesting")) {
        if (is.element("size.class", colnames(d_ata))) {
            if (is.null(unit_sp_sz)) stop("unit_sp_sz doit être défini")

            d_ata <- merge(d_ata,
                          unit_sp_sz[, c("species.code", "observation.unit", "size.class", "readable.tracks")],
                          by = c("species.code", "observation.unit", "size.class"),
                          suffixes = c("", ".y"))
        }else{
            if (is.null(unit_sp)) stop("unit_sp must be defined")

            d_ata <- merge(d_ata,
                          unit_sp[, c("species.code", "observation.unit", "readable.tracks")],
                          by = c("species.code", "observation.unit"),
                          suffixes = c("", ".y"))
        }
    }

    ## Add "number" field for computing ponderate means if absent :
    if (any(cas_metric[metrics] == "w.mean" | cas_metric[metrics] == "w.mean.prop")) {
        if (is.element("size.class", colnames(d_ata))) {
            if (is.null(unit_sp_sz)) stop("unit_sp_sz must be defined")

            d_ata <- merge(d_ata,
                          unit_sp_sz[, c("species.code", "observation.unit", "size.class", nb_name)],
                          by = c("species.code", "observation.unit", "size.class"),
                          suffixes = c("", ".y"))

            ## add tot abundance / species / observation unit :
            nb_tot <- tapply(unit_sp_sz[, nb_name],
                            as.list(unit_sp_sz[, c("species.code", "observation.unit")]),
                            sum, na.rm = TRUE)

            d_ata <- merge(d_ata,
                          as.data.frame(as.table(nb_tot), responseName = "nombre.tot"))
        }else{
            if (is.null(unit_sp)) stop("unit_sp must be defined")

            d_ata <- merge(d_ata,
                          unit_sp[, c("species.code", "observation.unit", nb_name)], # [!!!] unit_sp_sz ?
                          by = c("species.code", "observation.unit"),
                          suffixes = c("", ".y"))
        }
    }

    ## Add biomass field of biomass proportion by size class :
    if (any(cas_metric[metrics] == "w.mean.prop.bio")) {
        if (is.null(unit_sp_sz)) stop("unit_sp_sz doit être défini")

        d_ata <- merge(d_ata,
                      unit_sp_sz[, c("species.code", "observation.unit", "size.class", "biomass")],
                      by = c("species.code", "observation.unit", "size.class"),
                      suffixes = c("", ".y"))

        ## add tot biomass / species / observation unit :
        biom_tot <- tapply(unit_sp_sz$biomass,
                          as.list(unit_sp_sz[, c("species.code", "observation.unit")]),
                          function(x) {
                          ifelse(all(is.na(x)),
                                 NA,
                                 sum(x, na.rm = TRUE))
                      })

        d_ata <- merge(d_ata,
                      as.data.frame(as.table(biom_tot), responseName = "tot.biomass"))
    }

    ## add colony field for ponderate means pondérées if absent :
    if (any(cas_metric[metrics] == "w.mean.colonies" & ! is.element("colonies", colnames(d_ata)))) {
        d_ata$colonies <- unit_sp[match(apply(d_ata[, c("species.code", "observation.unit")],
                                           1, paste, collapse = "*"),
                                     apply(unit_sp[, c("species.code", "observation.unit")],
                                           1, paste, collapse = "*")), "colonies"]
    }


    ## Aggregation of metric depending on factors :
    reslong <- bettercbind(df_list = lapply(metrics,   # sapply used to have names
                                         agregation_f,
                                         d_ata = d_ata, factors = factors, cas_metric = cas_metric,
                                         nb_name = nb_name))

    ## Aggregation and add other factors :
    if (! (is.null(list_fact) || length(list_fact) == 0)) {
        reslong <- cbind(reslong,
                         sapply(d_ata[, list_fact, drop = FALSE],
                                function(fact) {
                                tapply(fact,
                                       as.list(d_ata[, factors, drop = FALSE]),
                                       function(x) {
                                       if (length(x) > 1 && length(unique(x)) > 1) { # must be one modality
                                           return(NULL)                  # otherwise it is NULL
                                       }else{
                                           unique(as.character(x))
                                       }
                                   })
                            }))
    }

    ## If some factors aren't at the right class :
    if (any(tmp <- sapply(reslong[, list_fact, drop = FALSE], class) != sapply(d_ata[, list_fact, drop = FALSE], class))) {
        for (i in which(tmp)) {
            switch(sapply(d_ata[, list_fact, drop = FALSE], class)[i],
                   "integer" = {
                       reslong[, list_fact[i]] <- as.integer(as.character(reslong[, list_fact[i]]))
                   },
                   "numeric" = {
                       reslong[, list_fact[i]] <- as.numeric(as.character(reslong[, list_fact[i]]))
                   },
                   reslong[, list_fact[i]] <- eval(call(paste("as", sapply(d_ata[, list_fact, drop = FALSE], class)[i], sep = "."),
                                                        reslong[, list_fact[i]]))
                   )
        }
    }

    ## Initial order of factors levels :
    reslong <- as.data.frame(sapply(colnames(reslong),
                                    function(x) {
                                    if (is.factor(reslong[, x])) {
                                        return(factor(reslong[, x], levels = levels(d_ata[, x])))
                                    }else{
                                        return(reslong[, x])
                                    }
                                }, simplify = FALSE))


    ## Check of other aggregated factors supplémentaires. There must be no NULL elements :
    if (any(sapply(reslong[, list_fact], function(x) {
                                             any(is.null(unlist(x)))
                                                     }))) {
        warning(paste("One of the suppl. factors is probably a subset",
                      " of the observations grouping factor(s).", sep = ""))
        return(NULL)
    }else{
        return(reslong)
    }
}

######################################### end of the function agregations_generic_f

######################################### start of the function drop_levels_f called y calc_biodiv_f in FucntExeCalcCommIndexesGalaxy.r and glm_community in FunctExeCalcGLMGalaxy.r
drop_levels_f <- function(df, which = NULL) {
    ## Purpose: Suppress unused levels of factors
    ## ----------------------------------------------------------------------
    ## Arguments: df : a data.frame
    ##            which : included columns index (all by default)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 août 2010, 13:29 modified by Coline ROYAUX 04 june 2020

    if (class(df) != "data.frame") {
        stop("'df' must be a data.frame")
    }else{
        if (is.null(which)) {
            x <- as.data.frame(sapply(df, function(x) {
                                      return(x[, drop = TRUE])
                                  }, simplify = FALSE),
                               stringsAsFactors = FALSE)
        }else{                          # Only some columns used
            x <- df

            x[, which] <- as.data.frame(sapply(df[, which, drop = FALSE],
                                                function(x) {
                                                return(x[, drop = TRUE])
                                            }, simplify = FALSE),
                                         stringsAsFactors = FALSE)
        }

        return(x)
    }
}
######################################### end of the function drop_levels_f

######################################### start of the function subset_all_tables_f called by glm_community in FunctExeCalcGLMGalaxy.r

subset_all_tables_f <- function(metrique, tab_metrics, facteurs, selections,
                                 tab_unitobs, refesp, tab_metrique = "", nb_name = "number", obs_type = "",
                                 exclude = NULL, add = c("species.code", "observation.unit")) {
    ## Purpose: Extract useful data only from chosen metrics and factors
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : chosen metric
    ##            facteurs : all chosen factors
    ##            selections : corresponding modality selected
    ##            tab_metrique : metrics table name
    ##            exclude : factors levels to exclude
    ##            add : field to add to data table
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 août 2010, 16:46 modified by Coline ROYAUX 04 june 2020

    ## If no metrics table available :
    if (is.element(tab_metrique, c("", "TableOccurrences", "TablePresAbs"))) {
        tab_metrique <- "unit_sp"
    }

    cas_tables <- c("unit_sp" = "unit_sp",
                   "TablePresAbs" = "unit_sp",
                   "unit_sp_sz" = "unit_sp_sz")

    ## Recuperation of metrics table :
    data_metric <- tab_metrics
    unitobs <- tab_unitobs
    refesp <- refesp

    ## If no metrics available or already computed :
    if (is.element(metrique, c("", "occurrence.frequency"))) {
        metrique <- "tmp"
        data_metric$tmp <- 0
        data_metric$tmp[data_metric[, nb_name] > 0] <- 1
    }

    if (!is.null(add)) {
        metriques <- c(metrique, add[is.element(add, colnames(data_metric))])
    }else{
        metriques <- metrique
    }

    ## Subset depending on metrics table
    switch(cas_tables[tab_metrique],
           ## Observation table by unitobs and species :
           unit_sp = {
                restmp <- cbind(data_metric[!is.na(data_metric[, metrique]), metriques, drop = FALSE],
                                unitobs[match(data_metric$observation.unit[!is.na(data_metric[, metrique])],
                                              unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs
                                        facteurs[is.element(facteurs, colnames(unitobs))], drop = FALSE],
                                refesp[match(data_metric$species.code[!is.na(data_metric[, metrique])],
                                             refesp$species.code),        # ajout des colonnes sélectionnées d'especes
                                       facteurs[is.element(facteurs, colnames(refesp))], drop = FALSE])
            },
           ## Observation table by unitobs, species and size class :
           unit_sp_sz = {
               restmp <- cbind(data_metric[!is.na(data_metric[, metrique]),
                                            c(metriques, "size.class"), drop = FALSE],
                               unitobs[match(data_metric$observation.unit[!is.na(data_metric[, metrique])],
                                             unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs
                                       facteurs[is.element(facteurs, colnames(unitobs))], drop = FALSE],
                               refesp[match(data_metric$species.code[!is.na(data_metric[, metrique])],
                                            refesp$species.code),        # ajout des colonnes sélectionnées d'especes
                                      facteurs[is.element(facteurs, colnames(refesp))], drop = FALSE])
           },
           ## Other cases :
           restmp <- cbind(data_metric[!is.na(data_metric[, metrique]), metriques, drop = FALSE],
                           unitobs[match(data_metric$observation.unit[!is.na(data_metric[, metrique])],
                                         unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs.
                                   facteurs[is.element(facteurs, colnames(unitobs))], drop = FALSE])
           )

    sel_col <- which(!is.na(selections))
    if (!is.null(exclude)) {
        sel_col <- sel_col[sel_col != exclude]
    }

    ## Particular case of size classes :
    if (is.element("size.class", colnames(restmp))) {
        if (length(grep("^[[:digit:]]*[-_][[:digit:]]*$", unique(as.character(restmp$size.class)), perl = TRUE)) ==
            length(unique(as.character(restmp$size.class)))) {
            restmp[, "size.class"] <-
                factor(as.character(restmp$size.class),
                       levels = unique(as.character(restmp$size.class))[
                               order(as.numeric(sub("^([[:digit:]]*)[-_][[:digit:]]*$",
                                                    "\\1",
                                                    unique(as.character(restmp$size.class)),
                                                    perl = TRUE)),
                                     na.last = FALSE)])
        }else{
            restmp[, "size.class"] <- factor(restmp$size.class)
        }
    }

    ## Biomass and density conversion -> /100m² :
    if (any(is.element(colnames(restmp), c("biomass", "density",
                                           "biomass.max", "density.max",
                                           "biomass.sd", "density.sd"))) && obs_type != "fishing") {
        restmp[, is.element(colnames(restmp),
                             c("biomass", "density",
                               "biomass.max", "density.max",
                               "biomass.sd", "density.sd"))] <- 100 *
                                   restmp[, is.element(colnames(restmp),
                                                       c("biomass", "density",
                                                         "biomass.max", "density.max",
                                                         "biomass.sd", "density.sd"))]
    }

    return(restmp)
}

######################################### end of the function subset_all_tables_f

######################################### start of the function organise_fact called by modeleLineaireWP2.xxx.f in FunctExeCalcGLMxxGalaxy.r

organise_fact <- function(list_rand, list_fact) {
    ## Purpose: organise response factors
    ## ----------------------------------------------------------------------
    ## Arguments: list_rand : Analysis random factors list
    ##            list_fact : Analysis factors list
    ## ----------------------------------------------------------------------
    ## Author: Coline ROYAUX 14 november 2020

    if (list_rand[1] != "None") {
        if (all(is.element(list_fact, list_rand)) || list_fact[1] == "None") {
            resp_fact <- paste("(1|", paste(list_rand, collapse = ") + (1|"), ")")
            list_f <- NULL
            list_fact <- list_rand
        }else{
            list_f <- list_fact[!is.element(list_fact, list_rand)]
            resp_fact <- paste(paste(list_f, collapse = " + "), " + (1|", paste(list_rand, collapse = ") + (1|"), ")")
            list_fact <- c(list_f, list_rand)
        }
    }else{
        list_f <- list_fact
        resp_fact <- paste(list_fact, collapse = " + ")
    }
    return(list(resp_fact, list_f, list_fact))
}

######################################### end of the function organise_fact

######################################### start of the function organise_fact called by modeleLineaireWP2.xxx.f in FunctExeCalcGLMxxGalaxy.r
distrib_choice <- function(distrib = distrib, metrique = metrique, data = tmpd_ata) {
    ## Purpose: choose the right distribution
    ## ----------------------------------------------------------------------
    ## Arguments: data : data used for analysis
    ##            metrique : Chosen metric
    ##            distrib : distribution law selected by user
    ## ----------------------------------------------------------------------
    ## Author: Coline ROYAUX 14 november 2020

    if (distrib == "None") {
        if (metrique == "presence_absence") {
            chose_distrib <- "binomial"
        }else{
            switch(class(data[, metrique]),
                  "integer" = {
                                   chose_distrib <- "poisson"
                              },
                  "numeric" = {
                                   chose_distrib <- "gaussian"
                              },
                  stop("Selected metric class doesn't fit, you should select an integer or a numeric variable"))
        }
    }else{
        chose_distrib <- distrib
    }
    return(chose_distrib)
}

######################################### end of the function organise_fact

######################################### start of the function create_res_table called by modeleLineaireWP2.xxx.f in FunctExeCalcGLMxxGalaxy.r
create_res_table <- function(list_rand, list_fact, row, lev, distrib) {
    ## Purpose: create results table
    ## ----------------------------------------------------------------------
    ## Arguments: list_rand : Analysis random factors list
    ##            list_fact : Analysis factors list
    ##            row : rows of results table = species or separation factor
    ##            lev : Levels of analysis factors list
    ##            distrib : distribution law
    ## ----------------------------------------------------------------------
    ## Author: Coline ROYAUX 04 october 2020

    if (list_rand[1] != "None") { ## if random effects
        tab_sum <- data.frame(analysis = row, Interest.var = NA, distribution = NA, AIC = NA, BIC = NA, logLik = NA, deviance = NA, df.resid = NA)
        colrand <- unlist(lapply(list_rand,
                           FUN = function(x) {
                                     lapply(c("Std.Dev", "NbObservation", "NbLevels"),
                                                  FUN = function(y) {
                                                            paste(x, y, collapse = ":")
                                                                    })
                                             }))
        tab_sum[, colrand] <- NA

        if (! is.null(lev)) { ## if fixed effects + random effects
            colcoef <- unlist(lapply(c("(Intercept)", lev),
                               FUN = function(x) {
                                         lapply(c("Estimate", "Std.Err", "Zvalue", "Pvalue", "IC_up", "IC_inf", "signif"),
                                                      FUN = function(y) {
                                                                paste(x, y, collapse = ":")
                                                                        })
                                                 }))

        }else{ ## if no fixed effects
            colcoef <- NULL
        }

    }else{ ## if no random effects
        tab_sum <- data.frame(analysis = row, Interest.var = NA, distribution = NA, AIC = NA, Resid.deviance = NA, df.resid = NA, Null.deviance = NA, df.null = NA)

        switch(distrib,
               "gaussian" = {
                                 colcoef <- unlist(lapply(c("(Intercept)", lev),
                                                          FUN = function(x) {
                                                                    lapply(c("Estimate", "Std.Err", "Tvalue", "Pvalue", "IC_up", "IC_inf", "signif"),
                                                                    FUN = function(y) {
                                                                              paste(x, y, collapse = ":")
                                                                                      })
                                                                            }))

                           },
               "quasipoisson" = {
                                     colcoef <- unlist(lapply(c("(Intercept)", lev),
                                                              FUN = function(x) {
                                                                        lapply(c("Estimate", "Std.Err", "Tvalue", "Pvalue", "IC_up", "IC_inf", "signif"),
                                                                    FUN = function(y) {
                                                                              paste(x, y, collapse = ":")
                                                                                      })
                                                                                }))

                               }
             , {
                    colcoef <- unlist(lapply(c("(Intercept)", lev),
                                      FUN = function(x) {
                                                lapply(c("Estimate", "Std.Err", "Zvalue", "Pvalue", "IC_up", "IC_inf", "signif"),
                                                       FUN = function(y) {
                                                                 paste(x, y, collapse = ":")
                                                                         })
                                                        }))
                })

    }

    tab_sum[, colcoef] <- NA


    return(tab_sum)
}
######################################### end of the function create_res_table

######################################### start of the function sorties_lm_f called by glm_community in FunctExeCalcGLMGalaxy.r
sorties_lm_f <- function(obj_lm, obj_lmy, tab_sum, #formule,
                        metrique, fact_ana, cut, col_ana, list_fact, list_rand, lev = NULL, d_ata,
                        log = FALSE, sufixe = NULL) {
    ## Purpose: Form GLM and LM results
    ## ----------------------------------------------------------------------
    ## Arguments: obj_lm : lm object
    ##            obj_lmy : lm object with year as continuous
    ##            tab_sum : output summary table
    ##            formule : LM formula
    ##            metrique : Chosen metric
    ##            fact_ana : separation factor
    ##            cut : level of separation factor
    ##            col_ana : colname for separation factor in output summary table
    ##            list_fact : Analysis factors list
    ##            list_rand : Analysis random factors list
    ##            levels : Levels of analysis factors list
    ##            d_ata : d_ata used for analysis
    ##            log : put log on metric ? (boolean)
    ##            sufixe : sufix for file name
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 août 2010, 16:19 modified by Coline ROYAUX 04 june 2020

    tab_sum[, "Interest.var"] <- as.character(metrique)
    sum_lm <- summary(obj_lm)
    tab_sum[, "distribution"] <- as.character(sum_lm$family[1])

    if (length(grep("^glmmTMB", obj_lm$call)) > 0) { #if random effects
        tab_sum[tab_sum[, col_ana] == cut, "AIC"] <- sum_lm$AICtab[1]
        tab_sum[tab_sum[, col_ana] == cut, "BIC"] <- sum_lm$AICtab[2]
        tab_sum[tab_sum[, col_ana] == cut, "logLik"] <- sum_lm$AICtab[3]
        tab_sum[tab_sum[, col_ana] == cut, "deviance"] <- sum_lm$AICtab[4]
        tab_sum[tab_sum[, col_ana] == cut, "df.resid"] <- sum_lm$AICtab[5]

        if (! is.null(lev)) { ## if fixed effects + random effects
            tab_coef <- as.data.frame(sum_lm$coefficients$cond)
            tab_coef$signif <- lapply(tab_coef[, "Pr(>|z|)"], FUN = function(x) {
                                                                                     if (!is.na(x) && x < 0.05) {
                                                                                         "yes"
                                                                                     }else{
                                                                                         "no"
                                                                                     }
                                                                                })

            tab_sum[tab_sum[, col_ana] == cut, grepl("Intercept.*Zvalue", colnames(tab_sum))] <- tab_coef[grepl("Intercept", rownames(tab_coef)), "z value"]
            tab_sum[tab_sum[, col_ana] == cut, grepl("Intercept.*Pvalue", colnames(tab_sum))] <- tab_coef[grepl("Intercept", rownames(tab_coef)), "Pr(>|z|)"]

            tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "Zvalue", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
                 if (length(grep(x, rownames(tab_coef))) > 0) {
                     tab_coef[grepl(x, rownames(tab_coef)), "z value"]
                 }else{
                     NA
                 }
            }))
            tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "Pvalue", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
                 if (length(grep(x, rownames(tab_coef))) > 0) {
                      tab_coef[grepl(x, rownames(tab_coef)), "Pr(>|z|)"]
                 }else{
                      NA
                 }
            }))

            if (any(obj_lmy != "")) {
                sum_lmy <- summary(obj_lmy)
                tab_coefy <- as.data.frame(sum_lmy$coefficients$cond)
                tab_coefy$signif <- lapply(tab_coefy[, "Pr(>|z|)"], FUN = function(x) {
                                                                                           if (!is.na(x) && x < 0.05) {
                                                                                               "yes"
                                                                                           }else{
                                                                                               "no"
                                                                                           }
                                                                                      })
                tab_sum[tab_sum[, col_ana] == cut, "year Zvalue"] <- ifelse(length(tab_coefy["year", "z value"]) > 0, tab_coefy["year", "z value"], NA)
                tab_sum[tab_sum[, col_ana] == cut, "year Pvalue"] <- ifelse(length(tab_coefy["year", "Pr(>|z|)"]) > 0, tab_coefy["year", "Pr(>|z|)"], NA)
            }

        }

        switch(as.character(length(sum_lm$varcor$cond)),
               "1" = {
                          std_d <- c(sum_lm$varcor$cond[[1]])
                     },
               "2" = {
                          std_d <- c(sum_lm$varcor$cond[[1]], sum_lm$varcor$cond[[2]])
                     },
               std_d <- NULL)

        tab_sum[tab_sum[, col_ana] == cut, grepl(paste(list_rand, "Std.Dev", collapse = "|"), colnames(tab_sum))] <- std_d
        tab_sum[tab_sum[, col_ana] == cut, grepl(paste(list_rand, "NbObservation", collapse = "|"), colnames(tab_sum))] <- sum_lm$nobs
        tab_sum[tab_sum[, col_ana] == cut, grepl(paste(list_rand, "NbLevels", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(list_rand, FUN = function(x) {
          nlevels(d_ata[, x])
                  }))

    }else{ ## if fixed effects only

        tab_sum[tab_sum[, col_ana] == cut, "AIC"] <- sum_lm$aic
        tab_sum[tab_sum[, col_ana] == cut, "Resid.deviance"] <- sum_lm$deviance
        tab_sum[tab_sum[, col_ana] == cut, "df.resid"] <- sum_lm$df.residual
        tab_sum[tab_sum[, col_ana] == cut, "Null.deviance"] <- sum_lm$null.deviance
        tab_sum[tab_sum[, col_ana] == cut, "df.null"] <- sum_lm$df.null
        tab_coef <- as.data.frame(sum_lm$coefficients)

        if (any(obj_lmy != "")) {
            sum_lmy <- summary(obj_lmy)
            tab_coefy <- as.data.frame(sum_lmy$coefficients)
        }

        if (sum_lm$family[1] == "gaussian" || sum_lm$family[1] == "quasipoisson") {

            tab_coef$signif <- lapply(tab_coef[, "Pr(>|t|)"], FUN = function(x) {
                                                                                     if (!is.na(x) && x < 0.05) {
                                                                                          "yes"
                                                                                     }else{
                                                                                          "no"
                                                                                     }
                                                                                 })
            tab_sum[tab_sum[, col_ana] == cut, grepl("Intercept.*Tvalue", colnames(tab_sum))] <- tab_coef[grepl("Intercept", rownames(tab_coef)), "t value"]
            tab_sum[tab_sum[, col_ana] == cut, grepl("Intercept.*Pvalue", colnames(tab_sum))] <- tab_coef[grepl("Intercept", rownames(tab_coef)), "Pr(>|t|)"]

            tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "Tvalue", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
                 if (length(grep(x, rownames(tab_coef))) > 0) {
                     tab_coef[grepl(x, rownames(tab_coef)), "t value"]
                 }else{
                     NA
                 }
            }))

            tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "Pvalue", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
                 if (length(grep(x, rownames(tab_coef))) > 0) {
                     tab_coef[grepl(x, rownames(tab_coef)), "Pr(>|t|)"]
                 }else{
                      NA
                 }
            }))

            if (any(obj_lmy != "")) {
                tab_coefy$signif <- lapply(tab_coefy[, "Pr(>|t|)"], FUN = function(x) {
                                                                                           if (!is.na(x) && x < 0.05) {
                                                                                               "yes"
                                                                                           }else{
                                                                                               "no"
                                                                                           }
                                                                                      })
                tab_sum[tab_sum[, col_ana] == cut, "year Tvalue"] <- ifelse(length(tab_coefy["year", "t value"]) > 0, tab_coefy["year", "t value"], NA)
                tab_sum[tab_sum[, col_ana] == cut, "year Pvalue"] <- ifelse(length(tab_coefy["year", "Pr(>|z|)"]) > 0, tab_coefy["year", "Pr(>|t|)"], NA)
            }

        }else{
            tab_coef$signif <- lapply(tab_coef[, "Pr(>|z|)"], FUN = function(x) {
                                                                                     if (!is.na(x) && x < 0.05) {
                                                                                         "yes"
                                                                                     }else{
                                                                                         "no"
                                                                                     }
                                                                                })

            tab_sum[tab_sum[, col_ana] == cut, grepl("Intercept.*Zvalue", colnames(tab_sum))] <- tab_coef[grepl("Intercept", rownames(tab_coef)), "z value"]
            tab_sum[tab_sum[, col_ana] == cut, grepl("Intercept.*Pvalue", colnames(tab_sum))] <- tab_coef[grepl("Intercept", rownames(tab_coef)), "Pr(>|z|)"]

            tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "Zvalue", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
                 if (length(grep(x, rownames(tab_coef))) > 0) {
                     tab_coef[grepl(x, rownames(tab_coef)), "z value"]
                 }else{
                     NA
                 }
            }))
            tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "Pvalue", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
                 if (length(grep(x, rownames(tab_coef))) > 0) {
                     tab_coef[grepl(x, rownames(tab_coef)), "Pr(>|z|)"]
                 }else{
                     NA
                 }
            }))

            if (any(obj_lmy != "")) {
                tab_coefy$signif <- lapply(tab_coefy[, "Pr(>|z|)"], FUN = function(x) {
                                                                                           if (!is.na(x) && x < 0.05) {
                                                                                               "yes"
                                                                                           }else{
                                                                                               "no"
                                                                                           }
                                                                                      })

                tab_sum[tab_sum[, col_ana] == cut, "year Zvalue"] <- ifelse(length(tab_coefy["year", "z value"]) > 0, tab_coefy["year", "z value"], NA)
                tab_sum[tab_sum[, col_ana] == cut, "year Pvalue"] <- ifelse(length(tab_coefy["year", "Pr(>|z|)"]) > 0, tab_coefy["year", "Pr(>|z|)"], NA)
            }
        }
    }

    if (! is.null(lev)) { ## if fixed effects
        tab_sum[tab_sum[, col_ana] == cut, grepl("Intercept.*Estimate", colnames(tab_sum))] <- tab_coef[grepl("Intercept", rownames(tab_coef)), "Estimate"]
        tab_sum[tab_sum[, col_ana] == cut, grepl("Intercept.*Std.Err", colnames(tab_sum))] <- tab_coef[grepl("Intercept", rownames(tab_coef)), "Std. Error"]
        tab_sum[tab_sum[, col_ana] == cut, grepl("Intercept.*signif", colnames(tab_sum))] <- tab_coef[grepl("Intercept", rownames(tab_coef)), "signif"]

        tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "Estimate", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
                 if (length(grep(x, rownames(tab_coef))) > 0) {
                     tab_coef[grepl(x, rownames(tab_coef)), "Estimate"]
                 }else{
                     NA
                 }
            }))
        tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "Std.Err", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
                 if (length(grep(x, rownames(tab_coef))) > 0) {
                     tab_coef[grepl(x, rownames(tab_coef)), "Std. Error"]
                 }else{
                     NA
                 }
            }))
        tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "signif", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
                 if (length(grep(x, rownames(tab_coef))) > 0) {
                     tab_coef[grepl(x, rownames(tab_coef)), "signif"]
                 }else{
                     NA
                 }
            }))

        if (any(obj_lmy != "")) {
            tab_sum[tab_sum[, col_ana] == cut, "year Estimate"] <- ifelse(length(tab_coefy["year", "Estimate"]) > 0, tab_coefy["year", "Estimate"], NA)
            tab_sum[tab_sum[, col_ana] == cut, "year Std.Err"] <- ifelse(length(tab_coefy["year", "Std. Error"]) > 0, tab_coefy["year", "Std. Error"], NA)
            tab_sum[tab_sum[, col_ana] == cut, "year signif"] <- ifelse(length(tab_coefy["year", "signif"]) > 0, tab_coefy["year", "signif"], NA)
        }

    }

    ic <- tryCatch(as.data.frame(confint(obj_lm)), error = function(e) {
                                                                       })

    tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "IC_up", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
    if (length(grep(x, rownames(ic))) > 0) {
        ic[grepl(x, rownames(ic)), "97.5 %"]
    }else{
        NA
    }
}))
    tab_sum[tab_sum[, col_ana] == cut, grepl(paste(lev, "IC_inf", collapse = "|"), colnames(tab_sum))] <- unlist(lapply(lev, FUN = function(x) {
     if (length(grep(x, rownames(ic))) > 0) {
         ic[grepl(x, rownames(ic)), "2.5 %"]
     }else{
         NA
     }
}))

    return(tab_sum)

}


######################################### end of the function sorties_lm_f


######################################### start of the function note_glm_f called by glm_species and glm_community

note_glm_f <- function(data, obj_lm, metric, list_fact, details = FALSE) {
    ## Purpose: Note your GLM analysis
    ## ----------------------------------------------------------------------
    ## Arguments: data : d_ataframe used for analysis
    ##            obj_lm : GLM assessed
    ##            metric : selected metric
    ##            list_fact : Analysis factors list
    ##            details : detailed output ?
    ## ----------------------------------------------------------------------
    ## Author: Coline ROYAUX, 26 june 2020

    rate <- 0
    detres <- list(complete_plan = NA, balanced_plan = NA, NA_proportion_OK = NA, no_residual_dispersion = NA, uniform_residuals = NA, outliers_proportion_OK = NA, no_zero_inflation = NA, observation_factor_ratio_OK = NA, enough_levels_random_effect = NA, rate = NA)

    #### d_ata criterions ####

    ## Plan

    plan <- as.data.frame(table(data[, list_fact]))

    if (nrow(plan[plan$Freq == 0, ]) < nrow(plan) * 0.1) { # +0.5 if less than 10% of possible factor's level combinations aren't represented in the sampling scheme
        rate <- rate + 0.5
        detres$complete_plan <- TRUE

        if (summary(as.factor(plan$Freq))[1] > nrow(plan) * 0.9) {  # +0.5 if the frequency of the most represented frequency of possible factor's levels combinations is superior to 90% of the total number of possible factor's levels combinations
            rate <- rate + 0.5
            detres$balanced_plan <- TRUE
        }

    }else{
        detres$complete_plan <- FALSE
        detres$balanced_plan <- FALSE
    }

    if (nrow(data) - nrow(na.omit(data)) < nrow(data) * 0.1) { # +1 if less than 10% of the lines in the dataframe bares a NA
        rate <- rate + 1
        detres["NA_proportion_OK"] <- TRUE
    }else{
        detres["NA_proportion_OK"] <- FALSE
    }

    #### Model criterions ####

    if (length(grep("quasi", obj_lm$family)) == 0) { #DHARMa doesn't work with quasi distributions

        residuals <- DHARMa::simulateResiduals(obj_lm)

        capture.output(test_res <- DHARMa::testResiduals(residuals))
        test_zero <- DHARMa::testZeroInflation(residuals)

        ## dispersion of residuals

        if (test_res$dispersion$p.value > 0.05) { # +1.5 if dispersion tests not significative
            rate <- rate + 1.5
            detres$no_residual_dispersion <- TRUE
        }else{
            detres$no_residual_dispersion <- FALSE
        }

        ## uniformity of residuals

        if (test_res$uniformity$p.value > 0.05) { # +1 if uniformity tests not significative
            rate <- rate + 1
            detres$uniform_residuals <- TRUE
        }else{
            detres$uniform_residuals <- FALSE
        }

        ## residuals outliers

        if (test_res$outliers$p.value > 0.05) { # +0.5 if outliers tests not significative
            rate <- rate + 0.5
            detres["outliers_proportion_OK"] <- TRUE
        }else{
            detres["outliers_proportion_OK"] <- FALSE
        }

        ## Zero inflation test

        if (test_zero$p.value > 0.05) { # +1 if zero inflation tests not significative
            rate <- rate + 1
            detres$no_zero_inflation <- TRUE
        }else{
            detres$no_zero_inflation <- FALSE
        }

        ## Factors/observations ratio

        if (length(list_fact) / nrow(na.omit(data)) < 0.1) { # +1 if quantity of factors is less than 10% of the quantity of observations
            rate <- rate + 1
            detres["observation_factor_ratio_OK"] <- TRUE
        }else{
            detres["observation_factor_ratio_OK"] <- FALSE
        }

        ## less than 10 factors' level on random effect

        if (length(grep("^glmmTMB", obj_lm$call)) > 0) {
            nlev_rand <- c()
            for (fact in names(summary(obj_lm)$varcor$cond)) {
                 nlev_rand <- c(nlev_rand, length(unlist(unique(data[, fact]))))
            }

            if (all(nlev_rand > 10)) { # +1 if more than 10 levels in one random effect
                rate <- rate + 1
                detres$enough_levels_random_effect <- TRUE
            }else{
                detres$enough_levels_random_effect <- FALSE
            }
        }

        detres$rate <- rate

        if (details) {
            return(detres)
        }else{
            return(rate)
        }

    }else{
        return(NA)
        cat("Models with quasi distributions can't be rated for now")
    }
}

######################################### end of the function note_glm_f

######################################### start of the function note_glms_f called by glm_species and glm_community

note_glms_f <- function(tab_rate, expr_lm, obj_lm, file_out = FALSE) {
    ## Purpose: Note your GLM analysis
    ## ----------------------------------------------------------------------
    ## Arguments: tab_rate : rates table from note_glm_f
    ##            expr_lm : GLM expression assessed
    ##            obj_lm : GLM object
    ##            file_out : Output as file ? else global rate only
    ## ----------------------------------------------------------------------
    ## Author: Coline ROYAUX, 26 june 2020
    namefile <- "RatingGLM.txt"

    if (length(grep("quasi", obj_lm$family)) == 0) { #DHARMa doesn't work with quasi distributions

    rate_m <- median(na.omit(tab_rate[, "rate"]))
    sum <- summary(obj_lm)

    if (length(grep("^glmmTMB", obj_lm$call)) > 0) {
        if (median(na.omit(tab_rate[, "rate"])) >= 6) { # if 50% has a rate superior or equal to 6 +1
            rate_m <- rate_m + 1
        }

        if (quantile(na.omit(tab_rate[, "rate"]), probs = 0.9) >= 6) { # if 90% has a rate superior or equal to 6 +1
            rate_m <- rate_m + 1
        }
    }else{
        if (median(na.omit(tab_rate[, "rate"])) >= 5) { # if 50% has a rate superior or equal to 5 +1
            rate_m <- rate_m + 1
        }

        if (quantile(na.omit(tab_rate[, "rate"]), probs = 0.9) >= 5) { # if 90% has a rate superior or equal to 5 +1
            rate_m <- rate_m + 1
        }
    }

    if (file_out) {

        cat("###########################################################################",
            "\n########################### Analysis evaluation ###########################",
            "\n###########################################################################", file = namefile, fill = 1, append = TRUE)

        ## Informations on model :
        cat("\n\n######################################### \nFitted model:", file = namefile, fill = 1, append = TRUE)
        cat("\t", deparse(expr_lm), "\n\n", file = namefile, sep = "", append = TRUE)
        cat("Family: ", sum$family[[1]],
            file = namefile, append = TRUE)
        cat("\n\nNumber of analysis: ", nrow(tab_rate), file = namefile, append = TRUE)

        ## Global rate :
        cat("\n\n######################################### \nGlobal rate for all analysis:",
            "\n\n", rate_m, "out of 10", file = namefile, append = TRUE)

        ## details on every GLM :

        cat("\n\n######################################### \nDetails on every analysis:\n\n", file = namefile, append = TRUE)
        cat("Analysis\tC1\tC2\tC3\tC4\tC5\tC6\tC7\tC8\tC9\tFinal rate", file = namefile, append = TRUE)
        apply(tab_rate, 1, FUN = function(x) {

                                  if (!is.na(x["complete_plan"]) && x["complete_plan"] == TRUE) {
                                      cat("\n", x[1], "\tyes", file = namefile, append = TRUE)
                                  }else{
                                      cat("\n", x[1], "\tno", file = namefile, append = TRUE)
                                  }

                                  for (i in c("balanced_plan", "NA_proportion_OK", "no_residual_dispersion", "uniform_residuals", "outliers_proportion_OK", "no_zero_inflation", "observation_factor_ratio_OK", "enough_levels_random_effect")) {
                                      if (!is.na(x[i]) && x[i] == TRUE) {
                                          cat("\tyes", file = namefile, append = TRUE)
                                      }else{
                                          cat("\tno", file = namefile, append = TRUE)
                                      }
                                  }

                                  cat("\t", x["rate"], "/ 8", file = namefile, append = TRUE)


                              })
        cat("\n\nC1: Complete plan?\nC2: Balanced plan?\nC3: Few NA?\nC4: Regular dispersion?\nC5: Uniform residuals?\nC6: Regular outliers proportion?\nC7: No zero-inflation?\nC8: Good observation/factor ratio?\nC9: Enough levels on random effect?", file = namefile, append = TRUE)

        ## Red flags - advice :
        cat("\n\n######################################### \nRed flags - advice:\n\n", file = namefile, append = TRUE)
        if (all(na.omit(tab_rate["NA_proportion_OK"]) == FALSE)) {
            cat("\n", "\t- More than 10% of lines of your dataset contains NAs", file = namefile, append = TRUE)
        }

        if (length(grep("FALSE", tab_rate["no_residual_dispersion"])) / length(na.omit(tab_rate["no_residual_dispersion"])) > 0.5) {
            cat("\n", "\t- More than 50% of your analyses are over- or under- dispersed : Try with another distribution family", file = namefile, append = TRUE)
        }

        if (length(grep("FALSE", tab_rate["uniform_residuals"])) / length(na.omit(tab_rate["uniform_residuals"])) > 0.5) {
            cat("\n", "\t- More than 50% of your analyses haven't an uniform distribution of residuals : Try with another distribution family", file = namefile, append = TRUE)
        }

        if (length(grep("FALSE", tab_rate["outliers_proportion_OK"])) / length(na.omit(tab_rate["outliers_proportion_OK"])) > 0.5) {
            cat("\n", "\t- More than 50% of your analyses have too much outliers : Try with another distribution family or try to select or filter your data", file = namefile, append = TRUE)
        }

        if (length(grep("FALSE", tab_rate["no_zero_inflation"])) / length(na.omit(tab_rate["no_zero_inflation"])) > 0.5) {
            cat("\n", "\t- More than 50% of your analyses have zero inflation : Try to select or filter your data", file = namefile, append = TRUE)
        }

        if (length(grep("FALSE", tab_rate["observation_factor_ratio_OK"])) / length(na.omit(tab_rate["observation_factor_ratio_OK"])) > 0.5) {
            cat("\n", "\t- More than 50% of your analyses have not enough observations for the amount of factors : Try to use less factors in your analysis or try to use another separation factor", file = namefile, append = TRUE)
        }

        if (any(tab_rate["enough_levels_random_effect"] == FALSE, na.rm = TRUE) && length(grep("^glmmTMB", obj_lm$call)) > 0) {
            cat("\n", "\t- Random effect hasn't enough levels to be robust : If it has less than ten levels remove the random effect", file = namefile, append = TRUE)
        }
    }else{

        return(rate_m)

    }
    }else{
        cat("Models with quasi distributions can't be rated for now", file = namefile, append = TRUE)
    }
}

######################################### end of the function note_glm_f

######################################### start of the function info_stats_f called by glm_species and glm_community

info_stats_f <- function(filename, d_ata, agreg_level = c("species", "unitobs"), type = c("graph", "stat"),
                        metrique, fact_graph, fact_graph_sel, list_fact, list_fact_sel) {
    ## Purpose: informations and simple statistics
    ## ----------------------------------------------------------------------
    ## Arguments: filename : name of file
    ##            d_ata : input data
    ##            agreg_level : aggregation level
    ##            type : type of function calling
    ##            metrique : selected metric
    ##            fact_graph : selection factor
    ##            fact_graph_sel : list of factors levels selected for this factor
    ##            list_fact : list of grouping factors
    ##            list_fact_sel : list of factors levels selected for these factors
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 sept. 2012, 15:26 modified by Coline ROYAUX 04 june 2020

    ## Open file :
    f_ile <- file(description = filename,
                 open = "w", encoding = "UTF-8")

    ## if error  :
    on.exit(if (exists("filename") &&
                tryCatch(isOpen(f_ile),
                         error = function(e)return(FALSE))) close(f_ile))

    ## Metrics and factors infos :
    print_selection_info_f(metrique = metrique, #fact_graph = fact_graph, fact_graph_sel = fact_graph_sel,
                         list_fact = list_fact, #list_fact_sel = list_fact_sel,
                         f_ile = f_ile,
                         agreg_level = agreg_level, type = type)

    ## statistics :
    if (class(d_ata) == "list") {
        cat("\n###################################################",
            "\nStatistics per level of splitting factor:\n",
            sep = "", file = f_ile, append = TRUE)

        invisible(sapply(seq_len(length(d_ata)),
                         function(i) {
                         print_stats_f(d_ata = d_ata[[i]], metrique = metrique, list_fact = list_fact, f_ile = f_ile,
                                      headline = fact_graph_sel[i])
                     }))
    }else{
        print_stats_f(d_ata = d_ata, metrique = metrique, list_fact = list_fact, f_ile = f_ile,
                     headline = NULL)
    }

    ## Close file :
    close(f_ile)

}

######################################### end of the function info_stats_f


######################################### start of the function print_selection_info_f called by info_stats_f

print_selection_info_f <- function(metrique, list_fact,
                                 f_ile,
                                 agreg_level = c("species", "unitobs"), type = c("graph", "stat")) {
    ## Purpose: Write data informations
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : chosen metric
    ##            list_fact : factor's list
    ##            f_ile : Results file name
    ##            agreg_level : aggregation level
    ##            type : function type
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 10:41 modified by Coline ROYAUX 04 june 2020

    cat("\n##################################################\n",
        "Metrics and factors (and possible units/selections):\n",
        sep = "", file = f_ile, append = TRUE)

    ## metric info :
    cat("\n Metrics:", metrique,
        "\n", file = f_ile, append = TRUE)

    ## Clustering factors :
    if (is.element(agreg_level, c("spCL_unitobs", "spCL_espece", "spSpecies", "spEspece",
                                 "spUnitobs", "spUnitobs(CL)"))) {
                                                                     type <- "spatialGraph"
                                                                 }

    cat(switch(type,
               "graph" = "\nGrouping factor(s): \n * ",
               "stat" = "\nAnalyses factor(s): \n * ",
               "spatialGraph" = "\nSpatial aggregation factor(s): \n * "),
        paste(list_fact, collaspe = "\n * "), "\n", file = f_ile, append = TRUE)

}

######################################### end of the function print_selection_info_f


######################################### start of the function print_stats_f called by info_stats_f

print_stats_f <- function(d_ata, metrique, list_fact, f_ile, headline = NULL) {
    ## Purpose: Write general statistics table
    ## ----------------------------------------------------------------------
    ## Arguments: d_ata : Analysis data
    ##            metrique : metric's name
    ##            list_fact : Factor's list
    ##            f_ile : Simple statistics file name
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 10:09 modified by Coline ROYAUX 04 june 2020

    ## Header :
    if (! is.null(headline)) {
        cat("\n", rep("#", nchar(headline) + 3), "\n",
            "## ", headline, "\n",
            sep = "", file = f_ile, append = TRUE)
    }

    cat("\n########################\nBase statistics:\n\n", file = f_ile, append = TRUE)

    capture.output(print(summary_fr(d_ata[, metrique])), file = f_ile, append = TRUE)

    if (! is.null(list_fact)) {
        cat("\n#########################################",
            "\nStatistics per combination of factor levels:\n\n", file = f_ile, sep = "", append = TRUE)

        ## Compute summary for each existing factor's cross :
        res <- with(d_ata,
                    tapply(eval(parse(text = metrique)),
                           INDEX = do.call(paste,
                                         c(lapply(list_fact,
                                                  function(y)eval(parse(text = y))),
                                           sep = ".")),
                           FUN = summary_fr))

        ## results in table
        capture.output(print(do.call(rbind, res)),
                       file = f_ile, append = TRUE)
    }

    ## empty line :
    cat("\n", file = f_ile, append = TRUE)
}

######################################### end of the function print_stats_f


######################################### start of the function summary_fr called by print_stats_f
summary_fr <- function(object, digits = max(3, getOption("digits") - 3), ...) {
    ## Purpose: Adding SD and N to summary
    ## ----------------------------------------------------------------------
    ## Arguments: object : Object to summarise
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 sept. 2012, 15:47 modified by Coline ROYAUX 04 june 2020

    if (! is.numeric(object)) stop("Programming error")

    ## Compute summary :
    res <- c(summary(object = object, digits, ...), "sd" = signif(sd(x = object), digits = digits), "N" = length(object))

    return(res)
}

######################################### start of the function summary_fr

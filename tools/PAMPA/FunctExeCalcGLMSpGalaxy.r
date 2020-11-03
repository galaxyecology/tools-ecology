#Rscript

#####################################################################################################################
#####################################################################################################################
################################# Compute a Generalized Linear Model from your data #################################
#####################################################################################################################
#####################################################################################################################

###################### Packages
suppressMessages(library(multcomp))
suppressMessages(library(DHARMa))
suppressMessages(library(glmmTMB)) ###Version: 0.2.3
suppressMessages(library(gap))


###################### Load arguments and declaring variables

args = commandArgs(trailingOnly = TRUE)

if (length(args) < 10) {
    stop("At least 4 arguments must be supplied : \n- two input dataset files (.tabular) : metrics table and unitobs table \n- Interest variable field from metrics table \n- Response variable from unitobs table.", call. = FALSE) # if no args -> error and exit1

} else {
    import_data <- args[1] ###### file name : metrics table
    import_unitobs <- args[2] ###### file name : unitobs informations
    colmetric <- as.numeric(args[3]) ###### Selected interest metric for GLM
    list_fact <- strsplit(args [4], ",")[[1]] ###### Selected response factors for GLM
    list_rand <- strsplit(args [5], ",")[[1]] ###### Selected randomized response factors for GLM
    col_fact_ana <- args[6] ####### (optional) Selected splitting factors for GLMs
    distrib <- args[7] ###### (optional) Selected distribution for GLM
    glm_out <- args[8] ###### (Optional) GLM object as Rdata output ?
    aggreg <- args[9] ###### Aggregation level of the data table
    source(args[10]) ###### Import functions

}
#### d_ata must be a dataframe with at least 3 variables : unitobs representing location and year ("observation.unit"), species code ("species.code") and abundance ("number")


#Import des données / Import data
obs<- read.table(import_data, sep = "\t", dec = ".", header = TRUE, encoding = "UTF-8") #
obs[obs == -999] <- NA
metric <- colnames(obs)[colmetric]
tab_unitobs <- read.table(import_unitobs, sep = "\t", dec = ".", header = TRUE, encoding = "UTF-8")
tab_unitobs[tab_unitobs == -999] <- NA

vars_data1<- c("species.code")
err_msg_data1 <- "The input metrics dataset doesn't have the right format. It needs to have at least the following 3 variables :\n- species.code \n- observation.unit (or year and site)\n- numeric or integer metric\n"
check_file(obs, err_msg_data1, vars_data1, 3)

vars_data2 <- c("observation.unit", list_fact, list_rand)
err_msg_data2 <- "The input unitobs dataset doesn't have the right format. It needs to have at least the following 2 variables :\n- observation.unit (or year and site)\n- factors used in GLM (habitat, year and/or site)\n"
check_file(tab_unitobs, err_msg_data2, vars_data2[vars_data2 != "None"], 2)


if (col_fact_ana != "None") {
    fact_ana <- col_fact_ana
    if (class(obs[fact_ana]) == "numeric" || fact_ana == "observation.unit"){stop("Wrong chosen separation factor : Analysis can't be separated by observation unit or numeric factor")
    }
}else{
    fact_ana <- col_fact_ana
}

if (all(c(list_fact, list_rand) == "None")) {stop("GLM needs to have at least one response variable.")
}

if (list_fact[1] == "None" || all(is.element(list_fact, list_rand))) {stop("GLM can't have only random effects.")
}

####################################################################################################
########## Computing Generalized Linear Model ## Function : linear_model_wp2_community_f ############
####################################################################################################

linear_model_wp2_species_f <- function(metrique, list_fact, list_rand, fact_ana, distrib, tab_metrics, tab_metrique, tab_unitobs, unitobs = "observation.unit", nb_name = "number") {
    ## Purpose: Monitoring steps for GLM on species data
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : selected metric
    ##            list_fact : Factors for GLM
    ##            list_rand : Random factors for GLM
    ##            fact_ana : Separation factor for GLMs
    ##            distrib : selected distribution for model
    ##            tab_metrics : data table metrics
    ##            tab_metrique : data table's name
    ##            tab_unitobs : data table unitobs
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 août 2010, 15:59 modified by Coline ROYAUX 04 june 2020

    tmpd_ata <- tab_metrics

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
    ##Creating model's expression :
    expr_lm <- eval(parse(text = paste(metrique, "~", resp_fact)))

    ##Creating analysis table :
    list_fact_tab <- c(list_fact, fact_ana)
    list_fact_tab <- list_fact_tab[list_fact_tab != "None"]

    if (all(is.na(match(tmpd_ata[, unitobs], tab_unitobs[, unitobs])))) {stop("Observation units doesn't match in the two input tables")}

    if(is.element("species.code", colnames(tmpd_ata))) {
        col <- c(unitobs, metrique, fact_ana)
        tmpd_ata <- cbind(tmpd_ata[, col], tab_unitobs[match(tmpd_ata[, unitobs], tab_unitobs[, unitobs]), list_fact])
        colnames(tmpd_ata) <- c(col, list_fact)

        for (i in list_fact_tab) {
            tmpd_ata[, i] <- as.factor(tmpd_ata[, i])
         }
    }else{
        stop("Warning : wrong data frame, data frame should be aggregated by observation unit (year and site) and species")
    }

    ## Suppression des 'levels' non utilisés :
    tmpd_ata <- drop_levels_f(tmpd_ata)

    ## Aide au choix du type d'analyse :
    if (distrib == "None") {
        if (metrique == "presence_absence") {
            chose_distrib <- "binomial"
        }else{
            switch(class(tmpd_ata[, metrique]),
                  "integer" = {chose_distrib <- "poisson"},
                  "numeric" = {chose_distrib <- "gaussian"},
                  stop("Selected metric class doesn't fit, you should select an integer or a numeric variable"))
        }
    }else{
        chose_distrib <- distrib
    }

    ##Create results table :
    lev <- unlist(lapply(list_f, FUN = function(x){levels(tmpd_ata[, x])}))
    row <- levels(tmpd_ata[, fact_ana])

    if (is.element("year", list_f) && ! is.element("year", list_rand)) {
        tab_sum <- create_res_table(list_rand = list_rand, list_fact = list_fact, row = row, lev = unlist(c("year", lev)), distrib = chose_distrib)
    }else{
        tab_sum <- create_res_table(list_rand = list_rand, list_fact = list_fact, row = row, lev = lev, distrib = chose_distrib)
    }
    ### creating rate table
    tab_rate <- data.frame(species = row, complete_plan = NA, balanced_plan = NA, NA_proportion_OK = NA, no_residual_dispersion = NA, uniform_residuals = NA, outliers_proportion_OK = NA, no_zero_inflation = NA, observation_factor_ratio_OK = NA, enough_levels_random_effect = NA, rate = NA)

    ## Compute Model(s) :

    for (sp in levels(tmpd_ata[, fact_ana])) {
        cutd_ata <- tmpd_ata[grep(sp, tmpd_ata[, fact_ana]), ]
        cutd_ata <- drop_levels_f(cutd_ata)

        res <- ""
        resy <- ""

        if (list_rand[1] != "None") {
            res <- tryCatch(glmmTMB(expr_lm, family = chose_distrib, data = cutd_ata), error = function(e){})

            if (is.element("year", list_f) && ! is.element("year", list_rand)) { #Model with year as continuous
                cutd_ata$year <- as.numeric(cutd_ata$year)
                resy <- tryCatch(glmmTMB(expr_lm, family = chose_distrib, data = cutd_ata), error = function(e){})
                cutd_ata$year <- as.factor(cutd_ata$year)
            }else{
                resy <- ""
            }
        }else{
            res <- tryCatch(glm(expr_lm, data = cutd_ata, family = chose_distrib), error = function(e){})
            if (is.element("year", list_f)) { #Model with year as continuous
                cutd_ata$year <- as.numeric(cutd_ata$year)
                resy <- tryCatch(glm(expr_lm, family = chose_distrib, data = cutd_ata), error = function(e){})
                cutd_ata$year <- as.factor(cutd_ata$year)
            }else{
                resy <- ""
            }
        }

          ## Write results :
        if (! is.null(res)) {
            file_save_glm_sp <- paste("GLM_", sp, ".Rdata", sep = "")
            save(res, file = file_save_glm_sp)

            tab_sum <- sorties_lm_f(obj_lm = res, obj_lmy = resy, tab_sum = tab_sum, fact_ana = fact_ana, cut = sp, col_ana = "analysis", lev = lev, d_ata = cutd_ata, metrique = metrique, list_fact = list_fact)

            tab_rate[tab_rate[, "species"] == sp, c(2:11)] <- note_glm_f(data = cutd_ata, obj_lm = res, metric = metrique, list_fact = list_fact, details = TRUE)

        }else{
            cat("\nCannot compute GLM for species", sp, "Check if one or more factor(s) have only one level, or try with another distribution for the model in advanced settings \n\n")
        }

    }
    note_glms_f(tab_rate = tab_rate, expr_lm = expr_lm, obj_lm = res, file_out = TRUE)

    ## simple statistics and infos :
    filename <- "GLMSummaryFull.txt"

    ## Save data on model :

    info_stats_f(filename = filename, d_ata = tmpd_ata, agreg_level = aggreg, type = "stat",
                metrique = metrique, fact_graph = fact_ana, #fact_graph_sel = modSel,
                list_fact = list_fact)#, list_fact_sel = list_fact_sel)

    return(tab_sum)
}

################# Analysis

Tab <- linear_model_wp2_species_f(metrique = metric, list_fact = list_fact, list_rand = list_rand, fact_ana = fact_ana, distrib = distrib, tab_metrics = obs, tab_metrique = aggreg, tab_unitobs = tab_unitobs, nb_name = "number")

write.table(Tab, "GLMSummary.tabular", row.names = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")

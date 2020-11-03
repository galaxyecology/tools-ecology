#Rscript

#####################################################################################################################
#####################################################################################################################
############################ Calculate presence absence table from observation data #################################
#####################################################################################################################
#####################################################################################################################

###################### Packages
suppressMessages(library(tidyr))

###################### Load arguments and declaring variables

args <- commandArgs(trailingOnly = TRUE)


if (length(args) < 2) {
    stop("At least one argument must be supplied, an input dataset file (.tabular).", call. = FALSE) # if no args -> error and exit1

} else {
    import_data <- args[1] ###### Nom du fichier importé avec son extension / file name imported with the file type ".filetype"
    source(args[2]) ###### Import functions

}
#### d_ata must be a dataframe with at least 3 variables : unitobs representing location and year ("observation.unit"), species code ("species.code") and abundance ("number")


#Import des données / Import data
obs<- read.table(import_data, sep = "\t", dec = ".", header = TRUE, encoding = "UTF-8") #
obs[obs == -999] <- NA
factors <- fact_det_f(Obs = obs)
obs_type <- def_typeobs_f(Obs = obs)
obs <- create_unitobs(data = obs)

vars_data <- c("observation.unit", "species.code", "number")
err_msg_data <- "The input dataset doesn't have the right format. It need to have at least the following 3 variables :\n- observation.unit (or location and year)\n- species.code\n- number\n"
check_file(obs, err_msg_data, vars_data, 3)


####################################################################################################
#################### Create presence/absence table ## Function : calc_pres_abs_f ####################
####################################################################################################

calc_pres_abs_f <- function(d_ata,
                           nb_name = "number") {
    ## Purpose: Compute presence absence
    ## ----------------------------------------------------------------------
    ## Arguments: d_ata : temporary metrics table
    ##            nb_name : name of abundance column
    ##
    ## Output: presence absence vector
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 12:04 modified by Coline ROYAUX 04 june 2020

    ## Presence - absence :
    pres_abs <- integer(nrow(d_ata))
    pres_abs[d_ata[, nb_name] > 0] <- as.integer(1)
    pres_abs[d_ata[, nb_name] == 0] <- as.integer(0)

    return(pres_abs)
}


################# Analysis

res <- calc_numbers_f(obs, obs_type = obs_type, factors = factors, nb_name = "number")
res$presence_absence <- calc_pres_abs_f(res, nb_name = "number")
res <- create_year_location(res)

#Save dataframe in a tabular format
filename_pres_abs <- "TabPresAbs.tabular"
write.table(res, filename_pres_abs, row.names = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")
cat(paste("\nWrite table with presence/absence. \n--> \"", filename_pres_abs, "\"\n", sep = ""))

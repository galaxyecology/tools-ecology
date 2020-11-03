#Rscript

#####################################################################################################################
#####################################################################################################################
################################# Calculate community indexes from observation data #################################
#####################################################################################################################
#####################################################################################################################

###################### Packages R

suppressMessages(library(tidyr))

###################### Load arguments and declaring variables

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 4) {
    stop("At least one argument must be supplied, an input dataset file (.tabular).", call. = FALSE) # if no args -> error and exit1

} else {
    import_data<-args[1] ###### Nom du fichier importé avec son extension / file name imported with the file type ".filetype"
    index <- args[2] ###### List of selected metrics to calculate
    source(args[3]) ###### Import functions

}
#### d_ata must be a dataframe with at least 3 variables : unitobs representing location and year ("observation.unit"), species code ("species.code") and abundance ("number")


#Import des données / Import data 
obs <- read.table(import_data, sep = "\t", dec = ".", header = TRUE, encoding = "UTF-8") #
obs[obs == -999] <- NA 
factors <- fact_det_f(Obs = obs)
obs_type <- def_typeobs_f(Obs = obs)
obs <- create_unitobs(data = obs)

vars_data<-c("observation.unit", "species.code", "number")
err_msg_data<-"The input dataset doesn't have the right format. It need to have at least the following 3 variables :\n- observation.unit (or location and year)\n- species.code\n- number\n"
check_file(obs, err_msg_data, vars_data, 3)



####################################################################################################
################# create community metrics table ## Function : calc_biodiv_f #######################
####################################################################################################

########################################################################################################################
calc_biodiv_f <- function(d_ata, unitobs = "observation.unit", code.especes = "species.code", nombres = "number",
                         indices = index)
{
    ## Purpose: compute biodiversity indexes
    ## ----------------------------------------------------------------------
    ## Arguments: d_ata : input observation file
    ##            unitobs : name of column observation unit
    ##            code.especes : name of species column
    ##            nombres : name of abundance column
    ##            indices : list of indexes to compute
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2010, 08:58 modified by Coline ROYAUX in june 2020

    ## Supress lines that doesn't represent a species :

    notspline <- grep("(sp\\.)$|([1-9])$|^(Absencemacrofaune)$|^(NoID)$|^(Acrobranc)$|^(Acrodigit)$|^(Acroencr)$|^(Acrosubm)$|^(Acrotabu)$|^(Adredure)$|^(Adremoll)$|^(Algaturf)$|^(Balimona)$|^(Corablan)$|^(CoradurV)$|^(Coraenal)$|^(Coramor1)$|^(Coramor2)$|^(Coramou)$|^( Dallcora)$|^(Debrcora)$|^(Debris)$|^(Hare)$|^(HexaChar)$|^(MuraCong)$|^(Nacrbran)$|^(Nacrcham)$|^(Nacrencr)$|^(Nacrfoli)$|^(Nacrmass)$|^(Nacrsubm)$|^(Recrcora)$|^(Roche)$|^(Sable)$|^(Vase)$", d_ata[, code.especes], value = FALSE)
    if (length(notspline) != 0)
    {
        d_ata <- d_ata[-notspline, ]
    }else{}

    ## Suppress unused factor levels :
    d_ata <- drop_levels_f(df = d_ata)


    ## aggregation of data if not already done :
    if (nrow(d_ata) > nrow(expand.grid(unique(d_ata[ , unitobs]), unique(d_ata[ , code.especes]))))
    {
        d_ata <- agregations_generic_f(d_ata = d_ata, metrics = nombres,
                                      factors = c(unitobs, code.especes),
                                      list_fact = NULL)
    }else{}

    df_biodiv <- as.data.frame(as.table(tapply(d_ata[ , nombres],
                                               d_ata[ , unitobs],
                                               sum, na.rm = TRUE)))

    colnames(df_biodiv) <- c(unitobs, nombres)

## ##################################################
    ## species richness :
    d_ata$pres.abs <- pres_abs_f(nombres = d_ata[ , nombres], logical = FALSE)

    df_biodiv$species.richness <- as.vector(tapply(d_ata$pres.abs,
                                                   d_ata[ , unitobs], sum, na.rm = TRUE),
                                            "integer")
    ## ... as.vector to avoid the class "array".

 ## ##################################################
    ## Simpson, Shannon indexes and derivatives :

    mat_nb <- tapply(d_ata[ , nombres], # Matrix of individual count /species/unitobs.
                         list(d_ata[ , unitobs], d_ata[ , code.especes]),
                         sum, na.rm = TRUE)

    mat_nb[is.na(mat_nb)] <- 0  # Vrais zéros

    ## each species individual proportion in the dataset :
    prop_indiv <- sweep(mat_nb, 1,
                       apply(mat_nb, 1, sum, na.rm = TRUE), # individual count / unitobs ; equiv df_biodiv$nombre.
                       FUN = "/")

    ## Simpson indexes :
    df_biodiv$simpson <- apply(prop_indiv^2, 1, sum, na.rm = TRUE)

    if (any(is.element(c("all", "simpson.l"), indices)))
    {
        df_biodiv$simpson.l <- 1 - df_biodiv$simpson
    }

    ## Shannon index :
    df_biodiv$shannon <- -1 * apply(prop_indiv * log(prop_indiv), 1, sum, na.rm = TRUE)

    ## Pielou index :
    if (any(is.element(c("all", "pielou"), indices)))
    {
        df_biodiv$pielou <- df_biodiv$shannon / log(df_biodiv$species.richness)
    }

    ## Hill index :
    if (any(is.element(c("all", "hill"), indices)))
    {
        df_biodiv$hill <- (1 - df_biodiv$simpson) / exp(df_biodiv$shannon)
                                        # equiv df_biodiv$l.simpson / exp(df_biodiv$shannon)
    }


    return(df_biodiv)
}

################# Analysis

res <- calc_numbers_f(obs, obs_type = obs_type, factors = factors, nb_name = "number")

table_comm_indexes <- calc_biodiv_f(res, unitobs = "observation.unit", code.especes = "species.code", nombres = "number",
                         indices = index)
table_comm_indexes <- create_year_location(table_comm_indexes)
#Save dataframe in a tabular format

filename_comm <- "TabCommunityIndexes.tabular"
write.table(table_comm_indexes, filename_comm, row.names = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")
cat(paste("\nWrite table with Community indexes. \n--> \"", filename_comm, "\"\n", sep = ""))


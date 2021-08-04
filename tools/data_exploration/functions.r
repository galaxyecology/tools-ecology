#Rscript

#########################################################################################
####################### Exploration data tools function #################################
#########################################################################################
#### Based on Romain Lorrillière R script
#### Modified by Alan Amosse, Benjamin Yguel and Marie Jossé for integrating within Galaxy-E

######################################### start of the function makeTableAnalyse
##Species are placed in separated columns and addition of zero on plots where at least one selected species is present
make_table_analyse <- function(data, var, spe, var2, var3) {
    tab <- reshape(data
                  , v.names = var
                  , idvar = c(var2, var3)
                  , timevar = spe
                  , direction = "wide")
    tab[is.na(tab)] <- 0 ###### remplace les na par des 0 / replace NAs by 0 

    colnames(tab) <- sub(paste0(var, "."), "", colnames(tab))### remplace le premier pattern "abond." par le second "" / replace the column names "abond." by ""
    return(tab)
}
######################################### end of the function makeTableAnalyse

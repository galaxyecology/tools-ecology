#!/usr/bin/env Rscript

#####################################################################################################################
############## FILTERING RARE AND LOW-ABUNDANCE SPECIES   function:filtreEspeceRare    ##############################
#####################################################################################################################

#### Based on Romain Lorrillière R script
#### Modified by Alan Amosse and Benjamin Yguel for integrating within Galaxy-E


suppressMessages(library(reshape2))


###########
#delcaration des arguments et variables/ declaring some variables and load arguments

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
    stop("At least one argument must be supplied, dataset transformed by make table function (.tabular).", call.=FALSE) #si pas d'arguments -> affiche erreur et quitte / if no args -> error and exit1
} else {
    Datatransformedforfiltering_trendanalysis<-args[1] ###### Nom du fichier peut provenir de la fonction "MakeTableAnalyse" / file name , may result from the function "MakeTableAnalys"    
    source(args[2])### chargement des fonctions / load the functions
}

##### Le tableau de données doit posséder 3 variables en colonne minimum avec 1 seule espèce et autant de colonne en plus que d'espèces en plus: les carrés ou sont réalisés les observatiosn ("carre"), la ou les années des observations ("annee"), 1 colonne par espèce renseignée avec les abondances correspondantes
##### Data must be a dataframe with 3 variables in column: plots where observation where made ("carre"), year(s) of the different sampling ("annee"), and one column per species with its abundance


#Import des données / Import data 
tab <- read.table(Datatransformedforfiltering_trendanalysis,sep="\t",dec=".",header=TRUE) #  

err_msg_tab="\nThe input dataset doesn't have the right format. It need to have the following 2 variables : \"carre\" and \"annee\" followed by at least one species"
if(ncol(tab)<3 || !("carre" %in% names(tab)) || !("annee" %in% names(tab))){
    stop(err_msg_tab,call.=FALSE)
}




#Do your analysis
tab_filtred1<-filter_absent_species(tab)
tab_filtred2<-filter_rare_species(tab) 

#save the data in a output file in a tabular format
filename <- "Datafilteredfortrendanalysis.tabular"
write.table(tab_filtred2, filename,row.names=FALSE,sep="\t",dec=".")
cat(paste("\nWrite table with data filtered for trend analysis. \n--> \"",filename,"\"\n")) 



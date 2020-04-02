#!/usr/bin/env Rscript

######################################################################################################################################
############## COMMAND LINE TO CALCULATE AND PLOT EVOLUTION OF SPECIES POPULATION  function:main.glm    ##############################
######################################################################################################################################

#### Based on Romain Lorrillière R script
#### Modified by Alan Amosse and Benjamin Yguel for integrating within Galaxy-E

#suppressMessages(library(lme4))
suppressMessages(library(ggplot2))
suppressMessages(library(speedglm))
suppressMessages(library(arm))
#suppressMessages(library(reshape))
suppressMessages(library(data.table))
suppressMessages(library(reshape2))


###########
#delcaration des arguments et variables/ declaring some variables and load arguments

args = commandArgs(trailingOnly=TRUE)
options(encoding = "UTF-8")
source(args[6],encoding="UTF-8")### chargement des fonctions / load the functions

if ( (length(args)<8) || (length(args)>9)) {
    stop("At least 5 arguments must be supplied :\n- An input dataset filtered (.tabular). May come from the filter rare species tool.\n- A species detail table (.tabular)\n- An id to fix output repository name.\n- A list of species to exclude, can be empty.\n- TRUE/FALSE to perform the glm with confidence intervals calculations.\n\n", call.=FALSE) #si pas d'arguments -> affiche erreur et quitte / if no args -> error and exit1
} else {
    Datafilteredfortrendanalysis<-args[1] ###### Nom du fichier avec extension ".typedefichier", peut provenir de la fonction "FiltreEspeceRare" / file name without the file type ".filetype", may result from the function "FiltreEspeceRare"    
    tabSpecies<-args[2] ###### Nom du fichier avec extension ".typedefichier", fichier mis à disposition dans Galaxy-E avec specialisation à l'habitat des especes et si espece considérée comme indicatrice / file name without the file type ".filetype", file available in Galaxy-E containing habitat specialization for each species and whether or not they are considered as indicator  
    id<-args[3]  ##### nom du dossier de sortie des resultats / name of the output folder
    spExclude <- strsplit(args [4],",")[[1]] ##### liste d'espece qu on veut exclure de l analyse  / list of species that will be excluded
    AssessIC <-args [5] ##########  TRUE ou FALSE réalise glm "standard" avec calcul d'intervalle de confiance ou speedglm sans IC bien plus rapide / TRUE or FALSE perform a "standard" glm with confidance interval or speedglm without CI much more fast
}

## creation d'un dossier pour y mettre les resultats / create folder for the output of the analyses

dir.create(paste("Output/",id,sep=""),recursive=TRUE,showWarnings=FALSE)
#cat(paste("Create Output/",id,"\n",sep=""))
dir.create(paste("Output/",id,"/Incertain/",sep=""),recursive=TRUE,showWarnings=FALSE)
#cat(paste("Create Output/",id,"Incertain/\n",sep=""))


#Import des données / Import data 
tabCLEAN <- fread(Datafilteredfortrendanalysis,sep="\t",dec=".",header=TRUE,encoding="UTF-8") #### charge le fichier de données d abondance / load abundance of species
tabsp <- fread(tabSpecies,sep="\t",dec=".",header=TRUE,encoding="UTF-8")   #### charge le fichier de donnees sur nom latin, vernaculaire et abbreviation, espece indicatrice ou non / load the file with information on species specialization and if species are indicators

vars_tabCLEAN<-c("carre","annee","espece","abond")
err_msg_tabCLEAN<-"The input dataset filtered doesn't have the right format. It need to have the following 4 variables :\n- carre\n- annee\n- espece\n- abond\n"

vars_tabsp<-c("espece","nom","nomscientific","indicateur","specialisation")
err_msg_tabsp<-"\nThe species dataset filtered doesn't have the right format. It need to have the following 4 variables :\n- espece\n- nom\n- nomscientific\n- indicateur\n- specialisation\n"

check_file(tabCLEAN,err_msg_tabCLEAN,vars_tabCLEAN,4)
check_file(tabsp,err_msg_tabsp,vars_tabsp,5)



firstYear <- min(tabCLEAN$annee) #### Recupère 1ere annee des donnees / retrieve the first year of the dataset
lastYear <- max(tabCLEAN$annee)  #### Récupère la dernière annee des donnees / retrieve the last year of the dataset
annees <- firstYear:lastYear  ##### !!!! une autre variable s'appelle annee donc peut être à modif en "periode" ? ### argument de la fonction mais  DECLARER DANS LA FONCTION AUSSI donc un des 2 à supprimer
spsFiltre=unique(tabCLEAN$espece) #### Recupère la liste des especes du tabCLEAN qui ont été sélectionnée et qui ont passé le filtre / retrieve species name that were selected and then filtered before
#cat("\n\nspsFiltre\n")
tabsp=subset (tabsp, (espece %in% spsFiltre)) #### liste des espèces exclu par le filtre ou manuellement / List of species excluded manually or by the filter from the analyses 
#cat("\n\ntabsp\n")
sp=as.character(tabsp$espece)  ##### liste des espece en code ou abbreviation gardées pour les analyses ### arg de la fonction  DECLARE AUSSI APRES DS FONCTION  / list of the code or abbreviation of the species kept for the analyses
#cat("\n\nsp\n")
if(length(spExclude)!=0) {
    tabCLEAN <- subset(tabCLEAN,!(espece %in% spExclude))
    tabsp <- subset(tabsp, !(espece %in% spExclude))

    cat("\n\nEspèces exclues de l'analyse :\n")
    cat(spExclude)
    cat("\n")
}
if(length(tabCLEAN$espece)==0){
    stop("There is no species left for the analyse.", call.=FALSE) #si pas plus d'espèce après filtre / if no more species after filter
}
#cat("\n\ntabsp\n")


################## 
###  Do your analysis

main.glm(donneesAll=tabCLEAN,tabsp=tabsp,id=id,assessIC=AssessIC)






#!/usr/bin/env Rscript

##################################################################################################################################################
############## CALCULATE AND PLOT EVOLUTION OF SPECIES POPULATION BY SPECIALIZATION GROUP  function:analyse.Groupe  ##############################
##################################################################################################################################################

#### Based on Romain Lorrillière R script
#### Modified by Alan Amosse and Benjamin Yguel for integrating within Galaxy-E

suppressMessages(library(lme4))
suppressMessages(library(ggplot2))
suppressMessages(library(speedglm))
suppressMessages(library(arm))
suppressMessages(library(ggplot2))
#suppressMessages(library(reshape))
suppressMessages(library(data.table))
suppressMessages(library(reshape2))




###########
#delcaration des arguments et variables/ declaring some variables and load arguments

args = commandArgs(trailingOnly=TRUE)

print(args[7])

if ((length(args)<7) || (length(args)>11)) {
    stop("The tool need the following inputs :\n\n- A yearly species variations data set (.tabular). It may come from the main glm tool.\n- A species global tendencies dataset (.tabular). It may come from the main glm tool.\n- A species table filtered (.tabular). It may come from the Filter rare species tool.\n- An id to fix output repository name.\n- A list of species to exclude, can be empty.\n- A bias file.\n\n", call.=FALSE) #si pas d'arguments -> affiche erreur et quitte / if no args -> error and exit1
} else {
    donnees<-args[1] ###### Nom du fichier avec extension "***variationsAnnuellesEspece***.tabular", peut provenir de la fonction "mainglm" / file name without the file type "***variationsAnnuellesEspece***.tabular", may result from the function "mainglm"    
    donneesTrend <- args[2] ####### Nom du fichier avec extension "***tendanceGlobalEspece***.tabular", peut provenir de la fonction "mainglm" / / file name without the file type "***tendanceGlobalEspece***.tabular", may result from the function "mainglm"    
    tabSpecies<-args[3] ###### Nom du fichier avec extension ".typedefichier", peut provenir de la fonction "FiltreEspeceRare" / file name without the file type ".filetype", may result from the function "FiltreEspeceRare"  
    id<-args[4]  ##### nom du dossier de sortie des resultats / name of the output folder
    spExclude <- strsplit(args [5],",")[[1]] ##### liste d'espece qu on veut exclure de l analyse  / list of species that will be excluded
    tBiais <-args [7] ##########   fichier contenant le biais de détéction en fonction des occurances, obtenu à partir d'un modéle théorique de dynamique de pop et de survey / the file containing the detection bias depending on occurance data obtained with theoretical model of population dynamic and survey
    source(args[6])### chargement des fonctions analyseGroupe, geometriqueWeighted et checkfile / load the functions analyseGroupe, geometriqueWeighted and checkfile
}



#Import des données / Import data 
tBiais=read.table(tBiais,sep="\t",dec=".",header=TRUE) ###### charge le fichier contenant le biais de détéction en fonction des occurances, obtenu à partir d'un modéle théorique de dynamique de pop et de survey / load the file containing the detection bias obtained with theoretical model of population dynamic and survey
donnees <-  read.table(donnees,sep="\t",dec=".",header=TRUE) #### charge le fichier de resultat sur les tendances annuelles par espèce / load annual population evolution trend for each species obtained with the function mainglm
donneesTrend <- read.table(donneesTrend,sep="\t",dec=".",header=TRUE)#### charge le fichier de resultat sur les tendances sur la periode etudiée par espèce / load population evolution trend on the studied period for each species obtained with the function mainglm
tabsp <- read.table(tabSpecies,sep="\t",dec=".",header=TRUE)   #### charge le fichier de donnees sur nom latin, vernaculaire et abbreviation, espece indicatrice ou non / load the file with information on species specialization and if species are indicators


groupeNom = c("generaliste","milieux batis","milieux forestiers","milieux agricoles")
groupeCouleur = c("black","firebrick3","chartreuse4","orange")

vars_donnees<-c("id","code_espece","nom_espece","indicateur","annee","abondance_relative","IC_inferieur","IC_superieur","erreur_standard","p_value","significatif","nb_carre","nb_carre_presence","abondance")
err_msg_donnees<-"\nThe yearly species variation dataset doesn't have the right format. It need to have following 14 variables :\n- id\n- code_espece\n- nom_espece\n- indicateur\n- annee\n- abondance_relative\n- IC_inferieur\n- IC_superieur\n- erreur_standard\n- p_value\n- significatif\n- nb_carre\n- nb_carre_presence\n- abondance\n"

vars_donneesTrend<-c("id","code_espece","nom_espece","indicateur","nombre_annees","premiere_annee","derniere_annee","tendance","IC_inferieur","IC_superieur","pourcentage_variation","erreur_standard","p_value","significatif","categorie_tendance_EBCC","mediane_occurrence","valide","raison_incertitude")
err_msg_donneesTrend<-"\nThe species global tendances dataset doesn't have the right format. It need to have following 18 variables :\n- id\n- code_espece\n- nom_espece\n- indicateur\n- nombre_annees\n- premiere_annee\n- derniere_annee\n- tendance\n- IC_inferieur\n- IC_superieur\n- pourcentage_variation\n- erreur_standard\n- p_value\n- significatif\n- categorie_tendance_EBCC\n mediane_occurrence\n valide\n raison_incertitude\n"

vars_tabsp<-c("espece","nom","nomscientific","indicateur","specialisation")
err_msg_tabsp<-"\nThe species dataset filtered doesn't have the right format. It need to have the following 4 variables :\n- espece\n- nom\n- nomscientific\n- indicateur\n- specialisation\n"

vars_tBiais<-c("occurrenceMed","biais")
err_msg_tBiais<-"\nThe bias dataset doesn't have the right format. It need to have the following 2 variables :\n- occurenceMed\n- biais\n"

check_file(donnees,err_msg_donnees,vars_donnees,14)
check_file(donneesTrend,err_msg_donneesTrend,vars_donneesTrend,18)
check_file(tabsp,err_msg_tabsp,vars_tabsp,5)
check_file(tBiais,err_msg_tBiais,vars_tBiais,2)


spsFiltre=unique(levels(donnees$code_espece)) #### Recupère la liste des especes du tabCLEAN qui ont été sélectionnée et qui ont passé le filtre / retrieve species name that were selected and then filtered before

tabsp=subset (tabsp, (espece %in% spsFiltre)) #### Enlève les espèces qui n'ont pas passé le filtre ou exclu manuellement pour les analyses / keep only selected species and species with enough data
sp=as.character(tabsp$espece)  ##### liste des espece en code ou abbreviation gardées pour les analyses ### arg de la fonction  DECLARE AUSSI APRES DS FONCTION  / list of the code or abbreviation of the species kept for the analyses
tabsp=data.frame(tabsp,sp)### rajoute une colonne identique appelé sp / add new column called sp

if(length(spExclude)!=0) {
    donnees <- subset(donnees,!(code_espece %in% spExclude))
    tabsp <- subset(tabsp, !(espece %in% spExclude))

    cat("\n\nEspèces exclues de l'analyse :\n")
    cat(spExclude)
    cat("\n")
}
if(length(donnees$code_espece)==0){
    stop("There is no species left for the analyse.", call.=FALSE) #si pas plus d'espèce après filtre / if no more species after filter
}




## creation d'un dossier pour y mettre les resultats / create folder for the output of the analyses   ###### NORMALEMENT DOIT ËTRE DEJ2 CREER POUR LES SORTIES TENDANCES PAR SPS DONC PAS SUR QU IL FAUT REFAIRE CETTE ETAPE

dir.create(paste("Output/",id,sep=""),recursive=TRUE,showWarnings=FALSE)
cat(paste("Create Output/",id,"\n",sep=""))
dir.create(paste("Output/",id,"/Incertain/",sep=""),recursive=TRUE,showWarnings=FALSE)
cat(paste("Create Output/",id,"Incertain/\n",sep=""))





################## 
###  Do your analysis
analyseGroupe(id=id,tabsp=tabsp,donnees=donnees,donneesTrend=donneesTrend,ICfigureGroupeSp=TRUE,groupeNom = groupeNom,groupeCouleur=groupeCouleur)

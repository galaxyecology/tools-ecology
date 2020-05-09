#!/usr/bin/env Rscript

##################################################################################################################
################  Data transformation for population evolution trend analyses  function:makeTableAnalyse #########
##################################################################################################################
###########

library(data.table)
#delcaration des arguments et variables/ declaring some variables and load arguments

args = commandArgs(trailingOnly=TRUE) #####   par defaut prends les arguments comme du texte !!!! / default behaviour is to take the arguments as text !!!

if (length(args)==0) {
    stop("At least one argument must be supplied, an input dataset file (.tabular).", call.=FALSE) #si pas d'arguments -> affiche erreur et quitte / if no args -> error and exit1

} else {
    ImportduSTOC<-args[1] ###### Nom du fichier importé depuis la base de données STOCeps avec son extension / file name imported from the STOCeps database with the file type ".filetype"    
    source(args[2])### chargement des fonctions / load the functions

}

##### Le tableau de données doit posséder 4 variables en colonne: abondance ("abond"), les carrés ou sont réalisés les observatiosn ("carre"), la ou les années des observations ("annee"), et le code de ou des espèces ("espece")
##### Data must be a dataframe with 4 variables in column: abundance ("abond"), plots where observation where made ("carre"), year(s) of the different sampling ("annee"), and the species code ("espece") 


#Import des données / Import data 
data<- fread(ImportduSTOC,sep="\t",dec=".",header=TRUE,encoding="UTF-8") # 
vars_data<-c("carre","annee","espece","abond")
err_msg_data<-"The input dataset filtered doesn't have the right format. It need to have the following 4 variables :\n- carre\n- annee\n- espece\n- abond\n"
check_file(data,err_msg_data,vars_data,4)




#########
#Do your analysis
tableAnalyse<-makeTableAnalyse(data) #la fonction a un 'return' il faut donc stocker le resultat dans une nouvelle variable
#save the data in a output file in a tabular format
filename <- "Datatransformedforfiltering_trendanalysis.tabular"
write.table(tableAnalyse, filename,row.names=FALSE,sep="\t",dec=".",fileEncoding="UTF-8")

cat(paste("\nWrite table with data transformed for filtering. \n--> \"",filename,"\"\n",sep=""))

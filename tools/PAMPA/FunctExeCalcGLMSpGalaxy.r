#Rscript 

#####################################################################################################################
#####################################################################################################################
################################# Compute a Generalized Linear Model from your data #################################
#####################################################################################################################
#####################################################################################################################

###################### Packages
suppressMessages(library(multcomp))
suppressMessages(library(glmmTMB)) ###Version: 0.2.3
suppressMessages(library(gap))

###################### Load arguments and declaring variables

args = commandArgs(trailingOnly=TRUE)
#options(encoding = "UTF-8")

if (length(args) < 10) {
    stop("At least 4 arguments must be supplied : \n- two input dataset files (.tabular) : metrics table and unitobs table \n- Interest variable field from metrics table \n- Response variable from unitobs table.", call.=FALSE) #si pas d'arguments -> affiche erreur et quitte / if no args -> error and exit1

} else {
    Importdata <- args[1] ###### file name : metrics table
    ImportUnitobs <- args[2] ###### file name : unitobs informations
    colmetric <- as.numeric(args[3]) ###### Selected interest metric for GLM
    listFact <- strsplit(args [4],",")[[1]] ###### Selected response factors for GLM
    listRand <- strsplit(args [5],",")[[1]] ###### Selected randomized response factors for GLM
    colFactAna <- args[6] ####### (optional) Selected splitting factors for GLMs
    Distrib <- args[7] ###### (optional) Selected distribution for GLM 
    GLMout <- args[8] ###### (Optional) GLM object as Rdata output ?
    aggreg <- args[9] ###### Aggregation level of the data table
    source(args[10]) ###### Import functions

}
#### Data must be a dataframe with at least 3 variables : unitobs representing location and year ("observation.unit"), species code ("species.code") and abundance ("number")


#Import des données / Import data 
obs<- read.table(Importdata,sep="\t",dec=".",header=TRUE,encoding="UTF-8") #
obs[obs == -999] <- NA 
metric <- colnames(obs)[colmetric]
tabUnitobs <- read.table(ImportUnitobs,sep="\t",dec=".",header=TRUE,encoding="UTF-8")
tabUnitobs[tabUnitobs == -999] <- NA 

vars_data1<- c("species.code")
err_msg_data1<-"The input metrics dataset doesn't have the right format. It needs to have at least the following 3 variables :\n- species.code \n- observation.unit (or year and site)\n- numeric or integer metric\n"
check_file(obs,err_msg_data1,vars_data1,3)

vars_data2 <- c("observation.unit",listFact,listRand)
err_msg_data2<-"The input unitobs dataset doesn't have the right format. It needs to have at least the following 2 variables :\n- observation.unit (or year and site)\n- factors used in GLM (habitat, year and/or site)\n"
check_file(tabUnitobs,err_msg_data2,vars_data2[vars_data2 != "None"],2)


if (colFactAna != "None")
{
    FactAna <- colFactAna
    if (class(obs[FactAna]) == "numeric" || FactAna == "observation.unit"){stop("Wrong chosen separation factor : Analysis can't be separated by observation unit or numeric factor")}
}else{
    FactAna <- colFactAna
}

if (all(c(listFact,listRand)=="None")) {stop("GLM needs to have at least one response variable.")}

if (listFact[1] == "None" || all(is.element(listFact,listRand))) {stop("GLM can't have only random effects.")} 

####################################################################################################
########## Computing Generalized Linear Model ## Function : modeleLineaireWP2.unitobs.f ############
####################################################################################################

modeleLineaireWP2.species.f <- function(metrique, listFact, listRand, FactAna, Distrib, tabMetrics, tableMetrique, tabUnitobs, unitobs="observation.unit", outresiduals = FALSE, nbName="number")
{
    ## Purpose: Gestions des différentes étapes des modèles linéaires.
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : la métrique choisie.
    ##            factAna : le facteur de séparation des graphiques.
    ##            factAnaSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ##            tabMetrics : table de métriques.
    ##            tableMetrique : nom de la table de métriques.
    ##            dataEnv : environnement de stockage des données.
    ##            baseEnv : environnement de l'interface.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 août 2010, 15:59

    tmpData <- tabMetrics

    if (listRand[1] != "None")
    {
        if (all(is.element(listFact,listRand)) || listFact[1] == "None")
        {
            RespFact <- paste("(1|",paste(listRand,collapse=") + (1|"),")")
            listF <- NULL
            listFact <- listRand
        }else{
            listF <- listFact[!is.element(listFact,listRand)]
            RespFact <- paste(paste(listF, collapse=" + ")," + (1|",paste(listRand,collapse=") + (1|"),")")
            listFact <- c(listF,listRand)
        }   
    }else{
        listF <- listFact
        RespFact <- paste(listFact, collapse=" + ")
    }
    ##Creating model's expression :
    exprML <- eval(parse(text=paste(metrique, "~", RespFact)))

    ##Creating analysis table :
    listFactTab <- c(listFact,FactAna)
    listFactTab <- listFactTab[listFactTab != "None"]

    if (all(is.na(match(tmpData[,unitobs],tabUnitobs[,unitobs])))) {stop("Observation units doesn't match in the two input tables")}

    if(is.element("species.code",colnames(tmpData)))
    {
        col <- c(unitobs,metrique,FactAna)
        tmpData <- cbind(tmpData[,col], tabUnitobs[match(tmpData[,unitobs],tabUnitobs[,unitobs]),listFact])
        colnames(tmpData) <- c(col,listFact)

        for (i in listFactTab) {
            tmpData[,i] <- as.factor(tmpData[,i])
         }
    }else{
        stop("Warning : wrong data frame, data frame should be aggregated by observation unit (year and site) and species")
    }

    ## Suppression des 'levels' non utilisés :
    tmpData <- dropLevels.f(tmpData)

    ## Aide au choix du type d'analyse :
    if (Distrib == "None") 
    {
        if (metrique == "pres.abs") 
        { 
            loiChoisie <- "binomial"
        }else{
            switch(class(tmpData[,metrique]),
                  "integer"={loiChoisie <- "poisson"},
                  "numeric"={loiChoisie <- "gaussian"},
                  stop("Selected metric class doesn't fit, you should select an integer or a numeric variable"))
        }
    }else{
        loiChoisie <- Distrib
    }

    ##Create results table : 
    lev <- unlist(lapply(listF,FUN=function(x){levels(tmpData[,x])}))
    row <- levels(tmpData[,FactAna])

    if (is.element("year",listF) && ! is.element("year",listRand))
    {
        TabSum <- create.res.table(listRand=listRand, listFact=listFact, row=row, lev=unlist(c("year",lev)), distrib=loiChoisie)
    }else{
        TabSum <- create.res.table(listRand=listRand, listFact=listFact, row=row, lev=lev, distrib=loiChoisie)
    }
    ### creating rate table 
    TabRate <- data.frame(species=row, complete_plan=NA, balanced_plan=NA, NA_proportion_OK=NA, no_residual_dispersion=NA, uniform_residuals=NA, outliers_proportion_OK=NA, no_zero_inflation=NA, observation_factor_ratio_OK=NA, enough_levels_random_effect=NA, rate=NA)

    ## Compute Model(s) :
   
    for (sp in levels(tmpData[,FactAna])) 
    {
        cutData <- tmpData[grep(sp,tmpData[,FactAna]),]
        cutData <- dropLevels.f(cutData)

        res <-""
        resY <- ""

        if (listRand[1] != "None")
        {
            res <- tryCatch(glmmTMB(exprML,family=loiChoisie, data=cutData), error=function(e){})

            if (is.element("year",listF) && ! is.element("year",listRand)) #Model with year as continuous
            { 
                cutData$year <- as.numeric(cutData$year)
                resY <- tryCatch(glmmTMB(exprML,family=loiChoisie, data=cutData), error=function(e){})
                cutData$year <- as.factor(cutData$year)
            }else{resY <- ""}
        }else{
            res <- tryCatch(glm(exprML,data=cutData,family=loiChoisie), error=function(e){})
            if (is.element("year",listF)) #Model with year as continuous
            { 
                cutData$year <- as.numeric(cutData$year)
                resY <- tryCatch(glm(exprML,family=loiChoisie, data=cutData), error=function(e){})
                cutData$year <- as.factor(cutData$year)
            }else{resY <- ""}
        }

          ## Écriture des résultats formatés dans un fichier :
        if (! is.null(res))
        {   
            fileSaveGLMsp <- paste("GLM_",sp,".Rdata",sep="")
            save(res,file=fileSaveGLMsp)

            TabSum <- sortiesLM.f(objLM=res, objLMY=resY, TabSum=TabSum, factAna=factAna, cut=sp, colAna="analysis", lev=lev, Data=cutData, metrique=metrique, listFact=listFact)

            TabRate[TabRate[,"species"]==sp,c(2:11)] <- noteGLM.f(data=cutData, objLM=res, metric=metrique, listFact=listFact, details=TRUE)

        }else{
            cat("\nCannot compute GLM for species",sp,"Check if one or more factor(s) have only one level, or try with another distribution for the model in advanced settings \n\n")
        }

    }
    noteGLMs.f(tabRate=TabRate,exprML=exprML,objLM=res,file_out=TRUE)

    ## simple statistics and infos :
    filename <- "GLMSummaryFull.txt"

    ## Save data on model :

    infoStats.f(filename=filename, Data=tmpData, agregLevel=aggreg, type="stat",
                metrique=metrique, factGraph=factAna, #factGraphSel=modSel,
                listFact=listFact)#, listFactSel=listFactSel)

    return(TabSum)
}

################# Analysis

Tab <- modeleLineaireWP2.species.f(metrique=metric, listFact=listFact, listRand=listRand, FactAna=FactAna, Distrib=Distrib, tabMetrics=obs, tableMetrique=aggreg, tabUnitobs=tabUnitobs, outresiduals=SupprOutlay, nbName="number")

write.table(Tab,"GLMSummary.tabular", row.names=FALSE, sep="\t", dec=".",fileEncoding="UTF-8")


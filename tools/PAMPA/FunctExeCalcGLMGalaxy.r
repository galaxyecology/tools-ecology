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

if (colFactAna != "None")
{
    FactAna <- colnames(tabUnitobs)[as.numeric(colFactAna)]
    if (class(tabUnitobs[FactAna]) == "numeric" || FactAna == "observation.unit"){stop("Wrong chosen separation factor : Analysis can't be separated by observation unit or numeric factor")}
}else{
    FactAna <- colFactAna
}


#factors <- fact.det.f(Obs=obs)

vars_data1<- NULL
err_msg_data1<-"The input metrics dataset doesn't have the right format. It needs to have at least the following 2 variables :\n- observation.unit (or year and site)\n- numeric or integer metric\n"
check_file(obs,err_msg_data1,vars_data1,2)

vars_data2 <- c("observation.unit",listFact,listRand)
err_msg_data2<-"The input unitobs dataset doesn't have the right format. It needs to have at least the following 2 variables :\n- observation.unit (or year and site)\n- factors used in GLM (habitat, year and/or site)\n"
check_file(tabUnitobs,err_msg_data2,vars_data2[vars_data2 != "None"],2)

if (all(c(listFact,listRand)=="None")) {stop("GLM needs to have at least one response variable.")}

if (listFact[1] == "None" || all(is.element(listFact,listRand))) {stop("GLM can't have only random effects.")} 


####################################################################################################
########## Computing Generalized Linear Model ## Function : modeleLineaireWP2.unitobs.f ############
####################################################################################################

modeleLineaireWP2.unitobs.f <- function(metrique, listFact, listRand, FactAna, Distrib, tabMetrics, tableMetrique, tabUnitobs, unitobs="observation.unit", nbName="number")
{
    ## Purpose: Monitoring steps for GLM on unitobs
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : selected metric
    ##            listFact : Factors for GLM
    ##            listRand : Random factors for GLM
    ##            factAna : Separation factor for GLMs
    ##            Distrib : selected distribution for model
    ##            tabMetrics : data table metrics
    ##            tableMetrique : data table's name
    ##            tabUnitobs : data table unitobs
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 août 2010, 15:59 modified by Coline ROYAUX 04 june 2020

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

    listFactTab <- c(listFact, FactAna)
    listFactTab <- listFactTab[listFactTab != "None"]

    if (all(is.na(match(tmpData[,unitobs],tabUnitobs[,unitobs])))) {stop("Observation units doesn't match in the two input tables")}

    if(! is.element("species.code",colnames(tmpData)))
    {
        col <- c(unitobs,metrique)
        tmpData <- cbind(tmpData[,col], tabUnitobs[match(tmpData[,unitobs],tabUnitobs[,unitobs]),listFactTab])
        colnames(tmpData) <- c(col,listFactTab)

        for (i in listFactTab) {
            switch(i,
                  tmpData[,i] <- as.factor(tmpData[,i]))
         }
    }else{
        stop("Warning : wrong data frame, data frame should be aggregated by observation unit (year and site)")
    }

    ## Suppress unsed 'levels' :
    tmpData <- dropLevels.f(tmpData)

    ## Automatic choice of distribution if none is selected by user :
    if (Distrib == "None") 
    {
        switch(class(tmpData[,metrique]),
              "integer"={loiChoisie <- "poisson"},
              "numeric"={loiChoisie <- "gaussian"},
              stop("Selected metric class doesn't fit, you should select an integer or a numeric variable"))
    }else{
        loiChoisie <- Distrib
    }

    if (FactAna != "None" && nlevels(tmpData[,FactAna]) > 1)
    {
        Anacut <- levels(tmpData[,FactAna])
    }else{
        Anacut <- NULL
    }

    ##Create results table : 
    lev <- unlist(lapply(listF,FUN=function(x){levels(tmpData[,x])}))
    row <- c("global", Anacut)

    if (is.element("year",listF) && ! is.element("year",listRand))
    {
        TabSum <- create.res.table(listRand=listRand, listFact=listFact, row=row, lev=unlist(c("year",lev)), distrib=loiChoisie)
    }else{
        TabSum <- create.res.table(listRand=listRand, listFact=listFact, row=row, lev=lev, distrib=loiChoisie)
    }
    ### creating rate table 
    TabRate <- data.frame(analysis=row, complete_plan=NA, balanced_plan=NA, NA_proportion_OK=NA, no_residual_dispersion=NA, uniform_residuals=NA, outliers_proportion_OK=NA, no_zero_inflation=NA, observation_factor_ratio_OK=NA, enough_levels_random_effect=NA, rate=NA)

    ##plural analysis
    for (cut in Anacut) 
    {
        cutData <- tmpData[grep(cut,tmpData[,FactAna]),]
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

          ## Write results :
         if (! is.null(res))
         {
            fileSaveGLMcut <- paste("GLM_",cut,".Rdata",sep="")
            save(res,file=fileSaveGLMcut)

            TabSum <- sortiesLM.f(objLM=res, objLMY=resY, TabSum=TabSum, metrique=metrique,
                                  factAna=factAna, cut=cut, colAna="analysis", lev=lev, #modSel=iFactGraphSel, listFactSel=listFactSel,
                                  listFact=listFact,
                                  Data=cutData)

            TabRate[TabRate[,"analysis"]==cut,c(2:11)] <- noteGLM.f(data=cutData, objLM=res, metric=metrique, listFact=listFact, details=TRUE)

        }else{
            cat("\nCannot compute GLM for level",cut,"Check if one or more factor(s) have only one level, or try with another distribution for the model in advanced settings \n\n")
        }

    }

    ## Global analysis : 

    resG <- ""
    resGY <- ""

    if (listRand[1] != "None")
    {
        resG <- glmmTMB(exprML,family=loiChoisie, data=tmpData)
        if (is.element("year",listFact) && ! is.element("year",listRand)) #Model with year as continuous
        { 
            tmpData$year <- as.numeric(tmpData$year)
            resGY <- glmmTMB(exprML,family=loiChoisie, data=tmpData)
            tmpData$year <- as.factor(tmpData$year)
        }else{resGY <- ""}

    }else{
        resG <- glm(exprML,data=tmpData,family=loiChoisie)
        if (is.element("year",listFact)) #Model with year as continuous
        { 
            tmpData$year <- as.numeric(tmpData$year)
            resGY <- glm(exprML,family=loiChoisie, data=tmpData)
            tmpData$year <- as.factor(tmpData$year)
        }else{resGY <- ""}
    }

    ## write results :

    save(resG,file="global_GLM.Rdata")

    TabSum <- sortiesLM.f(objLM=resG, objLMY=resGY, TabSum=TabSum, metrique=metrique,
                          factAna=factAna, cut="global", colAna="analysis", lev=lev, #modSel=iFactGraphSel, listFactSel=listFactSel,
                          listFact=listFact,
                          Data=tmpData)

    TabRate[TabRate[,"analysis"]=="global",c(2:11)] <- noteGLM.f(data=tmpData, objLM=resG, metric=metrique, listFact=listFact, details=TRUE)
    noteGLMs.f(tabRate=TabRate,exprML=exprML,objLM=resG, file_out=TRUE)

    ## simple statistics and infos :
    filename <- "GLMSummaryFull.txt"
        
    infoStats.f(filename=filename, Data=tmpData, agregLevel=aggreg, type="stat",
                metrique=metrique, factGraph=factAna, #factGraphSel=modSel,
                listFact=listFact)#, listFactSel=listFactSel)

    TabSum$separation <- FactAna

    return(TabSum)

}

################# Analysis

Tab <- modeleLineaireWP2.unitobs.f(metrique=metric, listFact=listFact, listRand=listRand, FactAna=FactAna, Distrib=Distrib, tabMetrics=obs, tableMetrique=aggreg, tabUnitobs=tabUnitobs, nbName="number")

write.table(Tab,"GLMSummary.tabular", row.names=FALSE, sep="\t", dec=".",fileEncoding="UTF-8")

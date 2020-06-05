#Rscript


##################################################################################################################################
####################### PAMPA Galaxy tools functions : Calculate metrics, compute GLM and plot   #################################
##################################################################################################################################

#### Based on Yves Reecht R script
#### Modified by Coline ROYAUX for integrating within Galaxy-E

######################################### start of the function fact.def.f called by FunctExeCalcCommIndexesGalaxy.r and FunctExeCalcPresAbsGalaxy.r
####### Define the finest aggregation with the observation table

fact.det.f <- function (Obs,
                        size.class="size.class",
                        code.especes="species.code",
                        unitobs="observation.unit")
{
    if (any(is.element(c(size.class), colnames(obs))) && all(! is.na(obs[, size.class])))
        {
            factors <- c(unitobs, code.especes, size.class)
        }else{
            factors <- c(unitobs, code.especes)
        }
    return(factors)
}

######################################### end of the function fact.def.f 

######################################### start of the function def.typeobs.f called by FunctExeCalcCommIndexesGalaxy.r and FunctExeCalcPresAbsGalaxy.r
####### Define observation type from colnames

def.typeobs.f <- function(Obs)
{
    if (any(is.element(c("rotation","rot","rotate"),colnames(obs))))
    {
        ObsType <- "SVR"
    }else{
        ObsType <- "other"
    }
    return(ObsType)
}
######################################### end of the function fact.def.f 

######################################### start of the function create.unitobs called by FunctExeCalcCommIndexesGalaxy.r and FunctExeCalcPresAbsGalaxy.r
####### Create unitobs column when inexistant
create.unitobs <- function(data,year="year",point="point", unitobs="observation.unit")
{
    if (is.element(paste(unitobs),colnames(data)) && all(grepl("[1-2][0|8|9][0-9]{2}_.*",data[,unitobs])==FALSE))
    {
        unitab <- data

    }else{ 

        unitab <- unite(data,col="observation.unit",c(year,point))
    }
    return(unitab)
}
######################################### start of the function create.unitobs

######################################### start of the function create.year.point called by FunctExeCalcCommIndexesGalaxy.r and FunctExeCalcPresAbsGalaxy.r
####### separate unitobs column when existant
create.year.point <- function(data,year="year",point="point", unitobs="observation.unit")
{
    if (all(grepl("[1-2][0|8|9][0-9]{2}_.*",data[,unitobs]))==TRUE)
    {
        tab <- separate(data,col=unitobs,into=c(year,point),sep="_")
    }else{
        tab <- separate(data,col=unitobs,into=c("site1", year,"obs"),sep=c(2,4))
        tab <- unite(tab, col=point, c("site1","obs"))

    }

    tab <- cbind(tab,observation.unit = data[,unitobs])

    return(tab)
}
######################################### start of the function create.unitobs

######################################### start of the function check_file called by every Galaxy Rscripts

check_file<-function(dataset,err_msg,vars,nb_vars){

    ## Purpose: General function to check integrity of input file. Will 
    ##          check numbers and contents of variables(colnames).
    ##          return an error message and exit if mismatch detected
    ## ----------------------------------------------------------------------
    ## Arguments: dataset : dataset name
    ##            err_msg : output error
    ##            vars : expected name of variables
    ##            nb_vars : expected number of variables
    ## ----------------------------------------------------------------------
    ## Author: Alan Amosse, Benjamin Yguel 

    if(ncol(dataset) < nb_vars){ #checking for right number of columns in the file if not = error message
        cat("\nerr nb var\n") 
        stop(err_msg, call.=FALSE)
    }

    for(i in vars){
        if(!(i %in% names(dataset))){ #checking colnames
            stop(err_msg,call.=FALSE)
        }
    }
}

######################################### end of the function check_file


######################################### start of the function statRotationsNumber.f called by calc.numbers.f

statRotationsNumber.f <- function(factors, obs)
{
    ## Purpose: Computing abundance statistics by rotation (max, sd) 
    ##          on SVR data
    ## ----------------------------------------------------------------------
    ## Arguments: factors : Names of aggregation factors
    ##            obs : observation data
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2012, 16:01 modified by Coline ROYAUX 04 june 2020

    ## Identification of valid rotations :
    if (is.element("observation.unit", factors))
    {
        ## valid rotations (empty must be there as well) :
        rotations <- tapply(obs$rotation,
                            as.list(obs[ , c("observation.unit", "rotation"), drop=FALSE]),
                            function(x)length(x) > 0)

        ## Changing NA rotations in FALSE :
        rotations[is.na(rotations)] <- FALSE
    }else{
        #stop(mltext("statRotations.err.1"))
    }

    ## ###########################################################
    ## Abundance per rotation at chosen aggregation factors :
    nombresR <- tapply(obs$number,
                       as.list(obs[ , c(factors, "rotation"), drop=FALSE]),
                       function(x,...){ifelse(all(is.na(x)), NA, sum(x,...))},
                       na.rm = TRUE)

    ## If valid rotation NA are considered 0 :
    nombresR <- sweep(nombresR,
                      match(names(dimnames(rotations)), names(dimnames(nombresR)), nomatch=NULL),
                      rotations,        # Tableau des secteurs valides (booléens).
                      function(x, y)
                  {
                      x[is.na(x) & y] <- 0 # Lorsque NA et secteur valide => 0.
                      return(x)
                  })

    ## ##################################################
    ## Statistics :

    ## Means :
    nombresMean <- apply(nombresR, which(is.element(names(dimnames(nombresR)), factors)),
                         function(x,...){ifelse(all(is.na(x)), NA, mean(x,...))}, na.rm=TRUE)

    ## Maxima :
    nombresMax <- apply(nombresR, which(is.element(names(dimnames(nombresR)), factors)),
                        function(x,...){ifelse(all(is.na(x)), NA, max(x,...))}, na.rm=TRUE)

    ## SD :
    nombresSD <- apply(nombresR, which(is.element(names(dimnames(nombresR)), factors)),
                       function(x,...){ifelse(all(is.na(x)), NA, sd(x,...))}, na.rm=TRUE)

    ## Valid rotations count :
    nombresRotations <- apply(rotations, 1, sum, na.rm=TRUE)

    ## Results returned as list :
    return(list(nombresMean=nombresMean, nombresMax=nombresMax, nombresSD=nombresSD,
                nombresRotations=nombresRotations, nombresTot=nombresR))
}

######################################### end of the function statRotationsNumber.f 

######################################### start of the function calcNumber.default.f called by calc.numbers.f

calcNumber.default.f <- function(obs,
                                 factors=c("observation.unit", "species.code", "size.class"),
                                 nbName="number")
{
    ## Purpose : Compute abundances at finest aggregation 
    ## ---------------------------------------------------------------------
    ## Arguments: obs : observation table
    ##            factors : aggregation factors
    ##            nbName : name of abundance column.
    ##
    ## Output: array with ndimensions = nfactors.
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 13:38 modified by Coline ROYAUX 04 june 2020

    ## Sum individuals number :
    nbr <- tapply(obs[ , nbName],
                  as.list(obs[ , factors]),
                  sum, na.rm = TRUE)

    ## Absences as "true zero" :
    nbr[is.na(nbr)] <- 0

    return(nbr)
}

######################################### end of the function calcNumber.default.f

######################################### start of the function calc.numbers.f

calc.numbers.f <- function(obs, ObsType="", factors=c("observation.unit", "species.code", "size.class"), nbName="number")
{
    ## Purpose: Produce data.frame used as table from output of calcNumber.default.f().
    ## ----------------------------------------------------------------------
    ## Arguments: obs : observation table
    ##            ObsType : Type of observation (SVR, LIT, ...)
    ##            factors : aggregation factors
    ##            nbName : name of abundance column
    ##
    ## Output: data.frame with (N aggregation factors + 1) columns
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 19 déc. 2011, 13:46 modified by Coline ROYAUX 04 june 2020

    if (ObsType == "SVR")
    {
         ## Compute SVR abundances statistics :
         statRotations <- statRotationsNumber.f(factors=factors,
                                                  obs=obs)

         ## Mean for rotating videos (3 rotations at most times) :
         nbr <- statRotations[["nombresMean"]]

    }else{

         nbr <- calcNumber.default.f(obs, factors, nbName)
    }

    res <- as.data.frame(as.table(nbr), responseName=nbName)

    if (is.element("size.class", colnames(res)))
    {
        res$size.class[res$size.class == ""] <- NA
    }else{}

    ## If integer abundances :
    if (isTRUE(all.equal(res[ , nbName], as.integer(res[ , nbName]))))
    {
        res[ , nbName] <- as.integer(res[ , nbName])
    }else{}

    if (ObsType == "SVR")
    {
        ## statistics on abundances :
        res$number.max <- as.vector(statRotations[["nombresMax"]])
        res$number.sd <- as.vector(statRotations[["nombresSD"]])
              
    }else{}

    return(res)
}

######################################### end of the function calc.numbers.f

######################################### start of the function presAbs.f called by calcBiodiv.f

presAbs.f <- function(nombres, logical=FALSE)
{
    ## Purpose: Compute presence absence from abundances
    ## ----------------------------------------------------------------------
    ## Arguments: nombres : vector of individuals count.
    ##            logical : (boolean) results as boolean or 0/1 ?
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 29 oct. 2010, 10:20 modified by Coline ROYAUX 04 june 2020

    if (any(nombres < 0, na.rm=TRUE))
    {
        stop("Negative abundances!")
    }else{}

    if (logical)
    {
        return(nombres > 0)
    }else{
        nombres[nombres > 0] <- 1
        return(nombres)
    }
}

######################################### end of the function presAbs.f

######################################### start of the function betterCbind called by agregations.generic.f

betterCbind <- function(..., dfList=NULL, deparse.level = 1)
{
    ## Purpose: Apply cbind to data frame with mathcing columns but without
    ##          redundancies.
    ## ----------------------------------------------------------------------
    ## Arguments: same as cbind...
    ##            dfList : data.frames list
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 17 janv. 2012, 21:10 modified by Coline ROYAUX 04 june 2020

    if (is.null(dfList))
    {
        dfList <- list(...)
    }else{}

    return(do.call(cbind,
                   c(list(dfList[[1]][ , c(tail(colnames(dfList[[1]]), -1),
                                           head(colnames(dfList[[1]]), 1))]),
                     lapply(dfList[-1],
                            function(x, colDel)
                        {
                            return(x[ , !is.element(colnames(x),
                                                    colDel),
                                     drop=FALSE])
                        },
                            colDel=colnames(dfList[[1]])),
                     deparse.level=deparse.level)))
}

######################################### end of the function betterCbind

######################################### start of the function agregation.f called by agregations.generic.f

agregation.f <- function(metric, Data, factors, casMetrique,
                         nbName="number")
{
    ## Purpose: metric aggregation
    ## ----------------------------------------------------------------------
    ## Arguments: metric: colnames of chosen metric
    ##            Data: Unaggregated data table
    ##            factors: aggregation factors vector
    ##            casMetrique: named vector of observation types depending
    ##                         on chosen metric
    ##            nbName : abundance column name
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 20 déc. 2011, 14:29 modified by Coline ROYAUX 04 june 2020

    switch(casMetrique[metric],
           "sum"={
               res <- tapply(Data[ , metric],
                             as.list(Data[ , factors, drop=FALSE]),
                             function(x)
                         {
                             ifelse(all(is.na(x)),
                                    NA,
                                    sum(x, na.rm=TRUE))
                         })
           },
           "w.mean"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , factors, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metric])),
                                    NA,
                                    weighted.mean(Data[ii, metric],
                                                  Data[ii, nbName],
                                                  na.rm=TRUE))
                         })
           },
           "w.mean.colonies"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , factors, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metric])),
                                    NA,
                                    weighted.mean(Data[ii, metric],
                                                  Data[ii, "colonies"],
                                                  na.rm=TRUE))
                         })
           },
           "w.mean.prop"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , factors, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metric])) || sum(Data[ii, "nombre.tot"], na.rm=TRUE) == 0,
                                    NA,
                                    ifelse(all(na.omit(Data[ii, metric]) == 0), # Pour ne pas avoir NaN.
                                           0,
                                           (sum(Data[ii, nbName][ !is.na(Data[ii, metric])], na.rm=TRUE) /
                                            sum(Data[ii, "nombre.tot"], na.rm=TRUE)) *
                                           ## Correction if size class isn't an aggregation factor
                                           ## (otherwise value divided by number of present classes) :
                                           ifelse(is.element("size.class", factors),
                                                  100,
                                                  100 * length(unique(Data$size.class)))))
                         })

           },
           "w.mean.prop.bio"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , factors, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metric])) || sum(Data[ii, "tot.biomass"], na.rm=TRUE) == 0,
                                    NA,
                                    ifelse(all(na.omit(Data[ii, metric]) == 0), # Pour ne pas avoir NaN.
                                           0,
                                           (sum(Data[ii, "biomass"][ !is.na(Data[ii, metric])], na.rm=TRUE) /
                                            sum(Data[ii, "tot.biomass"], na.rm=TRUE)) *
                                           ## Correction if size class isn't an aggregation factor
                                           ## (otherwise value divided by number of present classes) :
                                           ifelse(is.element("size.class", factors),
                                                  100,
                                                  100 * length(unique(Data$size.class)))))
                         })

           },
           "pres"={
               res <- tapply(Data[ , metric],
                             as.list(Data[ , factors, drop=FALSE]),
                             function(x)
                         {
                             ifelse(all(is.na(x)), # When only NAs.
                                    NA,
                                    ifelse(any(x > 0, na.rm=TRUE), # Otherwise...
                                           1, # ... presence if at least one observation in the group.
                                           0))
                         })
           },
           "nbMax"={
               ## Recuperation of raw abundances with selections :
               nbTmp <- getReducedSVRdata.f(dataName=".NombresSVR", data=Data)

              ## Sum by factor cross / rotation :
               nbTmp2 <- apply(nbTmp,
                             which(is.element(names(dimnames(nbTmp)), c(factors, "rotation"))),
                             function(x)
                         {
                             ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))
                         })

               ## Sum by factor cross :
               res <- as.array(apply(nbTmp2,
                                     which(is.element(names(dimnames(nbTmp)), factors)),
                                     function(x)
                                 {
                                     ifelse(all(is.na(x)), NA, max(x, na.rm=TRUE))
                                 }))
           },
           "nbSD"={
               ## Recuperation of raw abundances with selections :
               nbTmp <- getReducedSVRdata.f(dataName=".NombresSVR", data=Data)

               ## Sum by factor cross / rotation :
               nbTmp2 <- apply(nbTmp,
                             which(is.element(names(dimnames(nbTmp)), c(factors, "rotation"))),
                             function(x)
                         {
                             ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))
                         })

               ## Sum by factor cross :
               res <- as.array(apply(nbTmp2,
                                     which(is.element(names(dimnames(nbTmp)), factors)),
                                     function(x)
                                 {
                                     ifelse(all(is.na(x)), NA, sd(x, na.rm=TRUE))
                                 }))
           },
           "densMax"={
               ## Recuperation of raw abundances with selections :
               densTmp <- getReducedSVRdata.f(dataName=".DensitesSVR", data=Data)

               ## Sum by factor cross / rotation :
               densTmp2 <- apply(densTmp,
                                 which(is.element(names(dimnames(densTmp)), c(factors, "rotation"))),
                                 function(x)
                             {
                                 ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))
                             })

               ## Sum by factor cross :
               res <- as.array(apply(densTmp2,
                                     which(is.element(names(dimnames(densTmp)), factors)),
                                     function(x)
                                 {
                                     ifelse(all(is.na(x)), NA, max(x, na.rm=TRUE))
                                 }))
           },
           "densSD"={
               ## Recuperation of raw abundances with selections :
               densTmp <- getReducedSVRdata.f(dataName=".DensitesSVR", data=Data)

               ## Sum by factor cross / rotation :
               densTmp2 <- apply(densTmp,
                                 which(is.element(names(dimnames(densTmp)), c(factors, "rotation"))),
                                 function(x)
                             {
                                 ifelse(all(is.na(x)), NA, sum(x, na.rm=TRUE))
                             })

               ## Sum by factor cross :
               res <- as.array(apply(densTmp2,
                                     which(is.element(names(dimnames(densTmp)), factors)),
                                     function(x)
                                 {
                                     ifelse(all(is.na(x)), NA, sd(x, na.rm=TRUE))
                                 }))
           },
           "%.nesting"={
               res <- tapply(1:nrow(Data),
                             as.list(Data[ , factors, drop=FALSE]),
                             function(ii)
                         {
                             ifelse(all(is.na(Data[ii, metric])),
                                    NA,
                                    weighted.mean(Data[ii, metric],
                                                  Data[ii, "readable.tracks"],
                                                  na.rm=TRUE))
                         })
           },
           stop("Not implemented!")
           )

    ## dimension names
    names(dimnames(res)) <- c(factors)

    ## Transformation to long format :
    reslong <- as.data.frame(as.table(res), responseName=metric)
    reslong <- reslong[ , c(tail(colnames(reslong), 1), head(colnames(reslong), -1))] # metric first

    return(reslong)
}

######################################### end of the function agregation.f

######################################### start of the function agregations.generic.f called y calcBiodiv.f in FucntExeCalcCommIndexesGalaxy.r

agregations.generic.f <- function(Data, metrics, factors, listFact=NULL, unitSpSz=NULL, unitSp=NULL,
                                  nbName="number")
{
    ## Purpose: Aggregate data 
    ## ----------------------------------------------------------------------
    ## Arguments: Data : data set
    ##            metrics : aggregated metric
    ##            factors : aggregation factors
    ##            listFact : other factors to aggregate and add to output
    ##            unitSpSz : Metrics table by unitobs/species/Size Class
    ##            unitSp : Metrics table by unitobs/species
    ##            nbName : abundance colname
    ##
    ## Output : aggregated data frame
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 18 oct. 2010, 15:47 modified by Coline ROYAUX 04 june 2020

    ## trt depending on metric type :
    casMetrique <- c("number"="sum",
                     "mean.length"="w.mean",
                     "taille_moy"="w.mean",
                     "biomass"="sum",
                     "Biomass"="sum",
                     "weight"="sum",
                     "mean.weight"="w.mean",
                     "density"="sum",
                     "Density"="sum",
                     "CPUE"="sum",
                     "CPUE.biomass"="sum",
                     "pres.abs"="pres",
                     "abundance.prop.SC"="w.mean.prop", # Not OK [!!!] ?
                     "biomass.prop.SC"="w.mean.prop.bio",  # Not OK [!!!] ?
                     ## Benthos :
                     "colonies"="sum",
                     "coverage"="sum",
                     "mean.size.colonies"="w.mean.colonies",
                     ## SVR (expérimental) :
                     "number.max"="nbMax",
                     "number.sd"="nbSD",
                     "density.max"="densMax",
                     "density.sd"="densSD",
                     "biomass.max"="sum",
                     "spawning.success"="%.nesting",
                     "spawnings"="sum",
                     "readable.tracks"="sum",
                     "tracks.number"="sum")

    ## add "readable.tracks" for egg laying percentage :
    if (any(casMetrique[metrics] == "%.nesting"))
    {
        if (is.element("size.class", colnames(Data)))
        {
            if (is.null(unitSpSz)) stop("unitSpSz doit être défini")

            Data <- merge(Data,
                          unitSpSz[ , c("species.code", "observation.unit", "size.class", "readable.tracks")],
                          by=c("species.code", "observation.unit", "size.class"),
                          suffixes=c("", ".y"))
        }else{
            if (is.null(unitSp)) stop("unitSp must be defined")

            Data <- merge(Data,
                          unitSp[ , c("species.code", "observation.unit", "readable.tracks")],
                          by=c("species.code", "observation.unit"),
                          suffixes=c("", ".y"))
        }
    }else{}

    ## Add "number" field for computing ponderate means if absent :
    if (any(casMetrique[metrics] == "w.mean" | casMetrique[metrics] == "w.mean.prop"))
    {
        if (is.element("size.class", colnames(Data)))
        {
            if (is.null(unitSpSz)) stop("unitSpSz must be defined")

            Data <- merge(Data,
                          unitSpSz[ , c("species.code", "observation.unit", "size.class", nbName)],
                          by=c("species.code", "observation.unit", "size.class"),
                          suffixes=c("", ".y"))

            ## add tot abundance / species / observation unit :
            nbTot <- tapply(unitSpSz[ , nbName],
                            as.list(unitSpSz[ , c("species.code", "observation.unit")]),
                            sum, na.rm=TRUE)

            Data <- merge(Data,
                          as.data.frame(as.table(nbTot), responseName="nombre.tot"))
        }else{
            if (is.null(unitSp)) stop("unitSp must be defined")

            Data <- merge(Data,
                          unitSp[ , c("species.code", "observation.unit", nbName)], # [!!!] unitSpSz ?
                          by=c("species.code", "observation.unit"),
                          suffixes=c("", ".y"))
        }
    }else{}

    ## Add biomass field of biomass proportion by size class :
    if (any(casMetrique[metrics] == "w.mean.prop.bio"))
    {
        if (is.null(unitSpSz)) stop("unitSpSz doit être défini")

        Data <- merge(Data,
                      unitSpSz[ , c("species.code", "observation.unit", "size.class", "biomass")],
                      by=c("species.code", "observation.unit", "size.class"),
                      suffixes=c("", ".y"))

        ## add tot biomass / species / observation unit :
        biomTot <- tapply(unitSpSz$biomass,
                          as.list(unitSpSz[ , c("species.code", "observation.unit")]),
                          function(x)
                      {
                          ifelse(all(is.na(x)),
                                 NA,
                                 sum(x, na.rm=TRUE))
                      })

        Data <- merge(Data,
                      as.data.frame(as.table(biomTot), responseName="tot.biomass"))
    }

    ## add colony field for ponderate means pondérées if absent :
    if (any(casMetrique[metrics] == "w.mean.colonies" & ! is.element("colonies", colnames(Data))))
    {
        Data$colonies <- unitSp[match(apply(Data[ , c("species.code", "observation.unit")],
                                           1, paste, collapse="*"),
                                     apply(unitSp[ , c("species.code", "observation.unit")],
                                           1, paste, collapse="*")), "colonies"]
    }else{}


    ## Aggregation of metric depending on factors :
    reslong <- betterCbind(dfList=lapply(metrics,   # sapply used to have names
                                         agregation.f,
                                         Data=Data, factors=factors, casMetrique=casMetrique,
                                         nbName=nbName))

    ## Aggregation and add other factors :
    if ( ! (is.null(listFact) || length(listFact) == 0))
    {
        reslong <- cbind(reslong,
                         sapply(Data[ , listFact, drop=FALSE],
                                function(fact)
                            {
                                tapply(fact,
                                       as.list(Data[ , factors, drop=FALSE]),
                                       function(x)
                                   {
                                       if (length(x) > 1 && length(unique(x)) > 1) # must be one modality
                                       {
                                           return(NULL)                  # otherwise it is NULL
                                       }else{
                                           unique(as.character(x))
                                       }
                                   })
                            }))
    }else{}

    ## If some factors aren't at the right class :
    if (any(tmp <- sapply(reslong[ , listFact, drop=FALSE], class) != sapply(Data[ , listFact, drop=FALSE], class)))
    {
        for (i in which(tmp))
        {
            switch(sapply(Data[ , listFact, drop=FALSE], class)[i],
                   "integer"={
                       reslong[ , listFact[i]] <- as.integer(as.character(reslong[ , listFact[i]]))
                   },
                   "numeric"={
                       reslong[ , listFact[i]] <- as.numeric(as.character(reslong[ , listFact[i]]))
                   },
                   reslong[ , listFact[i]] <- eval(call(paste("as", sapply(Data[ , listFact, drop=FALSE], class)[i], sep="."),
                                                        reslong[ , listFact[i]]))
                   )
        }
    }else{}

    ## Initial order of factors levels :
    reslong <- as.data.frame(sapply(colnames(reslong),
                                    function(x)
                                {
                                    if (is.factor(reslong[ , x]))
                                    {
                                        return(factor(reslong[ , x], levels=levels(Data[ , x])))
                                    }else{
                                        return(reslong[ , x])
                                    }
                                }, simplify=FALSE))


    ## Check of other aggregated factors supplémentaires. There must be no NULL elements :
    if (any(sapply(reslong[ , listFact], function(x){any(is.null(unlist(x)))})))
    {
        warning(paste("One of the suppl. factors is probably a subset",
                      " of the observations grouping factor(s).", sep=""))
        return(NULL)
    }else{
        return(reslong)
    }
}

######################################### end of the function agregations.generic.f

######################################### start of the function dropLevels.f called y calcBiodiv.f in FucntExeCalcCommIndexesGalaxy.r and modeleLineaireWP2.unitobs.f in FunctExeCalcGLMGalaxy.r
dropLevels.f <- function(df, which=NULL)
{
    ## Purpose: Suppress unused levels of factors
    ## ----------------------------------------------------------------------
    ## Arguments: df : a data.frame
    ##            which : included columns index (all by default)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 août 2010, 13:29 modified by Coline ROYAUX 04 june 2020

    if (class(df) != "data.frame")
    {
        stop("'df' must be a data.frame")
    }else{
        if (is.null(which))
        {
            x <- as.data.frame(sapply(df, function(x)
                                  {
                                      return(x[ ,drop=TRUE])
                                  }, simplify=FALSE),
                               stringsAsFactors=FALSE)
        }else{                          # Only some columns used
            x <- df

            x[ , which] <- as.data.frame(sapply(df[ , which, drop=FALSE],
                                                function(x)
                                            {
                                                return(x[ , drop=TRUE])
                                            }, simplify=FALSE),
                                         stringsAsFactors=FALSE)
        }

        return(x)
    }
}
######################################### end of the function dropLevels.f

######################################### start of the function subsetToutesTables.f called by modeleLineaireWP2.unitobs.f in FunctExeCalcGLMGalaxy.r

subsetToutesTables.f <- function(metrique, tabMetrics, facteurs, selections,
                                 tabUnitobs, refesp, tableMetrique="", nbName="number", ObsType = "",
                                 exclude=NULL, add=c("species.code", "observation.unit"))
{
    ## Purpose: Extract useful data only from chosen metrics and factors
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : chosen metric
    ##            facteurs : all chosen factors
    ##            selections : corresponding modality selected 
    ##            tableMetrique : metrics table name
    ##            exclude : factors levels to exclude
    ##            add : field to add to data table
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  6 août 2010, 16:46 modified by Coline ROYAUX 04 june 2020

    ## If no metrics table available :
    if (is.element(tableMetrique, c("", "TableOccurrences", "TablePresAbs")))
    {
        tableMetrique <- "unitSp"
    }else{}

    casTables <- c("unitSp"="unitSp",
                   "TablePresAbs"="unitSp",
                   "unitSpSz"="unitSpSz")

    ## Recuperation of metrics table :
    dataMetrique <- tabMetrics
    unitobs <- tabUnitobs
    refesp <- refesp

    ## If no metrics available or already computed :
    if (is.element(metrique, c("", "occurrence.frequency")))
    {
        metrique <- "tmp"
        dataMetrique$tmp <- 0
        dataMetrique$tmp[dataMetrique[ , nbName] > 0] <- 1
    }else{}

    if (!is.null(add))
    {
        metriques <- c(metrique, add[is.element(add, colnames(dataMetrique))])
    }else{
        metriques <- metrique
    }

    ## Subset depending on metrics table
    switch(casTables[tableMetrique],
           ## Observation table by unitobs and species :
           unitSp={
                restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metriques, drop=FALSE],
                                unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
                                              unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs
                                        facteurs[is.element(facteurs, colnames(unitobs))], drop=FALSE],
                                refesp[match(dataMetrique$species.code[!is.na(dataMetrique[ , metrique])],
                                             refesp$species.code),        # ajout des colonnes sélectionnées d'especes
                                       facteurs[is.element(facteurs, colnames(refesp))], drop=FALSE])
            },
           ## Observation table by unitobs, species and size class :
           unitSpSz={
               restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) ,
                                            c(metriques, "size.class"), drop=FALSE],
                               unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
                                             unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs
                                       facteurs[is.element(facteurs, colnames(unitobs))], drop=FALSE],
                               refesp[match(dataMetrique$species.code[!is.na(dataMetrique[ , metrique])],
                                            refesp$species.code),        # ajout des colonnes sélectionnées d'especes
                                      facteurs[is.element(facteurs, colnames(refesp))], drop=FALSE])
           },
           ## Other cases :
           restmp <- cbind(dataMetrique[!is.na(dataMetrique[ , metrique]) , metriques, drop=FALSE],
                           unitobs[match(dataMetrique$observation.unit[!is.na(dataMetrique[ , metrique])],
                                         unitobs$observation.unit), # ajout des colonnes sélectionnées d'unitobs.
                                   facteurs[is.element(facteurs, colnames(unitobs))], drop=FALSE])
           )

    selCol <- which(!is.na(selections))
    if (!is.null(exclude))
    {
        selCol <- selCol[selCol != exclude]
    }

    ## Particular case of size classes :
    if (is.element("size.class", colnames(restmp)))
    {
        if (length(grep("^[[:digit:]]*[-_][[:digit:]]*$", unique(as.character(restmp$size.class)), perl=TRUE)) ==
            length(unique(as.character(restmp$size.class))))
        {
            restmp$size.class <-
                factor(as.character(restmp$size.class),
                       levels=unique(as.character(restmp$size.class))[
                               order(as.numeric(sub("^([[:digit:]]*)[-_][[:digit:]]*$",
                                                    "\\1",
                                                    unique(as.character(restmp$size.class)),
                                                    perl=TRUE)),
                                     na.last=FALSE)])
        }else{
            restmp$size.class <- factor(restmp$size.class)
        }
    }else{}

    ## Biomass and density conversion -> /100m² :
    if (any(is.element(colnames(restmp), c("biomass", "density",
                                           "biomass.max", "density.max",
                                           "biomass.sd", "density.sd"))) && ObsType != "fishing")
    {
        restmp[ , is.element(colnames(restmp),
                             c("biomass", "density",
                               "biomass.max", "density.max",
                               "biomass.sd", "density.sd"))] <- 100 *
                                   restmp[, is.element(colnames(restmp),
                                                       c("biomass", "density",
                                                         "biomass.max", "density.max",
                                                         "biomass.sd", "density.sd"))]
    }else{}

    return(restmp)
}

######################################### end of the function subsetToutesTables.f


######################################### start of the function sortiesLM.f called by modeleLineaireWP2.unitobs.f in FunctExeCalcGLMGalaxy.r
sortiesLM.f <- function(objLM, formule, metrique, factAna, listFact, Data, 
                        Log=FALSE, sufixe=NULL, type="espece")
{
    ## Purpose: Form GLM and LM results
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : lm object
    ##            formule : LM formula
    ##            metrique : Chosen metric
    ##            factAna : separation factor
    ##            listFact : Analysis factors list
    ##            Data : Data used for analysis
    ##            Log : put log on metric ? (boolean)
    ##            sufixe : sufix for file name
    ##            type : analysis type 
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 août 2010, 16:19 modified by Coline ROYAUX 04 june 2020


    ## Add constant if 0 in metric + transformation 'log' :
    if (sum(Data[ , metrique] == 0, na.rm=TRUE) & Log)
    {
        Data[ , metrique] <- Data[ , metrique] +
            ((min(Data[ , metrique], na.rm=TRUE) + 1) / 1000)
    }else{}

    ## readable model formula:
    objLM$call$formula <- formule
    formule <<- formule
    resLM <<- objLM

    resFile <- "GLMSummary.txt"
    ## Global informations and statistics on model :
    infoStatLM.f(objLM=objLM, resFile=resFile)


    ## global model anova + coef significativity :
    signifParamLM.f(objLM=objLM, metrique=metrique, listFact=listFact, resFile=resFile)


    ## ##################################################
    ## values predicted by the model :
    valPreditesLM.f(objLM=objLM, Data=Data, listFact=listFact, resFile=resFile)

    ## ##################################################
    ## multiple comparisons :

    ## if (all(is.element(c("year", "protection.status"), listFact)))
    if (length(listFact) == 2)
    {
 

        ## compMultiplesLM.f(objLM=objLM, Data=Data, factSpatial="protection.status", factTemp="year", resFile=resFile)
        #compMultiplesLM.f(objLM=objLM, Data=Data, fact1=listFact[1], fact2=listFact[2],
         #                 resFile=resFile,Log=Log)

        ## Représentation des interactions :suppr
        
    }else{
        if (length(listFact) == 1)
        {
          #  compSimplesLM.f(objLM=objLM, Data=Data, fact=listFact,
           #                 resFile=resFile, Log=Log)
        }else{}
    }

    # suppr

    ## ##################################################
    ## simple statistics filenames :
    filename <- "GLMSummaryFull.txt"

    ## Save data on model :
    if ( ! isTRUE(sufixe == "(red)"))
    {
        infoStats.f(filename=filename, Data=Data, agregLevel=type, type="stat",
                    metrique=metrique, factGraph=factAna, #factGraphSel=modSel,
                    listFact=listFact)#, listFactSel=listFactSel)
    }else{}

    ## flush.console()
}


######################################### end of the function sortiesLM.f

######################################### start of the function infoStatLM.f called by sortiesLM.f

infoStatLM.f <- function(objLM, resFile)
{
    ## Purpose: Write model's info and global statistic in results files
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : LM or GLM object
    ##            resFile : Results file
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 16:57 modified by Coline ROYAUX 04 june 2020

    sumLM <- switch(class(objLM)[1],
                    lm = summary.lm(objLM),
                    glm = summary.glm(objLM),
                    negbin = MASS:::summary.negbin(objLM),
                    summary(objLM))

    ## Informations on model :
    cat("Fitted model:", file=resFile, fill=1,append=TRUE)
    cat("\t", deparse(objLM$call), "\n\n\n", file=resFile, sep="",append=TRUE)

    ## Global statistics :
    if (length(grep("^glm", objLM$call)) == 0)
    {
        cat("Global Fisher's statistics and R^2:", "\n\n", file=resFile,append=TRUE)
        cat("\t", "Multiple R^2: ", format(sumLM$r.squared, digits=3),
            " ",
            "\t", "Adjusted R^2 ", format(sumLM$adj.r.squared, digits=3), "\n", file=resFile, sep="",append=TRUE)

        cat("\t", "F-statistics:",
            paste(sapply(sumLM$fstatistic, format, digits=4, nsmall=0),
                  " over and DF,"),
            "\t", "P-value: ",
            format.pval(pf(sumLM$fstatistic[1L], sumLM$fstatistic[2L], sumLM$fstatistic[3L], lower.tail = FALSE),
                        digits=4),
            "\n\n\n", file=resFile, sep="",append=TRUE)
    }else{}
}

######################################### end of the function infoStatLM.f

######################################### start of the function signifParamLM.f called by sortiesLM.f

signifParamLM.f <- function(objLM, metrique, listFact, resFile)
{
    ## Purpose: Write Anova results and estimate coef significativity
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : LM or GLM object
    ##            resFile : Results file
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 17:07 modified by Coline ROYAUX 04 june 2020

    ## Anovas et résumés :
    if (length(grep("^glmmTMB", objLM$call)) > 0) #GLMMTMB
    {

        anovaLM <- NULL
        sumLM <- summary(objLM)
        #varstd <- as.data.frame(capture.output(sumLM$varcor)[5:13])
        varstd <- capture.output(sumLM)
        #var <- as.numeric(sumLM$varcor$cond)
        #varstd$var <- c("Variance",var)
        #colnames(varstd) <- rep("",length(colnames(varstd)))

        cat("---------------------------------------------------------------------------", 
            "\nSummary table :",
            "\n\nFamily : ", sumLM$family, ", link : ", sumLM$link,
            "\nResponse : ", metrique,
            "\n\nAIC table : \n\n AIC\tBIC\tlogLik\tdeviance\tdf.resid\n", sumLM$AICtab,
            file=resFile,append=TRUE)
        cat("\n\nAnalysis of Variance table (random effects) :\n",file=resFile,append=TRUE)

        cat("\n",paste(varstd[c(grep("Groups",
                        varstd):grep("Number of obs",
                        varstd))], collapse="\n"),
            "\n",file=resFile,append=TRUE)    

        ## Significativités des paramètres :
        cat("\n\n", "Parameter significances (fixed effects) :", #"\n(only the significant factors/interactions are shown):",
            "\n\n",
            file=resFile,append=TRUE)

        capture.output(printCoefmat.red(sumLM$coef$cond, anovaLM=anovaLM, objLM=objLM), file=resFile,append=TRUE)  
    }else{
        if (length(grep("^glm", objLM$call)) > 0) # GLMs.
        {
            anovaLM <- anova(objLM, test="Chisq") 
       
        }else{
            anovaLM <- anova(objLM) # LMs.
        }

    ## Anova globale du modèle :
    capture.output(print.anova.ml(anovaLM), file=resFile,append=TRUE)
    

    sumLM <- summary(objLM)
    ## Significativités des paramètres :
    cat("\n\n", "Parameter significances :", #"\n(only the significant factors/interactions are shown):",
        "\n\n",
        file=resFile,append=TRUE)

    capture.output(printCoefmat.red(sumLM$coef, anovaLM=anovaLM, objLM=objLM), file=resFile,append=TRUE)
    }  
}

######################################### end of the function signifParamLM.f 

######################################### start of the function print.anova.ml called by signifParamLM.f 

print.anova.ml <- function(x, digits = max(getOption("digits") - 2, 3), signif.stars = getOption("show.signif.stars"),
                           ...)
{
    ## Purpose: Hack of print.anova to supress useless infos
    ## ----------------------------------------------------------------------
    ## Arguments: same as print.anova
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 26 août 2010, 11:36 modified by Coline ROYAUX 04 june 2020

    ## def from initial function :
    if (!is.null(heading <- attr(x, "heading")))
    {
        cat("\n---------------------------------------------------------------------------\n", heading, sep = "\n",append=TRUE)
    }else{}

    nc <- dim(x)[2L]
    if (is.null(cn <- colnames(x)))
    {
        stop("'anova' object must have colnames")
    }else{}
    has.P <- grepl("^(P|Pr)\\(", cn[nc])
    zap.i <- 1L:(if (has.P)
             {
                 nc - 1
             }else{
                 nc
             })
    i <- which(substr(cn, 2, 7) == " value")
    i <- c(i, which(!is.na(match(cn, c("F", "Cp", "Chisq")))))
    if (length(i))
    {
        zap.i <- zap.i[!(zap.i %in% i)]
    }else{}

    tst.i <- i
    if (length(i <- grep("Df$", cn)))
    {
        zap.i <- zap.i[!(zap.i %in% i)]
    }else{}

    printCoefmat(x, digits = digits, signif.stars = signif.stars,
                 signif.legend=FALSE,
                 has.Pvalue = has.P, P.values = has.P, cs.ind = NULL,
                 zap.ind = zap.i, tst.ind = tst.i, na.print = "", ...)
    invisible(x)
}

######################################### end of the function print.anova.ml

######################################### start of the function printCoefmat.red called by signifParamLM.f

printCoefmat.red <- function(x, digits = max(3, getOption("digits") - 2),
                             signif.stars = getOption("show.signif.stars"),
                             signif.legend = signif.stars, dig.tst = max(1, min(5, digits - 1)),
                             cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(0),
                             P.values = NULL,
                             has.Pvalue = nc >= 4 &&
                                          substr(colnames(x)[nc], 1, 3) == "Pr(", eps.Pvalue = .Machine$double.eps,
                             na.print = "NA",
                             anovaLM=NULL,
                             objLM=NULL,
                             ...)
{
    ## Purpose: Modification of printCoefmat to have only z-values
    ##          and p-values
    ## ----------------------------------------------------------------------
    ## Arguments: same as printCoefmat
    ##            + anovaLM : results of global anova 
    ##            objLM : LM or GLM object
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 31 août 2010, 10:46 modified by Coline ROYAUX 04 june 2020

    ## Définitions issues de la fonction originale :
    if (is.null(d <- dim(x)) || length(d) != 2L)
        stop("'x' must be coefficient matrix/data frame")
    nc <- d[2L]
    if (is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if (!is.logical(scp) || is.na(scp)) {
            warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
            scp <- TRUE
        }
        P.values <- has.Pvalue && scp
    }
    else if (P.values && !has.Pvalue)
        stop("'P.values' is TRUE, but 'has.Pvalue' is not")
    if (has.Pvalue && !P.values) {
        d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
        nc <- nc - 1
        has.Pvalue <- FALSE
    }
    else xm <- data.matrix(x)
    k <- nc - has.Pvalue - (if (missing(tst.ind))
        1
    else length(tst.ind))
    if (!missing(cs.ind) && length(cs.ind) > k)
        stop("wrong k / cs.ind")
    Cf <- array("", dim = d, dimnames = dimnames(xm))
    ok <- !(ina <- is.na(xm))
    for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
    if (length(cs.ind)) {
        acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
        if (any(ia <- is.finite(acs))) {
            digmin <- 1 + if (length(acs <- acs[ia & acs != 0]))
                floor(log10(range(acs[acs != 0], finite = TRUE)))
            else 0
            Cf[, cs.ind] <- format(round(coef.se, max(1, digits -
                digmin)), digits = digits)
        }
    }
    if (length(tst.ind))
        Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst),
            digits = digits)
    if (any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind, if (has.Pvalue) nc))))
        for (i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
    okP <- if (has.Pvalue)
        ok[, -nc]
    else ok
    x1 <- Cf[okP]
    dec <- getOption("OutDec")
    if (dec != ".")
        x1 <- chartr(dec, ".", x1)
    x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
    if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
        Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1,
            digits - 1))
    }
    if (any(ina))
        Cf[ina] <- na.print
    if (P.values) {
        if (!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option \"show.signif.stars\" is invalid: assuming TRUE")
            signif.stars <- TRUE
        }
        if (any(okP <- ok[, nc])) {
            pv <- as.vector(xm[, nc])
            Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst,
                eps = eps.Pvalue)
            signif.stars <- signif.stars && any(pv[okP] < 0.1)
            if (signif.stars) {
                Signif <- symnum(pv, corr = FALSE, na = FALSE,
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                  symbols = c("***", "**", "*", ".", " "))
                Cf <- cbind(Cf, format(Signif))
            }
        }
        else signif.stars <- FALSE
    }
    else signif.stars <- FALSE

    ## columns selection :
    Cf <- Cf[ , ncol(Cf) - c(2:0)]

    print.default(Cf, quote = FALSE, right = TRUE, na.print = na.print,
        ...)
    if (signif.stars && signif.legend)
        cat("---\nSignif. codes: ", attr(Signif, "legend"), "\n")
    invisible(x)
}

######################################### end of the function printCoefmat.red

######################################### start of the function valPreditesLM.f called by sortiesLM.f

valPreditesLM.f <- function(objLM, Data, listFact, resFile)
{
    ## Purpose: Write predicted values 
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : LM or GLM object
    ##            Data : data used by the model 
    ##            listFact : list of used factors
    ##            resFile : results file name
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 16:12 modified by Coline ROYAUX 04 june 2020


    ## ##################################################
    ## Predicted values :
    OrdreNivFact <- sapply(unique(Data[ , listFact]), as.numeric)

    if (!is.matrix(OrdreNivFact))       # If only one factor, transform vector of level order in matrix
    {
        OrdreNivFact <- matrix(OrdreNivFact, ncol=1, dimnames=list(NULL, listFact))
    }else{}

    valPredites <- NULL
    ## Predicted values for every factors combination :
    if (length(grep("^glm", objLM$call)) > 0)
    {
        valPredites <- tryCatch(predict(objLM, newdata=unique(Data[ , listFact, drop=FALSE]), type="response"), error=function(e){})
    }else{
        valPredites <- tryCatch(predict(objLM, newdata=unique(Data[ , listFact, drop=FALSE])), error=function(e){})
    }


    if (! is.null(valPredites)) {
        ## Predicted values names (several factor levels combinations) :
        nomCoefs <- unique(apply(Data[ , listFact, drop=FALSE], 1, paste, collapse=":"))
        names(valPredites) <- nomCoefs

        ## put back modalities in order :
        valPredites <- valPredites[eval(parse(text=paste("order(",
                                          paste("OrdreNivFact[ , ", 1:ncol(OrdreNivFact), "]", sep="", collapse=", "),
                                          ")", sep="")))]

        ## Header :
        cat("\n\n\n---------------------------------------------------------------------------",
            "\n", "Values predicted by the model:", "\n\n",
            file=resFile,append=TRUE)

        ## Results :
        capture.output(print(valPredites), file=resFile,append=TRUE)
    }else{
        ## Header :
        cat("\n\n\n---------------------------------------------------------------------------",
            "\n", "Values predicted by the model:", "\n\n",
            file=resFile,append=TRUE)

        ## Error
        cat("\nNo values have been predicted : When using random effect(s), NAs can cause problems in predictions when one or several levels of a factor can only be combined with NAs from other factor \n\n",file=resFile,append=TRUE)
    }

}

######################################### end of the function valPreditesLM.f

######################################### start of the function compMultiplesLM.f called by sortiesLM.f

compMultiplesLM.f <- function(objLM, Data, fact1, fact2, resFile, exclude="", Log=FALSE)
{
    ## Purpose: Compute and write factor levels comparisons (when two factors in model)
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : LM or GLM object
    ##            Data : model's data
    ##            fact1 : First factor's name
    ##            fact2 : Second factor's name
    ##            resFile : results file name
    ##            exclude : ecluded factor
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 oct. 2010, 09:54 modified by Coline ROYAUX 04 june 2020

    facts <- c(fact1, fact2)

    ## header :
    cat("\n\n\n---------------------------------------------------------------------------",
        "\nMultiple comparisons:",
        file=resFile,append=TRUE)

    ## Warnings on comparisons :
    compMultiplesAvertissement.f(objLM=objLM, Log=Log, resFile=resFile)

    ## Test if one factor is temporal :
    tempFact <- is.temporal.f(facts, unitobs)

    ## Compute differences matrix :
    for (i in seq(along=facts))
    {
        ## fact <- get(paste("fact", i, sep=""))
        if (tempFact[i])                # If temporal factor :
        {
            assign(paste("diff", i, sep=""),
                   diffTemporelles.f(objLM=objLM,
                                     factSpatial=facts[-i],
                                     factTemp=facts[i],
                                     Data=Data,
                                     exclude=exclude))
        }else{                          # ... otherwise :
            difftmp <- diffSpatiales.f(objLM=objLM,
                                       factSpatial=facts[i],
                                       factTemp=facts[-i],
                                       Data=Data,
                                       exclude=exclude)
            ## rearrange second factor :
            assign(paste("diff", i, sep=""),
                   difftmp[order(sub("^([^:]+) :.+$", "\\1", row.names(difftmp))), ])
            rm(difftmp)
        }
    }

    ## If coefs can't be calculated, bug with glht... :
    if (any(is.na(coef(objLM))))
    {
        ## Warning :
        cat("\n\n\t",
            "Warning: difference matrices reduced to account for",
            "\n\tnot calculable coefficients (missing data for some levels",
            "\n\tof factors/interactions).", "\n",
            file=resFile,append=TRUE)

        ## Reduce difference matrix :
        diff1 <- diff1[ , !is.na(coef(objLM))]
        diff2 <- diff2[ , !is.na(coef(objLM))]

        objLM$coefficients <- objLM$coefficients[!is.na(coef(objLM))]
    }

    for (i in seq(along=facts))
    {
        ## Status and spatial comparisons results :
        cat(paste("\n\n", "Comparisons for differences in '", facts[i], "' ",
                  ifelse(tempFact[i],
                         paste0("(",
                                "temporal",
                                ") "),
                         ""),
                  "par '", facts[-i], "' ",
                  ifelse(tempFact[-i],
                         paste0("(",
                                "temporal",
                                ") "),
                         ""),
                  ":\n", sep=""),
            file=resFile,append=TRUE)
        #if (all(get(paste("diff", i, sep="") > 0)))
        #{
         #   capture.output(print.summary.glht.red(summary(glht(objLM,
          #                                                     linfct=get(paste("diff", i, sep="")),
           #                                                    alternative="two.sided"))),
            #               file=resFile,append=TRUE)
        #}else{}
    }
}

######################################### end of the function compMultiplesLM.f

######################################### start of the function is.temporal.f called by compMultiplesLM.f and compSimplesLM.f
is.temporal.f <- function(facteur, table)
{
    ## Purpose: test if temporal factor or not
    ## ----------------------------------------------------------------------
    ## Arguments: facteur : factor's name
    ##            table : table where field is present
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 oct. 2010, 10:01 modified by Coline ROYAUX 04 june 2020

    res <- sapply(facteur,
                  function(x)
              {
                  switch(x,
                         year={           # year always temporal
                             TRUE
                         },
                         annee.campagne={      
                             TRUE
                         },
                         geogr.descriptor2={ # Dépend du format.
                             ifelse(all(grepl("^[cC]?[[:digit:]]{4}$",
                                              as.character(table[ , "geogr.descriptor2"])), na.rm=TRUE),
                                    TRUE,
                                    FALSE)
                         },
                         FALSE)
              })
    return(res)
}

######################################### end of the function is.temporal.f

######################################### start of the function compMultiplesAvertissement.f called by compMultiplesLM.f and compSimplesLM.f

compMultiplesAvertissement.f <- function(objLM, Log, resFile)
{
    ## Purpose: Warnings about comparisons
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : GLM or LM object
    ##            Log : data log transformed ? boolean
    ##            resFile : results file name
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 31 janv. 2011, 14:11  modified by Coline ROYAUX 04 june 2020

    cat("\n",
        switch(modelType.f(objLM=objLM, Log=Log),
               "LM"={
                   ""
               },
               "LM-log"={
                   paste("\tWarning: differences are estimated on the logarithms:",
                         "\n\t(log(A) - log(B))", sep="")
               },
               "GLM-NB"={
                   paste("\tWarning: differences estimated in the link space (log):",
                         "\n\tlog(A) - log(B)", sep="")
               },
               "GLM-P"={
                   paste("\tWarning: differences estimated in the link space (log):",
                         "\n\tlog(A) - log(B)", sep="")
               },
               "GLM-B"={
                   paste("\tWarning: differences estimated in the link space (logit):",
                         "", sep="")
               },
               "GLM-Ga"={
                   paste("\tWarning: differences estimated in the link space (inverse):",
                         "\n\t(1/A) - (1/B)\t=>\t*", "inverse the sign of differences*", sep="")
               },
               ""),
        file=resFile,append=TRUE)
}

######################################### end of the function compMultiplesAvertissement.f 

######################################### start of the function modelType.f called by compMultiplesAvertissement.f 

modelType.f <- function(objLM, Log)
{
    ## Purpose: Fournir un prefix décrivant le modèle utilisé.
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : GLM or LM object
    ##            Log : data log transformed ? boolean
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 oct. 2010, 16:29 modified by Coline ROYAUX 04 june 2020

    return(ifelse(length(grep("^lm\\(", deparse(objLM$call), perl=TRUE)) > 0,
                  paste("LM", ifelse(Log, "-log", ""), sep=""),
                  ifelse(length(grep("^glm\\.nb", deparse(objLM$call), perl=TRUE)) > 0,
                         "GLM-NB",
                         ifelse(length(grep("^glm.*poisson", deparse(objLM$call), perl=TRUE)) > 0,
                                "GLM-P",
                                ifelse(length(grep("^glm.*\"binomial\"", deparse(objLM$call), perl=TRUE)) > 0,
                                       "GLM-B",
                                       ifelse(length(grep("family[[:blank:]]*=[[:blank:]]*\"Gamma\"", deparse(objLM$call), perl=TRUE)) > 0,
                                              "GLM-Ga",
                                              "Unknown-model"))))))
}

######################################### end of the function modelType.f 

######################################### start of the function diffTemporelles.f called by compMultiplesLM.f

diffTemporelles.f <- function(objLM, factSpatial, factTemp, Data, exclude)
{
    ## Purpose: Compute temporal differences matrix 
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : GLM or LM object
    ##            factSpatial : spatial factor's name
    ##            factTemp : temporal factor's name
    ##            Data : data of the model
    ##            exclude : excluded factor
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  8 sept. 2010, 11:11 modified by Coline ROYAUX 04 june 2020

    ## Coefficients :
    if (length(grep("^glmmTMB", objLM$call)) > 0)
    {
        theta <- c(levels(objLM$frame[,factTemp]),levels(objLM$frame[,factSpatial]))
    }else{
        theta <- names(coef(objLM))
    }

    tDiff <- paste(c(head(rev(levels(Data[ , factTemp])), 1), head(rev(levels(Data[ , factTemp])),  - 1)),
                   c(tail(rev(levels(Data[ , factTemp])), 1), tail(rev(levels(Data[ , factTemp])),  - 1)),
                   sep=" - ")

    ## Matrix for coef differences :
    Dtemp <- matrix(0,
                    ## As much temporal differences as levels for temporal variable :
                    nrow=nlevels(Data[ , factTemp]) * nlevels(Data[ , factSpatial]),
                    ncol=length(theta))

    ## colnames (not necessary but help for verifications) :

    row.names(Dtemp) <- paste(rep(levels(Data[ , factSpatial]), each=nlevels(Data[ , factTemp])),
                              tDiff,
                              sep=" : ")


    colnames(Dtemp) <- theta

    ## ordonate colnames :
    if (length(grep("^glmmTMB", objLM$call)) > 0)
    {
        namesCol <- c(colnames(Data)[1],
                      colnames(objLM$frame[ , -1]))
    }else{
        namesCol <- c(colnames(Data)[1],
                      colnames(objLM$model[ , -1]))
    }

    namesCol <- namesCol[! is.element(namesCol, exclude)] # excluded column

    ## Compute factor's column count and interactions :
    nlev <- combn(sapply(Data[ , namesCol],
                         function(x)
                     {
                         ifelse(is.factor(x),
                                nlevels(x) - 1,
                                1)
                     }),
                  2)

    ## Columns number per factor type/interaction :
    nCol <- apply(nlev, 2, prod)

    ## Location of first column
    premiereCol <- cumsum(c(1, nCol[- length(nCol)])) + 1

    ## Interest factor's and their interaction's position :
    facts <- c(factSpatial, factTemp)
    posTemp <- 1
    posSpatial <- 2
    #posInteraction <- which(is.element(attr(objLM$terms, "term.labels"),
     #                                  paste(facts, rev(facts), sep=":")))

    ## Différences sur l'effet temporel seul :
    d1 <- rbind(c(-1, rep(0, nCol[posTemp] - 1), 1),
                cbind(0, diag(1, nCol[posTemp])[ , seq(nCol[posTemp], 1)]) +
                cbind(diag(-1, nCol[posTemp])[ , seq(nCol[posTemp], 1)], 0))[ , -1]


    Dtemp[ , seq(from=premiereCol[posTemp],
                 length.out=nCol[posTemp])] <- sapply(as.data.frame(d1), rep, nlevels(Data[ , factSpatial]))


    ## Interactions differences :
#    d2 <- Dtemp[ , seq(from=premiereCol[posInteraction],
 #                       length.out=nCol[posInteraction]), drop=FALSE]

  #  l <- nlevels(Data[ , factTemp]) + 1
   # for (i in seq(from=0, length.out=nCol[posSpatial]))
    #{
     #   if (posSpatial > posTemp)       # traitement différent selon l'imbrication des facteurs :
      #  {                               # Cas où le facteur temporel est en premier :
       #     d2[seq(from=l, length.out=nlevels(Data[ , factTemp])) ,
        #       seq(from=1, length.out=nCol[posTemp]) + i * nCol[posTemp]] <- d1
#        }else{                          #... cas où il est en second :
 #           d2[seq(from=l, length.out=nlevels(Data[ , factTemp])) ,
  #             seq(from=1 + i, by=nCol[posSpatial], length.out=nCol[posTemp])] <- d1
   #     }
#
 #       l <- l + nlevels(Data[ , factTemp])
  #  }
#
 #   Dtemp[ , seq(from=premiereCol[posInteraction],
  #               length.out=nCol[posInteraction])] <- d2

    return(Dtemp)

}

######################################### end of the function diffTemporelles.f

######################################### start of the function diffSpatiales.f called by compMultiplesLM.f

diffSpatiales.f <- function(objLM, factSpatial, factTemp, Data, exclude)
{
    ## Purpose: Compute spatial differences matrix
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : GLM or LM object
    ##            factSpatial : spatial factor name
    ##            factTemp : temporal factor name
    ##            Data : model's data
    ##            exclude : excluded factor
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  7 sept. 2010, 16:15 modified by Coline ROYAUX 04 june 2020

    ## Coefficients :
    if (length(grep("^glmmTMB", objLM$call)) > 0)
    {
        theta <- c(levels(objLM$frame[,factTemp]),levels(objLM$frame[,factSpatial]))
    }else{
        theta <- names(coef(objLM))
    }
    ## Spatial differences name (protection status) :

    sDiff <- apply(combn(unique(Data[ , factSpatial]), 2),
                   2,
                   function(x){paste(rev(x), collapse = " - ")})

    ## Matrix to construct coef differences :
    Dspat <- matrix(0,
                    nrow=nlevels(Data[ , factTemp]) * choose(nlevels(Data[ , factSpatial]), 2),
                    ncol=length(theta))

    ## Colnames (not necessary but help for verifications) :
    #row.names(Dspat) <- paste(levels(Data[ , factTemp]),
     #                         rep(sDiff, each=nlevels(Data[ , factTemp])), sep=" : ")
    colnames(Dspat) <- theta

    ## Ordonate colnames :
    if (length(grep("^glmmTMB", objLM$call)) > 0)
    {
        namesCol <- c(colnames(Data)[1],
                      colnames(objLM$frame[ , -1]))
    }else{
        namesCol <- c(colnames(Data)[1],
                      colnames(objLM$model[ , -1]))
    }

    namesCol <- namesCol[! is.element(namesCol, exclude)] # - excluded column

    ## Compute factor's column count and interactions :
    nlev <- combn(sapply(Data[ , namesCol],
                         function(x)
                     {
                         ifelse(is.factor(x),
                                nlevels(x) - 1,
                                1)
                     }),
                  2)

    ## Columns number per factor type/interaction :
    nCol <- apply(nlev, 2, prod)

    ## Location of first column
    premiereCol <- cumsum(c(1, nCol[- length(nCol)])) + 1

    ## Interest factor's and their interaction's position :
    facts <- c(factSpatial, factTemp)
    posTemp <- 1
    posSpatial <- 2
    #posInteraction <- which(is.element(attr(objLM$terms, "term.labels"),
     #                                  paste(facts, rev(facts), sep=":")))

    ## Differences between status (sans interaction temporelles) :

    tmp <- sapply(as.data.frame(combn(1:nlevels(Data[ , factSpatial]), 2)),
                  function(x)
              {
                  m <- matrix(0,
                              ncol=nlevels(Data[ , factSpatial]),
                              nrow=nlevels(Data[ , factTemp]))
                  m[ , x] <- matrix(c(-1, 1),
                                    nrow=nlevels(Data[ , factTemp]),
                                    ncol=2,
                                    byrow=TRUE)
                  return(m)
              }, simplify=FALSE)

    m <- tmp[[1]][NULL, ]
    for(i in 1:length(tmp))
    {
        m <- rbind(m, tmp[[i]])
    }

    Dspat[ , premiereCol[posSpatial] - 1 + 1:nCol[posSpatial]] <- m[ , -1]

    ## Add interactions :
  #  tmp2 <- Dspat[ , seq(from=premiereCol[posInteraction], length.out=nCol[posInteraction]), drop=FALSE]
#
  #  l <- 1
   # for (i in as.data.frame(combn(0:nCol[posSpatial], 2))) # pour chaque combinaison de statut :
    #{
     #   if(i[1] != 0)
      #  {
       #     d1 <- rbind(0, diag(-1, nrow=nCol[posTemp]))
        #    if (posSpatial > posTemp)   # facteur spatial après le facteur temporel...
         #   {
          #      tmp[seq(from=l, length.out=nlevels(Data[ , factTemp])),
           #          seq(from=(i[1] - 1) * nCol[posTemp] + 1, length.out=nCol[posTemp])] <- d1
            #}else{                      # ... avant le facteur temporel.
             #   tmp[seq(from=l, length.out=nlevels(Data[ , factTemp])),
              #       seq(from=i[1], by=nCol[posSpatial] , length.out=nCol[posTemp])] <- d1
            #}
        #}else{}

      #  d2 <- rbind(0, diag(1, nrow=nCol[posTemp]))

       # if (posSpatial > posTemp)       # facteur spatial après le facteur temporel...
        #{
         #   stop(tmp)
      #      tmp[seq(from=l, length.out=nlevels(Data[ , factTemp])),
       #          seq(from=(i[2] - 1) * nCol[posTemp] + 1, length.out=nCol[posTemp])] <- d2
        #}else{                          # ... avant le facteur temporel.
         #   tmp[seq(from=l, length.out=nlevels(Data[ , factTemp])),
          #       seq(from=i[2], by=nCol[posSpatial], length.out=nCol[posTemp])] <- d2
        #}

     #   l <- l + nlevels(Data[ , factTemp])
    #}
#
 #   ## Stock interactions differences :
  #  Dspat[ , seq(from=premiereCol[posInteraction], length.out=nCol[posInteraction])] <- tmp2

    return(Dspat)
}

######################################### end of the function diffSpatiales.f

######################################### start of the function print.summary.glht.red called by compMultiplesLM.f and compSimplesLM.f

print.summary.glht.red <- function (x, digits = max(3, getOption("digits") - 3), ...)
{
    ## cat("\n\t", "Simultaneous Tests for General Linear Hypotheses\n\n")
    if (!is.null(x$observation.type))
        cat("Multiple comparisons of the means:", x$observation.type,
            "Contrasts", "\n\n\n")
    call <- if (isS4(x$model))
            {
                x$model@call
            }else{
                x$model$call
            }
    ## if (!is.null(call)) {
    ##     cat("Fit: ")
    ##     print(call)
    ##     cat("\n")
    ## }
    cat("\n")
    pq <- x$test
    mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
    error <- attr(pq$pvalues, "error")
    pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==
        0, "z", "t"), ")", sep = ""), greater = paste("Pr(>",
        ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",
        ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
    colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==
        0, "z value", "t value"), pname)
    type <- ifelse(is.null(pq$observation.type) & ! is.null(pq$type), pq$type, pq$observation.type)
    if (!is.null(error) && error > .Machine$double.eps)
    {
        sig <- which.min(abs(1/error - (10^(1:10))))
        sig <- 1/(10^sig)
    }else{
        sig <- .Machine$double.eps
    }
    cat("Linear hypotheses:", "\n")
    alt <- switch(x$alternative, two.sided = "==", less = ">=",
        greater = "<=")
    rownames(mtests) <- paste(rownames(mtests), alt, x$rhs)
    printCoefmat(mtests, digits = digits, has.Pvalue = TRUE,
                 P.values = TRUE, eps.Pvalue = sig)
    switch(type, univariate = cat("(Univariate P-values)"),
        `single-step` = cat("(Adjusted P-values -- 'single step' method)"),
        Shaffer = cat("(Adjusted P-values -- Shaffer method)"),
        Westfall = cat("(Adjusted P-values -- Westfall method)"),
        cat("(Adjusted P-values --", type, "method)"))
    cat("\n\n")
    invisible(x)
}

######################################### end of the function print.summary.glht.red

######################################### start of the function graphTitle.f called by sortiesLM.f

graphTitle.f <- function(metrique, modGraphSel, factGraph, listFact, model=NULL, type="espece",
                         lang = getOption("P.lang"))
{
    ## Purpose: Automatically write a name for a graph
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 14 oct. 2010, 15:44 modified by Coline ROYAUX 04 june 2020
    return(paste(ifelse(is.null(model),
                        "Values of ",
                        paste(model,
                              " for",
                              sep="")),
                 metrique,
                 ifelse(is.element(type, c("espece", "unitobs", "CL_espece", "unitobs(CL)")),
                        paste("aggregated"),
                        ""),
                 switch(type,
                        "espece"=" per species and station",
                        "CL_espece"=" per size class, species and station",
                        "unitobs"=" per station",
                        "unitobs(CL)"=" per station",
                        "CL_unitobs"=" per size class and station",
                        "biodiv"=" per station",
                        ""),
                 switch(type,
                        "espece"={
                            ifelse(modGraphSel == "", # Only separation factor if defined
                                   "",
                                   paste("\nfor the field",
                                         " '", factGraph, "' = ", modGraphSel, sep=""))
                        },
                        "CL_espece"={
                            ifelse(modGraphSel == "", # Only separation factor if defined
                                   "",
                                   paste("\nfor the field",
                                         " '", factGraph, "' = ", modGraphSel, sep=""))
                        },
                        "unitobs"={
                            ifelse(modGraphSel[1] == "", # Only separation factor if defined
                                   "\nfor all species",
                                   paste("\nfor all species matching",
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "unitobs(CL)"={
                            ifelse(modGraphSel[1] == "", # Only separation factor if defined
                                   "\nfor all size classes",
                                   paste("\nfor size classes matching",
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "CL_unitobs"={
                            ifelse(modGraphSel[1] == "", # Only separation factor if defined
                                   "\nfor all species",
                                   paste("\nfor all species matching",
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        "biodiv"={
                            ifelse(modGraphSel[1] == "", # Only separation factor if defined
                                   "",
                                   paste("\nfor stations matching",
                                         " '", factGraph, "' = (",
                                         paste(modGraphSel, collapse=", "), ")", sep=""))
                        },
                        ""),
                 "\n by ",
                 paste(sapply(listFact[length(listFact):1],
                              function(x)paste(c(## varNames.f(x, "article"),
                                                 "",
                                                 x, collapse="")),
                       collapse=" and"),
                 "\n", sep="")))
}

######################################### end of the function graphTitle.f

######################################### start of the function compSimplesLM.f called by sortiesLM.f

compSimplesLM.f <- function(objLM, Data, fact, resFile, Log=FALSE)
{
    ## Purpose: Compute simple comparisons (when only one factor in model)
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : LM or GLM object.
    ##            Data : data to adjust model
    ##            fact : factor's name
    ##            resFile : Results file name
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 oct. 2010, 15:51 modified by Coline ROYAUX 04 june 2020

    ## header :
    cat("\n\n\n---------------------------------------------------------------------------",
        "\n", "Comparisons of levels:",
        file=resFile,append=TRUE)

    ## Warning on comparisons :
    compMultiplesAvertissement.f(objLM=objLM, Log=Log, resFile=resFile)

    if (is.temporal.f(fact, unitobs))
    {
        ## header :
        cat(paste("\n\n\t", "Factor",
                  " '", fact, "' (",
                  "temporal",
                  ") :\n", sep=""),
            file=resFile,append=TRUE)

        ## temporal comparisons :
        compSimple <- glht(objLM,
                           linfct=diffTempSimples.f(objLM=objLM, fact=fact, Data=Data),
                           alternative="two.sided")

        ## write results :
        capture.output(print.summary.glht.red(summary(compSimple)),
                   file=resFile,append=TRUE)

    }else{
        ## header :
        cat(paste("\n\n", "Factor",
                  " '", fact, "' :\n", sep=""),
            file=resFile,append=TRUE)

        ## Tukey, pair comparisons :
        compSimple <- glht(objLM,
                           linfct=eval(parse(text=paste("mcp(", fact, "=\"Tukey\")"))),
                           alternative="two.sided")

        ## write results :
        capture.output(print.summary.glht.red(summary(compSimple)),
                   file=resFile,append=TRUE)
    }
}

######################################### end of the function compSimplesLM.f

######################################### start of the function diffTempSimples.f called by compSimplesLM.f
diffTempSimples.f <- function(objLM, fact, Data)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : GLM or LM object
    ##            factSpatial : spatial factor's name
    ##            Data : model's data
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  4 oct. 2010, 16:28 modified by Coline ROYAUX 04 june 2020

    tDiff <- paste(c(head(rev(levels(Data[ , fact])), 1), head(rev(levels(Data[ , fact])),  - 1)),
                   c(tail(rev(levels(Data[ , fact])), 1), tail(rev(levels(Data[ , fact])),  - 1)),
                   sep=" - ")

    ## Coefficients :
    if (length(grep("^glmmTMB", objLM$call)) > 0)
    {
        theta <- levels(objLM$frame[,fact])
    }else{
        theta <- names(coef(objLM))
    }
    diffDim <- length(theta)

    diffMat <- matrix(0, ncol=diffDim, nrow=diffDim,
                      dimnames=list(tDiff, theta))


    ## temporal effect differences :
    diffMat[ , -1] <- rbind(c(-1, rep(0, diffDim - 2), 1),
                            cbind(0, diag(1, diffDim - 1)[ , seq(diffDim - 1, 1)]) +
                            cbind(diag(-1, diffDim - 1)[ , seq(diffDim - 1, 1)], 0))[ , -1]

    return(diffMat)
}
######################################### end of the function diffTempSimples.f 

######################################### start of the function infoStats.f called by sortiesLM.f

infoStats.f <- function(filename, Data, agregLevel=c("species", "unitobs"), type=c("graph", "stat"),
                        metrique, factGraph, factGraphSel, listFact, listFactSel)
{
    ## Purpose: Écrire les infos et statistic sur les données associées à
    ##          un graphique ou analyse.
    ## ----------------------------------------------------------------------
    ## Arguments: filename : chemin du fichier de résultats.
    ##            Data : données du graphique/de l'analyse.
    ##            agregLevel : niveau d'agrégation de la fonction appelante.
    ##            type : type de fonction appelante (grapique ou analyse).
    ##            metrique : la métrique choisie.
    ##            factGraph : le facteur sélection des espèces.
    ##            factGraphSel : la sélection de modalités pour ce dernier
    ##            listFact : liste du (des) facteur(s) de regroupement
    ##            listFactSel : liste des modalités sélectionnées pour ce(s)
    ##                          dernier(s)
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 10 sept. 2012, 15:26 modified by Coline ROYAUX 04 june 2020

    ## Open file :
    File <- file(description=filename,
                 open="w", encoding="latin1")

    ## if error  :
    on.exit(if (exists("filename") &&
                tryCatch(isOpen(File),
                         error=function(e)return(FALSE))) close(File))

    ## Metrics and factors infos :
    printSelectionInfo.f(metrique=metrique, #factGraph=factGraph, factGraphSel=factGraphSel,
                         listFact=listFact, #listFactSel=listFactSel, 
                         File=File,
                         agregLevel=agregLevel, type=type)

    ## statistics :
    if (class(Data) == "list")
    {
        cat("\n###################################################",
            "\nStatistics per level of splitting factor:\n",
            sep="", file=File,append=TRUE)

        invisible(sapply(1:length(Data),
                         function(i)
                     {
                         printStats.f(Data=Data[[i]], metrique=metrique, listFact=listFact, File=File,
                                      headline=factGraphSel[i])
                     }))
    }else{
        printStats.f(Data=Data, metrique=metrique, listFact=listFact, File=File,
                     headline=NULL)
    }

    ## Close file :
    close(File)

}

######################################### end of the function infoStats.f


######################################### start of the function printSelectionInfo.f called by infoStats.f

printSelectionInfo.f <- function(metrique, listFact, 
                                 File,
                                 agregLevel=c("species", "unitobs"), type=c("graph", "stat"))
{
    ## Purpose: Write data informations
    ## ----------------------------------------------------------------------
    ## Arguments: metrique : chosen metric
    ##            listFact : factor's list
    ##            File : Results file name
    ##            agregLevel : aggregation level
    ##            type : function type 
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 10:41 modified by Coline ROYAUX 04 june 2020

    cat("\n##################################################\n",
        "Metrics and factors (and possible units/selections):\n",
        sep="", file=File,append=TRUE)

    ## metric info :
    cat("\n Metrics:", metrique,
        "\n", file=File,append=TRUE)

    ## aggregation level :
    cat("            aggregated per ",
        switch(agregLevel,
               "CL_espece"=,"CL_unitobs"=,"spCL_unitobs"=,"spCL_espece"={
                   "size class / "
               }),
        switch(agregLevel,
               "CL_espece"=,"spCL_espece"=,"species"=,"spSpecies"=,"spEspece"={
                   "species / "
               }),
        switch(agregLevel,
               "spUnitobs"=,"spCL_unitobs"=,"spCL_espece"=,"spUnitobs(CL)"=,"spSpecies"=,"spEspece"={
                   paste(listFact, " (mean over ", sep="")
              }),
        "observation units",
        switch(agregLevel,
               "spUnitobs"=,"spCL_unitobs"=,"spCL_espece"=,"spUnitobs(CL)"=,"spSpecies"=,"spEspece"={
                   ")"
              }),
        ".\n",
        sep="", file=File,append=TRUE)

    ## Separation factors :
#    switch(agregLevel,
 #          "species"=,"CL_espece"=,"espece"={ # Adapté également pour les LMs.
  #             cat("\n",
   #                switch(type,
    #                      "graph"="Graphics separation factor",
     #                     "stat"="Analyses separation factor"),
      #             " : ",
       #            ifelse(factGraph == "", "printSelectionInfo.f.11",
        #                  ifelse(is.na(factGraphSel[1]),
         #                        paste(varNames.f(factGraph, "nom"), "none!"),
          #                       paste(varNames.f(factGraph, "nom"), " (",
           #                            paste(factGraphSel, collapse=", "), ")", sep=""))), "\n",
            #       sep="", file=File,append=TRUE)
#           },
 #          "unitobs"=,"CL_unitobs"=,"unitobs(CL)"=,"spUnitobs"={
  #             cat("(warning: no selection!!!)",
   #                ifelse(factGraph == "", "\nSelection factor for aggregation of observations: ",
    #                      ifelse(is.na(factGraphSel[1]),
     #                            paste(varNames.f(factGraph, "nom"), "none (all species/size classes)!"),
      #                           paste(varNames.f(factGraph, "nom"), " (",
       #                                paste(factGraphSel, collapse=", "), ")", sep=""))), "\n",
        #           sep="", file=File,append=TRUE)
         #  })

    ## Clustering factors :
    if (is.element(agregLevel, c("spCL_unitobs", "spCL_espece", "spSpecies", "spEspece",
                                 "spUnitobs", "spUnitobs(CL)"))) {type <- "spatialGraph"}

    cat(switch(type,
               "graph"="\nGrouping factor(s): \n * ",
               "stat"="\nAnalyses factor(s): \n * ",
               "spatialGraph"="\nSpatial aggregation factor(s): \n * "),
        paste(listFact,collaspe="\n * "),"\n",file=File,append=TRUE)

#    invisible(sapply(1:length(listFact),
 #                    function(i)
  #               {
   #                  cat("\n  * ",
    #                     ifelse(is.na(listFactSel[[i]][1]),
     #                                  paste(varNames.f(listFact[i], "nom"), "(no selection)"),
      #                                 paste(varNames.f(listFact[i], "nom"), " (",
       #                                      paste(listFactSel[[i]], collapse=", "), ")", sep="")), "\n",
        #                 sep="", file=File,append=TRUE)
         #        }))
}

######################################### end of the function printSelectionInfo.f


######################################### start of the function printStats.f called by infoStats.f

printStats.f <- function(Data, metrique, listFact, File, headline=NULL)
{
    ## Purpose: Write general statistics table
    ## ----------------------------------------------------------------------
    ## Arguments: Data : Analysis data
    ##            metrique : metric's name 
    ##            listFact : Factor's list
    ##            File : Simple statistics file name
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 11 sept. 2012, 10:09 modified by Coline ROYAUX 04 june 2020

    ## Header :
    if ( ! is.null(headline))
    {
        cat("\n", rep("#", nchar(headline) + 3), "\n",
            "## ", headline, "\n",
            sep="", file=File,append=TRUE)
    }else{}

    cat("\n########################\nBase statistics:\n\n", file=File,append=TRUE)

    capture.output(print(summary.fr(Data[ , metrique])), file=File, append=TRUE)

    if ( ! is.null(listFact))
    {
        cat("\n#########################################",
            "\nStatistics per combination of factor levels:\n\n", file=File, sep="",append=TRUE)

        ## Compute summary for each existing factor's cross :
        res <- with(Data,
                    tapply(eval(parse(text=metrique)),
                           INDEX=do.call(paste,
                                         c(lapply(listFact,
                                                  function(y)eval(parse(text=y))),
                                           sep=".")),
                           FUN=summary.fr))

        ## results in table
        capture.output(print(do.call(rbind, res)),
                       file=File, append=TRUE)
    }else{}

    ## empty line :
    cat("\n", file=File,append=TRUE)
}

######################################### end of the function printStats.f


######################################### start of the function summary.fr called by printStats.f
summary.fr <- function(object, digits = max(3, getOption("digits") - 3),...)
{
    ## Purpose: Adding SD and N to summary
    ## ----------------------------------------------------------------------
    ## Arguments: object : Object to summarise
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 13 sept. 2012, 15:47 modified by Coline ROYAUX 04 june 2020

    if ( ! is.numeric(object)) stop("Programming error")

    ## Compute summary :
    res <- c(summary(object=object, digits, ...), "sd"=signif(sd(x=object), digits=digits), "N"=length(object))

    return(res)
}

######################################### start of the function summary.fr



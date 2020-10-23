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
create.unitobs <- function(data,year="year",location="location", unitobs="observation.unit")
{
    if (is.element(paste(unitobs),colnames(data)) )
    {
            unitab <- data
    }else{ 

        unitab <- unite(data,col="observation.unit",c(year,location))
    }
    return(unitab)
}
######################################### start of the function create.unitobs

######################################### start of the function create.year.location called by FunctExeCalcCommIndexesGalaxy.r and FunctExeCalcPresAbsGalaxy.r
####### separate unitobs column when existant
create.year.location <- function(data,year="year",location="location", unitobs="observation.unit")
{
    if (all(grepl("[1-2][0|8|9][0-9]{2}_.*",data[,unitobs]))==TRUE)
    {
        tab <- separate(data,col=unitobs,into=c(year,location),sep="_")
    }else{
        if (all(grepl("[A-Z]{2}[0-9]{2}.*",data[,unitobs])==TRUE))
        {
            tab <- separate(data,col=unitobs,into=c("site1", year,"obs"),sep=c(2,4))
            tab <- unite(tab, col=location, c("site1","obs"))
        }else{
            tab <- data
        }
    }

    tab <- cbind(tab,observation.unit = data[,unitobs])

    return(tab)
}
######################################### start of the function create.year.location

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


######################################### start of the function create.res.table called by modeleLineaireWP2.xxx.f in FunctExeCalcGLMxxGalaxy.r
create.res.table <- function(listRand, listFact, row, lev, distrib)
{
    ## Purpose: create results table
    ## ----------------------------------------------------------------------
    ## Arguments: listRand : Analysis random factors list    
    ##            listFact : Analysis factors list
    ##            row : rows of results table = species or separation factor
    ##            lev : Levels of analysis factors list
    ##            distrib : distribution law
    ## ----------------------------------------------------------------------
    ## Author: Coline ROYAUX 04 october 2020

    if (listRand[1] != "None") ## if random effects
    {
        TabSum <- data.frame(analysis=row,Interest.var=NA,distribution=NA,AIC=NA,BIC=NA,logLik=NA, deviance=NA,df.resid=NA)
        colrand <- unlist(lapply(listRand, 
                           FUN=function(x){lapply(c("Std.Dev","NbObservation","NbLevels"),
                                                  FUN=function(y){paste(x,y,collapse = ":")
                                                                 })
                                          }))
        TabSum[,colrand] <- NA

        if (! is.null(lev)) ## if fixed effects + random effects
        {
            colcoef <- unlist(lapply(c("(Intercept)",lev),
                               FUN=function(x){lapply(c("Estimate","Std.Err","Zvalue","Pvalue","IC_up","IC_inf","signif"),
                                                      FUN=function(y){paste(x,y,collapse = ":")
                                                                     })
                                              }))

        }else{ ## if no fixed effects
            colcoef <- NULL
        }

    }else{ ## if no random effects
        TabSum <- data.frame(analysis=row,Interest.var=NA,distribution=NA,AIC=NA,Resid.deviance=NA,df.resid=NA,Null.deviance=NA,df.null=NA)

        switch(distrib,
               "gaussian"={colcoef <- unlist(lapply(c("(Intercept)",lev),
                                             FUN=function(x){lapply(c("Estimate","Std.Err","Tvalue","Pvalue","IC_up","IC_inf","signif"),
                                                                    FUN=function(y){paste(x,y,collapse = ":")
                                                                                   })
                                                            }))

                           },
               "quasipoisson"={colcoef <- unlist(lapply(c("(Intercept)",lev),
                                             FUN=function(x){lapply(c("Estimate","Std.Err","Tvalue","Pvalue","IC_up","IC_inf","signif"),
                                                                    FUN=function(y){paste(x,y,collapse = ":")
                                                                                   })
                                                            }))

                               },
               {colcoef <- unlist(lapply(c("(Intercept)",lev),
                                        FUN=function(x){lapply(c("Estimate","Std.Err","Zvalue","Pvalue","IC_up","IC_inf","signif"),
                                                               FUN=function(y){paste(x,y,collapse = ":")
                                                                              })
                                                       }))
                })

    }
  
    TabSum[,colcoef] <- NA


    return(TabSum)
}
######################################### end of the function create.res.table 

######################################### start of the function sortiesLM.f called by modeleLineaireWP2.unitobs.f in FunctExeCalcGLMGalaxy.r
sortiesLM.f <- function(objLM, objLMY, TabSum, #formule, 
                        metrique, factAna, cut, colAna, listFact, lev = NULL, Data, 
                        Log=FALSE, sufixe=NULL)
{
    ## Purpose: Form GLM and LM results
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : lm object
    ##            objLMY : lm object with year as continuous
    ##            TabSum : output summary table
    ##            formule : LM formula
    ##            metrique : Chosen metric
    ##            factAna : separation factor
    ##            cut : level of separation factor
    ##            colAna : colname for separation factor in output summary table
    ##            listFact : Analysis factors list
    ##            levels : Levels of analysis factors list
    ##            Data : Data used for analysis
    ##            Log : put log on metric ? (boolean)
    ##            sufixe : sufix for file name
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 août 2010, 16:19 modified by Coline ROYAUX 04 june 2020

    TabSum[,"Interest.var"] <- as.character(metrique)#rep(,nrow(TabSum))
    sumLM <- summary(objLM)
    TabSum[,"distribution"] <- as.character(sumLM$family[1])#rep(,nrow(TabSum))

    if (length(grep("^glmmTMB", objLM$call)) > 0) #if random effects
    {
        TabSum[TabSum[,colAna]==cut,"AIC"] <- sumLM$AICtab[1]
        TabSum[TabSum[,colAna]==cut,"BIC"] <- sumLM$AICtab[2]
        TabSum[TabSum[,colAna]==cut,"logLik"] <- sumLM$AICtab[3]
        TabSum[TabSum[,colAna]==cut,"deviance"] <- sumLM$AICtab[4]  
        TabSum[TabSum[,colAna]==cut,"df.resid"] <- sumLM$AICtab[5]

        if (! is.null(lev)) ## if fixed effects + random effects
        {
            TabCoef <- as.data.frame(sumLM$coefficients$cond)
            TabCoef$signif <- lapply(TabCoef[,"Pr(>|z|)"],FUN=function(x){if(!is.na(x) && x < 0.05){"yes"}else{"no"}})

            TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Zvalue",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"z value"]
            TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Pvalue",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"Pr(>|z|)"]

            TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Zvalue",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"z value"]}else{NA}}))
            TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Pvalue",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"Pr(>|z|)"]}else{NA}}))

            if (any(objLMY != ""))
            {
                sumLMY <- summary(objLMY)
                TabCoefY <- as.data.frame(sumLMY$coefficients$cond)
                TabCoefY$signif <- lapply(TabCoefY[,"Pr(>|z|)"],FUN=function(x){if(!is.na(x) && x < 0.05){"yes"}else{"no"}})
                TabSum[TabSum[,colAna]==cut,"year Zvalue"] <- ifelse(length(TabCoefY["year","z value"]) > 0,TabCoefY["year","z value"],NA)
                TabSum[TabSum[,colAna]==cut,"year Pvalue"] <- ifelse(length(TabCoefY["year","Pr(>|z|)"]) > 0,TabCoefY["year","Pr(>|z|)"],NA)
            }else{}

        }else{}

        switch(as.character(length(sumLM$varcor$cond)),
               "1"={StdD <- c(sumLM$varcor$cond[[1]])},
               "2"={StdD <- c(sumLM$varcor$cond[[1]],sumLM$varcor$cond[[2]])},
               StdD <- NULL)

        TabSum[TabSum[,colAna]==cut,grepl(paste(listRand,"Std.Dev",collapse="|"),colnames(TabSum))] <- StdD
        TabSum[TabSum[,colAna]==cut,grepl(paste(listRand,"NbObservation",collapse="|"),colnames(TabSum))] <- sumLM$nobs
        TabSum[TabSum[,colAna]==cut,grepl(paste(listRand,"NbLevels",collapse="|"),colnames(TabSum))] <- unlist(lapply(listRand,FUN=function(x){nlevels(Data[,x])}))

    }else{ ## if fixed effects only

        TabSum[TabSum[,colAna]==cut,"AIC"] <- sumLM$aic
        TabSum[TabSum[,colAna]==cut,"Resid.deviance"] <- sumLM$deviance
        TabSum[TabSum[,colAna]==cut,"df.resid"] <- sumLM$df.residual
        TabSum[TabSum[,colAna]==cut,"Null.deviance"] <- sumLM$null.deviance
        TabSum[TabSum[,colAna]==cut,"df.null"] <- sumLM$df.null
        TabCoef <- as.data.frame(sumLM$coefficients)

        if (any(objLMY != ""))
        {
            sumLMY <- summary(objLMY)
            TabCoefY <- as.data.frame(sumLMY$coefficients)
        }else{}

        if (sumLM$family[1] == "gaussian" || sumLM$family[1] == "quasipoisson") 
        {

            TabCoef$signif <- lapply(TabCoef[,"Pr(>|t|)"],FUN=function(x){if(!is.na(x) && x < 0.05){"yes"}else{"no"}})
            TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Tvalue",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"t value"]
            TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Pvalue",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"Pr(>|t|)"]

            TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Tvalue",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"t value"]}else{NA}}))

            TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Pvalue",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"Pr(>|t|)"]}else{NA}}))

            if (any(objLMY != ""))
            {
                TabCoefY$signif <- lapply(TabCoefY[,"Pr(>|t|)"],FUN=function(x){if(!is.na(x) && x < 0.05){"yes"}else{"no"}})
                TabSum[TabSum[,colAna]==cut,"year Tvalue"] <- ifelse(length(TabCoefY["year","t value"]) > 0,TabCoefY["year","t value"],NA)
                TabSum[TabSum[,colAna]==cut,"year Pvalue"] <- ifelse(length(TabCoefY["year","Pr(>|z|)"]) > 0,TabCoefY["year","Pr(>|t|)"],NA)             
            }else{}

        }else{
            TabCoef$signif <- lapply(TabCoef[,"Pr(>|z|)"],FUN=function(x){if(!is.na(x) && x < 0.05){"yes"}else{"no"}})

            TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Zvalue",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"z value"]
            TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Pvalue",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"Pr(>|z|)"]
            
            TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Zvalue",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"z value"]}else{NA}}))
            TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Pvalue",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"Pr(>|z|)"]}else{NA}}))

            if (any(objLMY != ""))
            {
                TabCoefY$signif <- lapply(TabCoefY[,"Pr(>|z|)"],FUN=function(x){if(!is.na(x) && x < 0.05){"yes"}else{"no"}})

                TabSum[TabSum[,colAna]==cut,"year Zvalue"] <- ifelse(length(TabCoefY["year","z value"]) > 0,TabCoefY["year","z value"],NA)
                TabSum[TabSum[,colAna]==cut,"year Pvalue"] <- ifelse(length(TabCoefY["year","Pr(>|z|)"]) > 0,TabCoefY["year","Pr(>|z|)"],NA)
            }else{}
        }
    }

    if (! is.null(lev)) ## if fixed effects
    {
        TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Estimate",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"Estimate"]
        TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Std.Err",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"Std. Error"]
        TabSum[TabSum[,colAna]==cut,grepl("Intercept.*signif",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"signif"]

        TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Estimate",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"Estimate"]}else{NA}}))
        TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Std.Err",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"Std. Error"]}else{NA}}))
        TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"signif",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"signif"]}else{NA}})) 

        if (any(objLMY != ""))
        {
            TabSum[TabSum[,colAna]==cut,"year Estimate"] <- ifelse(length(TabCoefY["year","Estimate"]) > 0,TabCoefY["year","Estimate"],NA)
            TabSum[TabSum[,colAna]==cut,"year Std.Err"] <- ifelse(length(TabCoefY["year","Std. Error"]) > 0,TabCoefY["year","Std. Error"],NA)
            TabSum[TabSum[,colAna]==cut,"year signif"] <- ifelse(length(TabCoefY["year","signif"]) > 0,TabCoefY["year","signif"],NA)
        }else{}

    }else{}

    IC <- tryCatch(as.data.frame(confint(objLM)), error=function(e){})

    TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"IC_up",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(IC))) > 0) {IC[grepl(x,rownames(IC)),"97.5 %"]}else{NA}})) 
    TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"IC_inf",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(IC))) > 0) {IC[grepl(x,rownames(IC)),"2.5 %"]}else{NA}})) 

    return(TabSum)
   
}


######################################### end of the function sortiesLM.f

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

######################################### start of the function noteGLM.f called by modeleLineaireWP2.species.f and modeleLineaireWP2.unitobs.f

noteGLM.f <- function(data, objLM, metric, listFact, details = FALSE)
{
    ## Purpose: Note your GLM analysis
    ## ----------------------------------------------------------------------
    ## Arguments: data : Dataframe used for analysis
    ##            objLM : GLM assessed
    ##            metric : selected metric
    ##            listFact : Analysis factors list
    ##            details : detailed output ?
    ## ----------------------------------------------------------------------
    ## Author: Coline ROYAUX, 26 june 2020

    rate <- 0
    detres <- list(complete_plan=NA, balanced_plan=NA, NA_proportion_OK=NA, no_residual_dispersion=NA, uniform_residuals=NA, outliers_proportion_OK=NA, no_zero_inflation=NA, observation_factor_ratio_OK=NA, enough_levels_random_effect=NA, rate=NA)

    #### Data criterions ####
    
    ## Plan

    plan <- as.data.frame(table(data[,listFact]))

    if (nrow(plan[plan$Freq==0,]) < nrow(plan)*0.1)    # +0.5 if less than 10% of possible factor's level combinations aren't represented in the sampling scheme
    { 
        rate <- rate + 0.5 
        detres$complete_plan <- TRUE

        if (summary(as.factor(plan$Freq))[1] > nrow(plan)*0.9)  # +0.5 if the frequency of the most represented frequency of possible factor's levels combinations is superior to 90% of the total number of possible factor's levels combinations
        { 
            rate <- rate + 0.5
            detres$balanced_plan <- TRUE
        }else{}

    }else{
        detres$complete_plan <- FALSE
        detres$balanced_plan <- FALSE
    }  

    if (nrow(data) - nrow(na.omit(data)) < nrow(data)*0.1) # +1 if less than 10% of the lines in the dataframe bares a NA 
    {
        rate <- rate + 1
        detres$NA_proportion_OK <- TRUE
    }else{
        detres$NA_proportion_OK <- FALSE
    }

    #### Model criterions ####

    if (length(grep("quasi",objLM$family)) == 0) #DHARMa doesn't work with quasi distributions
    {
 
        Residuals <- simulateResiduals(objLM)

        capture.output(testRes <- testResiduals(Residuals))
        testZero <- testZeroInflation(Residuals)

        ## dispersion of residuals

        if (testRes$dispersion$p.value > 0.05) # +1.5 if dispersion tests not significative 
        {
            rate <- rate + 1.5
            detres$no_residual_dispersion <- TRUE
        }else{
            detres$no_residual_dispersion <- FALSE
        }

        ## uniformity of residuals

        if (testRes$uniformity$p.value > 0.05) # +1 if uniformity tests not significative 
        {
            rate <- rate + 1.5
            detres$uniform_residuals <- TRUE
        }else{
            detres$uniform_residuals <- FALSE
        }

        ## residuals outliers
    
        if (testRes$outliers$p.value > 0.05) # +0.5 if outliers tests not significative 
        {
            rate <- rate + 0.5
            detres$outliers_proportion_OK <- TRUE
        }else{
            detres$outliers_proportion_OK <- FALSE
        }

        ## Zero inflation test

        if (testZero$p.value > 0.05) # +1 if zero inflation tests not significative 
        {
            rate <- rate + 1.5
            detres$no_zero_inflation <- TRUE
        }else{
            detres$no_zero_inflation <- FALSE
        }

        ## Factors/observations ratio

        if (length(listFact)/nrow(na.omit(data)) < 0.1) # +1 if quantity of factors is less than 10% of the quantity of observations
        {
            rate <- rate + 1
            detres$observation_factor_ratio_OK <- TRUE
        }else{
            detres$observation_factor_ratio_OK <- FALSE
        }

        ## less than 10 factors' level on random effect

        if (length(grep("^glmmTMB", objLM$call)) > 0)
        {
            nlevRand <- c()
            for(fact in names(summary(objLM)$varcor$cond))
            {
                nlevRand <- c(nlevRand,length(unlist(unique(data[,fact]))))
            }
 
            if (all(nlevRand > 10)) # +1 if more than 10 levels in one random effect 
            {
                rate <- rate + 1
                detres$enough_levels_random_effect <- TRUE
            }else{
                detres$enough_levels_random_effect <- FALSE
            }
        }else{}

        detres$rate <- rate

        if (details) 
        {
            return(detres)   
        }else{
            return(rate)
        }

    }else{
        return(NA) 
        cat("Models with quasi distributions can't be rated for now")
    }
}

######################################### end of the function noteGLM.f

######################################### start of the function noteGLMs.f called by modeleLineaireWP2.species.f and modeleLineaireWP2.unitobs.f

noteGLMs.f <- function(tabRate, exprML, objLM, file_out=FALSE)
{
    ## Purpose: Note your GLM analysis
    ## ----------------------------------------------------------------------
    ## Arguments: tabRate : rates table from noteGLM.f
    ##            exprML : GLM expression assessed
    ##            objLM : GLM object
    ##            file_out : Output as file ? else global rate only
    ## ----------------------------------------------------------------------
    ## Author: Coline ROYAUX, 26 june 2020

    RateM <- median(na.omit(tabRate[,"rate"]))
    sum <- summary(objLM)

    if (length(grep("^glmmTMB", objLM$call)) > 0)
    {
        if (median(na.omit(tabRate[,"rate"])) >= 6) # if 50% has a rate superior or equal to 6 +1
        {
            RateM <- RateM + 1
        } 

        if (quantile(na.omit(tabRate[,"rate"]), probs=0.9) >= 6) # if 90% has a rate superior or equal to 6 +1
        {
            RateM <- RateM + 1
        } 
    }else{
        if (median(na.omit(tabRate[,"rate"])) >= 5) # if 50% has a rate superior or equal to 5 +1
        {
            RateM <- RateM + 1
        } 

        if (quantile(na.omit(tabRate[,"rate"]), probs=0.9) >= 5) # if 90% has a rate superior or equal to 5 +1
        {
            RateM <- RateM + 1
        } 
    }

    if (file_out)
    {
        namefile <- "RatingGLM.txt"

        cat("###########################################################################",
            "\n########################### Analysis evaluation ###########################",
            "\n###########################################################################", file=namefile, fill=1,append=TRUE)

        ## Informations on model :
        cat("\n\n######################################### \nFitted model:", file=namefile, fill=1,append=TRUE)
        cat("\t", deparse(exprML), "\n\n", file=namefile, sep="",append=TRUE)
        cat("Family: ", sum$family[[1]], 
            file=namefile,append=TRUE)
        cat("\n\nNumber of analysis: ", nrow(tabRate), file=namefile, append=TRUE)

        ## Global rate : 
        cat("\n\n######################################### \nGlobal rate for all analysis:", 
            "\n\n", RateM, "out of 10", file=namefile, append=TRUE)

        ## details on every GLM : 
#NA_proportion_OK=NA, no_residual_dispersion=NA, uniform_residuals=NA, outliers_proportion_OK=NA, no_zero_inflation=NA, observation_factor_ratio_OK=NA, enough_levels_random_effect=NA, rate=NA
        cat("\n\n######################################### \nDetails on every analysis:\n\n", file=namefile, append=TRUE)
        cat("Analysis\tC1\tC2\tC3\tC4\tC5\tC6\tC7\tC8\tC9\tFinal rate", file=namefile, append=TRUE)
        apply(tabRate, 1, FUN=function(x)
                              {

                                  if (!is.na(x["complete_plan"]) && x["complete_plan"]==TRUE)
                                  {
                                      cat("\n",x[1],"\tyes", file=namefile, append=TRUE)
                                  }else{
                                      cat("\n",x[1],"\tno", file=namefile, append=TRUE)
                                  }

                                  for (i in c("balanced_plan","NA_proportion_OK", "no_residual_dispersion", "uniform_residuals", "outliers_proportion_OK", "no_zero_inflation", "observation_factor_ratio_OK", "enough_levels_random_effect"))
                                  { 
                                      if (!is.na(x[i]) && x[i]==TRUE)
                                      {
                                          cat("\tyes", file=namefile, append=TRUE)
                                      }else{
                                          cat("\tno", file=namefile, append=TRUE)
                                      }
                                  }
                                  
                                  cat("\t",x["rate"], "/ 8", file=namefile, append=TRUE)

                                             
                              })
        cat("\n\nC1: Complete plan?\nC2: Balanced plan?\nC3: Few NA?\nC4: Regular dispersion?\nC5: Uniform residuals?\nC6: Regular outliers proportion?\nC7: No zero-inflation?\nC8: Good observation/factor ratio?\nC9: Enough levels on random effect?", file=namefile, append=TRUE)

        ## Red flags - advice :
        cat("\n\n######################################### \nRed flags - advice:\n\n", file=namefile, append=TRUE)
        if (all(na.omit(tabRate["NA_proportion_OK"]) == FALSE))
        {
            cat("\n","\t- More than 10% of lines of your dataset contains NAs", file=namefile, append=TRUE)
        }else{}

        if (length(grep("FALSE",tabRate["no_residual_dispersion"])) / length(na.omit(tabRate["no_residual_dispersion"])) > 0.5)
        {
            cat("\n","\t- More than 50% of your analyses are over- or under- dispersed : Try with another distribution family", file=namefile, append=TRUE)
        }else{}

        if (length(grep("FALSE",tabRate["uniform_residuals"])) / length(na.omit(tabRate["uniform_residuals"])) > 0.5)
        {
            cat("\n","\t- More than 50% of your analyses haven't an uniform distribution of residuals : Try with another distribution family", file=namefile, append=TRUE)
        }else{}

        if (length(grep("FALSE",tabRate["outliers_proportion_OK"])) / length(na.omit(tabRate["outliers_proportion_OK"])) > 0.5)
        {
            cat("\n","\t- More than 50% of your analyses have too much outliers : Try with another distribution family or try to select or filter your data", file=namefile, append=TRUE)
        }else{}

        if (length(grep("FALSE",tabRate["no_zero_inflation"])) / length(na.omit(tabRate["no_zero_inflation"])) > 0.5)
        {
            cat("\n","\t- More than 50% of your analyses have zero inflation : Try to select or filter your data", file=namefile, append=TRUE)
        }else{}

        if (length(grep("FALSE",tabRate["observation_factor_ratio_OK"])) / length(na.omit(tabRate["observation_factor_ratio_OK"])) > 0.5)
        {
            cat("\n","\t- More than 50% of your analyses have not enough observations for the amount of factors : Try to use less factors in your analysis or try to use another separation factor", file=namefile, append=TRUE)
        }else{}

        if (any(tabRate["enough_levels_random_effect"] == FALSE, na.rm=TRUE) && length(grep("^glmmTMB", objLM$call)) > 0)
        {
            cat("\n","\t- Random effect hasn't enough levels to be robust : If it has less than ten levels remove the random effect", file=namefile, append=TRUE)
        }else{}
    }else{

    return(RateM)

    }
}

######################################### end of the function noteGLM.f

######################################### start of the function infoStats.f called by modeleLineaireWP2.species.f and modeleLineaireWP2.unitobs.f

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



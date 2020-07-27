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
    if (is.element(paste(unitobs),colnames(data)) && all(grepl("[1-2][0|8|9][0-9]{2}_.*",data[,unitobs])==FALSE))
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
        tab <- separate(data,col=unitobs,into=c("site1", year,"obs"),sep=c(2,4))
        tab <- unite(tab, col=location, c("site1","obs"))

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


######################################### start of the function sortiesLM.f called by modeleLineaireWP2.unitobs.f in FunctExeCalcGLMGalaxy.r
sortiesLM.f <- function(objLM, TabSum, #formule, 
                        metrique, factAna, cut, colAna, listFact, lev = NULL, Data, 
                        Log=FALSE, sufixe=NULL, type="espece")
{
    ## Purpose: Form GLM and LM results
    ## ----------------------------------------------------------------------
    ## Arguments: objLM : lm object
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
    ##            type : analysis type 
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 25 août 2010, 16:19 modified by Coline ROYAUX 04 june 2020

    sumLM <- summary(objLM)
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

        if (sumLM$family[1] == "gaussian" || sumLM$family[1] == "quasipoisson") 
        {

            TabCoef$signif <- lapply(TabCoef[,"Pr(>|t|)"],FUN=function(x){if(!is.na(x) && x < 0.05){"yes"}else{"no"}})
            TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Tvalue",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"t value"]
            TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Pvalue",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"Pr(>|t|)"]

            TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Tvalue",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"t value"]}else{NA}}))

            TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Pvalue",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"Pr(>|t|)"]}else{NA}}))
          }else{
            TabCoef$signif <- lapply(TabCoef[,"Pr(>|z|)"],FUN=function(x){if(!is.na(x) && x < 0.05){"yes"}else{"no"}})

            TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Zvalue",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"z value"]
            TabSum[TabSum[,colAna]==cut,grepl("Intercept.*Pvalue",colnames(TabSum))] <- TabCoef[grepl("Intercept",rownames(TabCoef)),"Pr(>|z|)"]
            
            TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Zvalue",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"z value"]}else{NA}}))
            TabSum[TabSum[,colAna]==cut,grepl(paste(lev,"Pvalue",collapse="|"),colnames(TabSum))] <- unlist(lapply(lev,FUN=function(x){if (length(grep(x,rownames(TabCoef))) > 0) {TabCoef[grepl(x,rownames(TabCoef)),"Pr(>|z|)"]}else{NA}}))
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
    }else{}

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

######################################### Package DHARMa

################ simulateResiduals.R

#' Create simulated residuals
#'
#' The function creates scaled residuals by simulating from the fitted model. Residuals can be extracted with \code{\link{residuals.DHARMa}}. See \code{\link{testResiduals}} for an overview of residual tests, \code{\link{plot.DHARMa}} for an overview of available plots.
#'
#' @param fittedModel a fitted model  of a class supported by DHARMa
#' @param n number of simulations. Default is 100. A more save value would be 250 or even 1000. The smaller the number, the higher the stochastic error on the residuals. Also, for very small n, discretization artefacts can influence the tests.
#' @param refit if FALSE, new data will be simulated and scaled residuals will be created by comparing observed data with new data. If TRUE, the model will be refit on the simulated data (parametric bootstrap), and scaled residuals will be created by comparing observed with refitted residuals.
#' @param integerResponse if TRUE, noise will be added at to the residuals to maintain a uniform expectations for integer responses (such as Poisson or Binomial). Usually, the model will automatically detect the appropriate setting, so there is no need to adjust this setting.
#' @param plot if TRUE, \code{\link{plotResiduals}} will be directly run after the residuals have been calculated
#' @param ... parameters to pass to the simulate function of the model object. An important use of this is to specify whether simulations should be conditional on the current random effect estimates, e.g. via re.form. Note that not all models support syntax to specify conditionao or unconditional simulations. See also details
#' @param seed the random seed to be used within DHARMa. The default setting, recommended for most users, is keep the random seed on a fixed value 123. This means that you will always get the same randomization and thus teh same result when running the same code. NULL = no new seed is set, but previous random state will be restored after simulation. FALSE = no seed is set, and random state will not be restored. The latter two options are only recommended for simulation experiments. See vignette for details.
#' @param method the quantile randomization method used. The two options implemented at the moment are probability integral transform (PIT-) residuals (current default), and the "traditional" randomization procedure, that was used in DHARMa until version 0.3.0. For details, see \code{\link{getQuantile}}
#' @return An S3 class of type "DHARMa", essentially a list with various elements. Implemented S3 functions include plot, print and \code{\link{residuals.DHARMa}}. Residuals returns the calculated scaled residuals.
#'
#' @details There are a number of important considerations when simulating from a more complex (hierarchical) model:
#'
#' \strong{Re-simulating random effects / hierarchical structure}: in a hierarchical model, we have several stochastic processes aligned on top of each other. Specifically, in a GLMM, we have a lower level stochastic process (random effect), whose result enters into a higher level (e.g. Poisson distribution). For other hierarchical models such as state-space models, similar considerations apply.
#'
#' In such a situation, we have to decide if we want to re-simulate all stochastic levels, or only a subset of those. For example, in a GLMM, it is common to only simulate the last stochastic level (e.g. Poisson) conditional on the fitted random effects. This is often referred to as a conditional simuation. For controlling how many levels should be re-simulated, the simulateResidual function allows to pass on parameters to the simulate function of the fitted model object. Please refer to the help of the different simulate functions (e.g. ?simulate.merMod) for details. For merMod (lme4) model objects, the relevant parameters are parameters are use.u and re.form
#'
#' If the model is correctly specified, the simulated residuals should be flat regardless how many hierarchical levels we re-simulate. The most thorough procedure would therefore be to test all possible options. If testing only one option, I would recommend to re-simulate all levels, because this essentially tests the model structure as a whole. This is the default setting in the DHARMa package. A potential drawback is that re-simulating the lower-level random effects creates more variability, which may reduce power for detecting problems in the upper-level stochastic processes. In particular dispersion tests may produce different results when switching from conditional to unconditional simulations, and often the conditional simulation is more sensitive.
#'
#' \strong{Integer responses}: a second complication is the treatment of inter responses. Imaging we have observed a 0, and we predict 30\% zeros - what is the quantile that we should display for the residual? To deal with this problem and maintain a uniform response, the option integerResponse adds a uniform noise from -0.5 to 0.5 on the simulated and observed response, which creates a uniform distribution - you can see this via hist(ecdf(runif(10000))(runif(10000))).
#'
#'  DHARMa will try to automatically if the fitted model has an integer or discrete distribution via the family argument. However, in some cases the family does not allow to uniquely identify the distribution type. For example, a tweedie distribution can be inter or continuous. Therefore, DHARMa will additionally check the simulation results for repeated values, and will change the distribution type if repeated values are found (a message is displayed in this case).
#'
#' \strong{Refitting or not}: a third issue is how residuals are calculated. simulateResiduals has two options that are controlled by the refit parameter:
#'
#' 1. if refit = FALSE (default), new data is simulated from the fitted model, and residuals are calculated by comparing the observed data to the new data
#'
#' 2. if refit = TRUE, a parametric bootstrap is performed, meaning that the model is refit on the new data, and residuals are created by comparing observed residuals against refitted residuals. I advise against using this method per default (see more comments in the vignette), unless you are really sure that you need it.
#'
#' \strong{Residuals per group}: In many situations, it can be useful to look at residuals per group, e.g. to see how much the model over / underpredicts per plot, year or subject. To do this, use \code{\link{recalculateResiduals}}, together with a grouping variable (see also help)
#'
#' \strong{Transformation to other distributions}: DHARMa calculates residuals for which the theoretical expectation (assuming a correctly specified model) is uniform. To transfor this residuals to another distribution (e.g. so that a correctly specified model will have normal residuals) see \code{\link{residuals.DHARMa}}.
#'
#' @seealso \code{\link{testResiduals}}, \code{\link{plot.DHARMa}}, \code{\link{plotResiduals}}, \code{\link{print.DHARMa}}, \code{\link{residuals.DHARMa}}, \code{\link{recalculateResiduals}}
#'
#'
#' @example inst/examples/simulateResidualsHelp.R
#' @import stats
#' @export
simulateResiduals <- function(fittedModel, n = 250, refit = F, integerResponse = NULL, plot = F, seed = 123, method = c("PIT", "traditional"), ...){

  ######## general assertions and startup calculations ##########

  if (n < 2) stop("error in DHARMa::simulateResiduals: n > 1 is required to calculate scaled residuals")
  checkModel(fittedModel)
  match.arg(method)
  randomState <-getRandomState(seed)
  on.exit({randomState$restoreCurrent()})
  ptm <- proc.time()

  ####### extract model info ############

  out = list()

  family = family(fittedModel)
  out$fittedModel = fittedModel
  out$modelClass = class(fittedModel)[1]

  out$nObs = nobs(fittedModel)
  out$nSim = n
  out$refit = refit
  out$observedResponse = getObservedResponse(fittedModel)

  if(is.null(integerResponse)){
    if (family$family %in% c("binomial", "poisson", "quasibinomial", "quasipoisson", "Negative Binom", "nbinom2", "nbinom1", "genpois", "compois", "truncated_poisson", "truncated_nbinom2", "truncated_nbinom1", "betabinomial", "Poisson", "Tpoisson", "COMPoisson", "negbin", "Tnegbin") | grepl("Negative Binomial",family$family) ) integerResponse = TRUE
    else integerResponse = FALSE
  }
  out$integerResponse = integerResponse

  out$problems = list()

  # re-form should be set to ~0 to avoid spurious residual patterns, see https://github.com/florianhartig/DHARMa/issues/43

  if(out$modelClass %in% c("HLfit")){
    out$fittedPredictedResponse = predict(fittedModel, type = "response", re.form = ~0)[,1L]
  }else{
    out$fittedPredictedResponse = predict(fittedModel, type = "response", re.form = ~0)
  }

  out$fittedFixedEffects = getFixedEffects(fittedModel)
  out$fittedResiduals = residuals(fittedModel, type = "response")

  ######## refit = F ##################

  if (refit == FALSE){

    out$simulatedResponse = getSimulations(fittedModel, nsim = n, type = "normal", ...)

    checkSimulations(out$simulatedResponse, out$nObs, out$nSim)

    out$scaledResiduals = getQuantile(simulations = out$simulatedResponse , observed = out$observedResponse , integerResponse = integerResponse, method = method)

  ######## refit = T ##################
  } else {

    # Adding new outputs

    out$refittedPredictedResponse <- matrix(nrow = out$nObs, ncol = n )
    out$refittedFixedEffects <- matrix(nrow = length(out$fittedFixedEffects), ncol = n )
    #out$refittedRandomEffects <- matrix(nrow = length(out$fittedRandomEffects), ncol = n )
    out$refittedResiduals = matrix(nrow = out$nObs, ncol = n)
    out$refittedPearsonResiduals = matrix(nrow = out$nObs, ncol = n)

    out$simulatedResponse = getSimulations(fittedModel, nsim = n, type = "refit", ...)

    for (i in 1:n){

      simObserved = out$simulatedResponse[[i]]

      try({

        # for testing
        # if (i==3) stop("x")
        # Note: also set silent = T for production

        refittedModel = getRefit(fittedModel, simObserved)

        out$refittedPredictedResponse[,i] = predict(refittedModel, type = "response")
        out$refittedFixedEffects[,i] = getFixedEffects(refittedModel)
        out$refittedResiduals[,i] = residuals(refittedModel, type = "response")
        out$refittedPearsonResiduals[,i] = residuals(refittedModel, type = "pearson")
        #out$refittedRandomEffects[,i]  = ranef(refittedModel)
      }, silent = TRUE)
    }

    ######### residual checks ###########

    if(anyNA(out$refittedResiduals)) warning("DHARMa::simulateResiduals warning: on refit = TRUE, at least one of the refitted models produced an error. Inspect the refitted model values. Results may not be reliable.")

    ## check for convergence problems

    dup = sum(duplicated(out$refittedFixedEffects, MARGIN = 2))
    if (dup > 0){
      if (dup < n/3){
        warning(paste("There were", dup, "of", n ,"duplicate parameter estimates in the refitted models. This may hint towards a problem with optimizer convergence in the fitted models. Results may not be reliable. The suggested action is to not use the refitting procedure, and diagnose with tools available for the normal (not refitted) simulated residuals. If you absolutely require the refitting procedure, try changing tolerance / iterations in the optimizer settings."))
      } else {
        warning(paste("There were", dup, "of", n ,"duplicate parameter estimates in the refitted models. This may hint towards a problem with optimizer convergence in the fitted models. Results are likely not reliable. The suggested action is to not use the refitting procedure, and diagnose with tools available for the normal (not refitted) simulated residuals. If you absolutely require the refitting procedure, try changing tolerance / iterations in the optimizer settings."))
        out$problems[[length(out$problems)+ 1]] = "error in refit"
      }
    }

    ######### residual calculations ###########

    out$scaledResiduals = getQuantile(simulations = out$refittedResiduals, observed = out$fittedResiduals, integerResponse = integerResponse, method = method)
  }

  ########### Wrapup ############

  out$time = proc.time() - ptm
  out$randomState = randomState

  class(out) = "DHARMa"

  if(plot == TRUE) plot(out)

  return(out)
}

getPossibleModels<-function()c("lm", "glm", "negbin", "lmerMod", "glmerMod", "gam", "bam", "glmmTMB", "HLfit")



#' Check if the fitted model is supported by DHARMa
#'
#' The function checks if the fitted model is supported by DHARMa, and if there are other issues that could create problems
#'
#' @param fittedModel a fitted model
#' @param stop whether to throw an error if the model is not supported by DHARMa
#'
#' @details The main purpose of this function os to check if the fitted model class is supported by DHARMa. The function additionally checks for properties of the fitted model that could create problems for calculating residuals or working with the resuls in DHARMa.
#'
#'
#' @keywords internal
checkModel <- function(fittedModel, stop = F){

  out = T

  if(!(class(fittedModel)[1] %in% getPossibleModels())){
    if(stop == FALSE) warning("DHARMa: fittedModel not in class of supported models. Absolutely no guarantee that this will work!")
    else stop("DHARMa: fittedModel not in class of supported models")
  }

  # if(hasNA(fittedModel)) message("It seems there were NA values in the data used for fitting the model. This can create problems if you supply additional data to DHARMa functions. See ?checkModel for details")

  # TODO: check as implemented does not work reliably, check if there is any other option to check for NA
  # #' @example inst/examples/checkModelHelp.R

  #  NA values in the data: checkModel will detect if there were NA values in the data frame. For NA values, most regression models will remove the entire observation from the data. This is not a problem for DHARMa - residuals are then only calculated for non-NA rows in the data. However, if you provide additional predictors to DHARMa, for example to plot residuals against a predictor, you will have to remove all NA rows that were also removed in the model. For most models, you can get the rows of the data that were actually used in the fit via rownames(model.frame(fittedModel))


  if (class(fittedModel)[1] == "gam" ) if (class(fittedModel$family)[1] == "extended.family") stop("It seems you are trying to fit a model from mgcv that was fit with an extended.family. Simulation functions for these families are not yet implemented in DHARMa. See issue https://github.com/florianhartig/DHARMa/issues/11 for updates about this")

}



#' Check simulated data
#'
#' The function checks if the simulated data seems fine
#'
#' @param simulatedResponse the simulated response
#' @param nObs number of observations
#' @param nSim number of simulations
#'
#' @keywords internal
checkSimulations <- function(simulatedResponse, nObs, nSim){

  if(!inherits(simulatedResponse, "matrix")) securityAssertion("Simulation from the model produced wrong class", stop = T)

  if(any(dim(simulatedResponse) != c(nObs, nSim) )) securityAssertion("Simulation from the model produced wrong dimension", stop = T)

  if(any(!is.finite(simulatedResponse))) message("Simulations from your fitted model produce infinite values. Consider if this is sensible")

  if(any(is.nan(simulatedResponse))) securityAssertion("Simulations from your fitted model produce NaN values. DHARMa cannot calculated residuals for this. This is nearly certainly an error of the regression package you are using", stop = T)
  if(any(is.na(simulatedResponse))) securityAssertion("Simulations from your fitted model produce NA values. DHARMa cannot calculated residuals for this. This is nearly certainly an error of the regression package you are using", stop = T)

}




#' Recalculate residuals with grouping
#'
#' The purpose of this function is to recalculate scaled residuals per group, based on the simulations done by \code{\link{simulateResiduals}}
#'
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param group group of each data point
#' @param aggregateBy function for the aggregation. Default is sum. This should only be changed if you know what you are doing. Note in particular that the expected residual distribution might not be flat any more if you choose general functions, such as sd etc.
#' @param seed the random seed to be used within DHARMa. The default setting, recommended for most users, is keep the random seed on a fixed value 123. This means that you will always get the same randomization and thus teh same result when running the same code. NULL = no new seed is set, but previous random state will be restored after simulation. FALSE = no seed is set, and random state will not be restored. The latter two options are only recommended for simulation experiments. See vignette for details.
#' @param method the quantile randomization method used. The two options implemented at the moment are probability integral transform (PIT-) residuals (current default), and the "traditional" randomization procedure, that was used in DHARMa until version 0.3.0. For details, see \code{\link{getQuantile}}
#' @return an object of class DHARMa, similar to what is returned by \code{\link{simulateResiduals}}, but with additional outputs for the new grouped calculations. Note that the relevant outputs are 2x in the object, the first is the grouped calculations (which is returned by $name access), and later another time, under identical name, the original output. Moreover, there is a function 'aggregateByGroup', which can be used to aggregate predictor variables in the same way as the variables calculated here
#'
#' @example inst/examples/simulateResidualsHelp.R
#' @export
recalculateResiduals <- function(simulationOutput, group = NULL, aggregateBy = sum, seed = 123, method = c("PIT", "traditional")){

  randomState <-getRandomState(seed)
  on.exit({randomState$restoreCurrent()})
  match.arg(method)

  if(!is.null(simulationOutput$original)) simulationOutput = simulationOutput$original

  out = list()
  out$original = simulationOutput

  if(is.null(group)) return(simulationOutput)
  else group =as.factor(group)
  out$nGroups = nlevels(group)

  aggregateByGroup <- function(x) aggregate(x, by=list(group), FUN=aggregateBy)[,2]

  out$observedResponse = aggregateByGroup(simulationOutput$observedResponse)
  out$fittedPredictedResponse = aggregateByGroup(simulationOutput$fittedPredictedResponse)

  if (simulationOutput$refit == F){

    out$simulatedResponse = apply(simulationOutput$simulatedResponse, 2, aggregateByGroup)
    out$scaledResiduals = getQuantile(simulations = out$simulatedResponse , observed = out$observedResponse , integerResponse = simulationOutput$integerResponse, method = method)

  ######## refit = T ##################
  } else {

    out$refittedPredictedResponse <- apply(simulationOutput$refittedPredictedResponse, 2, aggregateByGroup)
    out$fittedResiduals = aggregateByGroup(simulationOutput$fittedResiduals)
    out$refittedResiduals = apply(simulationOutput$refittedResiduals, 2, aggregateByGroup)
    out$refittedPearsonResiduals = apply(simulationOutput$refittedPearsonResiduals, 2, aggregateByGroup)

    out$scaledResiduals = getQuantile(simulations = out$refittedResiduals , observed = out$fittedResiduals , integerResponse = simulationOutput$integerResponse, method = method)

  }

  # hack - the c here will result in both old and new outputs to be present resulting output, but a named access should refer to the new, grouped calculations
  # question to myself - what's the use of that, why not erase the old outputs? they are anyway saved in the old object

  out$aggregateByGroup = aggregateByGroup
  out = c(out, simulationOutput)
  out$randomState = randomState
  class(out) = "DHARMa"
  return(out)
}

################ simulateResiduals.R

################ DHARMa.R

#' @title DHARMa - Residual Diagnostics for HierArchical (Multi-level / Mixed) Regression Models
#' @name DHARMa
#' @docType package
#' @description The 'DHARMa' package uses a simulation-based approach to create  readily interpretable scaled (quantile) residuals for fitted (generalized) linear mixed models. Currently supported are linear and generalized linear (mixed) models from 'lme4' (classes 'lmerMod', 'glmerMod'), 'glmmTMB' and 'spaMM', generalized additive models ('gam' from 'mgcv'), 'glm' (including 'negbin' from 'MASS', but excluding quasi-distributions) and 'lm' model classes. Moreover, externally created simulations, e.g. posterior predictive simulations from Bayesian software such as 'JAGS', 'STAN', or 'BUGS' can be processed as well. The resulting residuals are standardized to values between 0 and 1 and can be interpreted as intuitively as residuals from a linear regression. The package also provides a number of plot and test functions for typical model misspecification problems, such as over/underdispersion, zero-inflation, and residual spatial and temporal autocorrelation.
#' @details See index / vignette for details
#' @seealso \code{\link{simulateResiduals}}
#' @examples
#' vignette("DHARMa", package="DHARMa")
NULL


#' Print simulated residuals
#'
#' @param x an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param ... optional arguments for compatibility with the generic function, no function implemented
#' @export
print.DHARMa <- function(x, ...){
  cat(paste("Object of Class DHARMa with simulated residuals based on", x$nSim, "simulations with refit =", x$refit , ". See ?DHARMa::simulateResiduals for help."), "\n", "\n")
  if (length(x$scaledResiduals) < 20) cat("Scaled residual values:", x$scaledResiduals)
  else {
    cat("Scaled residual values:", x$scaledResiduals[1:20], "...")
  }
}

#' Return residuals of a DHARMa simulation
#'
#' @param object an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param quantileFunction optional - a quantile function to transform the uniform 0/1 scaling of DHARMa to another distribution
#' @param outlierValues if a quantile function with infinite support (such as dnorm) is used, residuals that are 0/1 are mapped to -Inf / Inf. outlierValues allows to convert -Inf / Inf values to an optional min / max value.
#' @param ... optional arguments for compatibility with the generic function, no function implemented
#' @details the function accesses the slot $scaledResiduals in a fitted DHARMa object, and optionally transforms the standard DHARMa quantile residuals (which have a uniform distribution) to a particular pdf.
#'
#' @note some of the papers on simulated quantile residuals transforming the residuals (which are natively uniform) back to a normal distribution. I presume this is because of the larger familiarity of most users with normal residuals. Personally, I never considered this desirable, for the reasons explained in https://github.com/florianhartig/DHARMa/issues/39, but with this function, I wanted to give users the option to plot normal residuals if they so wish.
#'
#' @export
#' @example inst/examples/simulateResidualsHelp.R
#'
residuals.DHARMa <- function(object, quantileFunction = NULL, outlierValues = NULL, ...){

  if(is.null(quantileFunction)){
    return(object$scaledResiduals)
  } else {
    res = quantileFunction(object$scaledResiduals)
    if(!is.null(outlierValues)){
      res = ifelse(res == -Inf, outlierValues[1], res)
      res = ifelse(res == Inf, outlierValues[2], res)
    }
    return(res)
  }
}



#' Return outliers
#'
#' Returns the outliers of a DHARMa object
#'
#' @param object an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param lowerQuantile lower threshold for outliers. Default is zero = outside simulation envelope
#' @param upperQuantile upper threshold for outliers. Default is 1 = outside simulation envelope
#' @param return wheter to return an indices of outliers or a logical vector
#'
#' @details First of all, note that the standard definition of outlier in the DHARMa plots and outlier tests is an observation that is outside the simulation envelope. How far outside that is depends a lot on how many simulations you do. If you have 100 data points and to 100 simulations, you would expect to have one "outlier" on average, even with a perfectly fitting model. This is in fact what the outlier test tests.
#'
#' Thus, keep in mind that for a small number of simulations, outliers are mostly a technical term: these are points that are outside our simulations, but we don't know how far away they are.
#'
#' If you are seriously interested in HOW FAR outside the expected distribution a data point is, you should increase the number of simulations in \code{\link{simulateResiduals}} to be sure to get the tail of the data distribution correctly. In this case, it may make sense to adjust lowerQuantile and upperQuantile, e.g. to 0.025, 0.975, which would define outliers as values outside the central 95% of the distribution.
#'
#' Also, note that outliers are particularly concerning if they have a strong influence on the model fit. One could test the influence, for example, by removing them from the data, or by some meausures of leverage, e.g. generalisations for Cook's distance as in Pinho, L. G. B., Nobre, J. S., & Singer, J. M. (2015). Cook’s distance for generalized linear mixed models. Computational Statistics & Data Analysis, 82, 126–136. doi:10.1016/j.csda.2014.08.008. At the moment, however, no such function is provided in DHARMa.
#'
#' @export
#'
outliers <- function(object, lowerQuantile = 0, upperQuantile = 1, return = c("index", "logical")){

  return = match.arg(return)

  out = residuals(object) >= upperQuantile | residuals(object) <= lowerQuantile

  if(return == "logical") return(out)
  else(return(which(out)))
}



#' Create a DHARMa object from hand-coded simulations or Bayesian posterior predictive simulations
#'
#' @param simulatedResponse matrix of observations simulated from the fitted model - row index for observations and colum index for simulations
#' @param observedResponse true observations
#' @param fittedPredictedResponse optional fitted predicted response. For Bayesian posterior predictive simulations, using the median posterior prediction as fittedPredictedResponse is recommended. If not provided, the mean simulatedResponse will be used.
#' @param integerResponse if T, noise will be added at to the residuals to maintain a uniform expectations for integer responses (such as Poisson or Binomial). Unlike in \code{\link{simulateResiduals}}, the nature of the data is not automatically detected, so this MUST be set by the user appropriately
#' @param seed the random seed to be used within DHARMa. The default setting, recommended for most users, is keep the random seed on a fixed value 123. This means that you will always get the same randomization and thus teh same result when running the same code. NULL = no new seed is set, but previous random state will be restored after simulation. FALSE = no seed is set, and random state will not be restored. The latter two options are only recommended for simulation experiments. See vignette for details.
#' @param method the quantile randomization method used. The two options implemented at the moment are probability integral transform (PIT-) residuals (current default), and the "traditional" randomization procedure, that was used in DHARMa until version 0.3.0. For details, see \code{\link{getQuantile}}
#' @details The use of this function is to convert simulated residuals (e.g. from a point estimate, or Bayesian p-values) to a DHARMa object, to make use of the plotting / test functions in DHARMa
#' @note Either scaled residuals or (simulatedResponse AND observed response) have to be provided
#' @example inst/examples/createDharmaHelp.R
#' @export
createDHARMa <- function(simulatedResponse , observedResponse , fittedPredictedResponse = NULL, integerResponse = F, seed = 123,  method = c("PIT", "traditional")){

  randomState <-getRandomState(seed)
  on.exit({randomState$restoreCurrent()})
  match.arg(method)

  out = list()
  out$simulatedResponse = simulatedResponse
  out$refit = F
  out$integerResponse = integerResponse
  out$observedResponse = observedResponse

  if(!is.matrix(simulatedResponse) & !is.null(observedResponse)) stop("either scaled residuals or simulations and observations have to be provided")
  if(ncol(simulatedResponse) < 2) stop("simulatedResponse with less than 2 simulations provided - cannot calculate residuals on that.")

  if(ncol(simulatedResponse) < 10) warning("simulatedResponse with less than 10 simulations provided. This rarely makes sense")

  out$nObs = length(observedResponse)

  if (out$nObs < 3) stop("warning - number of observations < 3 ... this rarely makes sense")

  if(! (out$nObs == nrow(simulatedResponse))) stop("dimensions of observedResponse and simulatedResponse do not match")

  out$nSim = ncol(simulatedResponse)

  out$scaledResiduals = getQuantile(simulations = simulatedResponse , observed = observedResponse , integerResponse = integerResponse, method = method)


  # makes sure that DHARM plots that rely on this vector won't crash
  if(is.null(fittedPredictedResponse)){
    message("No fitted predicted response provided, using the mean of the simulations")
    fittedPredictedResponse = apply(simulatedResponse, 1, mean)
  }
  out$fittedPredictedResponse = fittedPredictedResponse
  out$randomState = randomState
  class(out) = "DHARMa"
  return(out)
}


#' Ensures that an object is of class DHARMa
#'
#' @param simulationOutput a DHARMa simulation output or an object that can be converted into a DHARMa simulation output
#' @param convert if TRUE, attempts to convert model + numeric to DHARMa, if "Model", converts only supported models to DHARMa
#' @details The
#' @return an object of class DHARMa
#' @keywords internal
ensureDHARMa <- function(simulationOutput,
                         convert = F){

  if(inherits(simulationOutput, "DHARMa")){
    return(simulationOutput)
  } else {

    if(convert == FALSE) stop("wrong argument to function, simulationOutput must be a DHARMa object!")
    else {

      if (class(simulationOutput)[1] %in% getPossibleModels()){
        if (convert == "Model" | convert == T) return(simulateResiduals(simulationOutput))
      } else if(is.vector(simulationOutput, mode = "numeric") & convert == T) {
        out = list()
        out$scaledResiduals = simulationOutput
        out$nObs = length(out$scaledResiduals)
        class(out) = "DHARMa"
        return(out)
      }
    }
  }
  stop("wrong argument to function, simulationOutput must be a DHARMa object or a numeric vector of quantile residuals!")
}

####################### DHARMa.R

####################### tests.R

#' DHARMa general residual test
#'
#' Calls both uniformity and dispersion test
#'
#' This function is a wrapper for the various test functions implemented in DHARMa. Currently, this function calls the \code{\link{testUniformity}} and the \code{\link{testDispersion}} functions. All other tests (see list below) have to be called by hand.
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via \code{\link{simulateResiduals}} or by \code{\link{createDHARMa}} for simulations created outside DHARMa
#' @param plot if T, plots functions of the tests are called
#' @author Florian Hartig
#' @seealso \code{\link{testUniformity}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testQuantiles}}
#' @example inst/examples/testsHelp.R
#' @export
testResiduals <- function(simulationOutput, plot = T){

  opar = par(mfrow = c(1,3))
  on.exit(par(opar))
  out = list()
  out$uniformity = testUniformity(simulationOutput, plot = plot)
  out$dispersion = testDispersion(simulationOutput, plot = plot)
  out$outliers = testOutliers(simulationOutput, plot = plot)

  print(out)
  return(out)
}

#' Residual tests
#'
#' @details Deprecated, switch your code to using the \code{\link{testResiduals}} function
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via \code{\link{simulateResiduals}} or by \code{\link{createDHARMa}} for simulations created outside DHARMa
#' @author Florian Hartig
#' @export
testSimulatedResiduals <- function(simulationOutput){
  message("testSimulatedResiduals is deprecated, switch your code to using the testResiduals function")
  testResiduals(simulationOutput)
}


#' Test for overall uniformity
#'
#' This function tests the overall uniformity of the simulated residuals in a DHARMa object
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via \code{\link{simulateResiduals}} or by \code{\link{createDHARMa}} for simulations created outside DHARMa
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis. See \code{\link[stats]{ks.test}} for details
#' @param plot if T, plots calls \code{\link{plotQQunif}} as well
#' @details The function applies a \code{\link[stats]{ks.test}} for uniformity on the simulated residuals.
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testQuantiles}}
#' @example inst/examples/testsHelp.R
#' @export
testUniformity<- function(simulationOutput, alternative = c("two.sided", "less", "greater"), plot = T){

  simulationOutput = ensureDHARMa(simulationOutput, convert = T)

  out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif', alternative = alternative))
  if(plot == T) plotQQunif(simulationOutput = simulationOutput)
  return(out)
}


# Experimental
testBivariateUniformity<- function(simulationOutput, alternative = c("two.sided", "less", "greater"), plot = T){

  simulationOutput = ensureDHARMa(simulationOutput, convert = T)

  #out <- suppressWarnings(ks.test(simulationOutput$scaledResiduals, 'punif', alternative = alternative))
  #if(plot == T) plotQQunif(simulationOutput = simulationOutput)
  out = NULL
  return(out)
}



#' Test for quantiles
#'
#' This function tests
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via \code{\link{simulateResiduals}} or by \code{\link{createDHARMa}} for simulations created outside DHARMa
#' @param predictor an optional predictor variable to be used, instead of the predicted response (default)
#' @param quantiles the quantiles to be tested
#' @param plot if T, the function will create an additional plot
#' @details The function fits quantile regressions (via package qgam) on the residuals, and compares their location to the expected location (because of the uniform distributionm, the expected location is 0.5 for the 0.5 quantile).
#'
#' A significant p-value for the splines means the fitted spline deviates from a flat line at the expected location (p-values of intercept and spline are combined via Benjamini & Hochberg adjustment to control the FDR)
#'
#' The p-values of the splines are combined into a total p-value via Benjamini & Hochberg adjustment to control the FDR.
#'
#' @author Florian Hartig
#' @example inst/examples/testQuantilesHelp.R
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testOutliers}}
#' @export
testQuantiles <- function(simulationOutput, predictor = NULL, quantiles = c(0.25,0.5,0.75), plot = T){

  if(plot == F){

    out = list()
    out$data.name = deparse(substitute(simulationOutput))

    simulationOutput = ensureDHARMa(simulationOutput, convert = T)
    res = simulationOutput$scaledResiduals
    pred = ensurePredictor(simulationOutput, predictor)

    dat=data.frame(res =  simulationOutput$scaledResiduals , pred = pred)

    quantileFits <- list()
    pval = rep(NA, length(quantiles))
    predictions = data.frame(pred = sort(dat$pred))
    predictions = cbind(predictions, matrix(ncol = 2 * length(quantiles), nrow = nrow(dat)))
    for(i in 1:length(quantiles)){
      datTemp = dat
      datTemp$res = datTemp$res - quantiles[i]

      # settings for k = the dimension of the basis used to represent the smooth term.
      # see https://github.com/mfasiolo/qgam/issues/37
      dimSmooth =  min(length(unique(datTemp$pred)), 10)
      quantResult = try(capture.output(quantileFits[[i]] <- qgam::qgam(res ~ s(pred, k = dimSmooth) ,  data =datTemp, qu = quantiles[i])), silent = T)
      if(inherits(quantResult, "try-error")){
        message("Unable to calculate quantile regression for quantile ", quantiles[i], ". Possibly to few (unique) data points / predictions. Will be ommited in plots and significance calculations.")
      } else {
        x = summary(quantileFits[[i]])
        pval[i] = min(p.adjust(c(x$p.table[1,4], x$s.table[1,4]), method = "BH")) # correction for test on slope and intercept
        quantPre = predict(quantileFits[[i]], newdata = predictions, se = T)
        predictions[, 2*i] = quantPre$fit + quantiles[i]
        predictions[, 2*i + 1] = quantPre$se.fit
      }
    }

    out$method = "Test for location of quantiles via qgam"
    out$alternative = "both"
    out$pvals = pval
    out$p.value = min(p.adjust(pval, method = "BH")) # correction for multiple quantile tests
    out$predictions = predictions
    out$qgamFits = quantileFits

    class(out) = "htest"

  } else if(plot == T) {
    out <- plotResiduals(simulationOutput = simulationOutput, predictor = predictor, quantiles = quantiles)
  }
  return(out)
}


#unif.2017YMi(X, type = c("Q1", "Q2", "Q3"), lower = rep(0, ncol(X)),upper = rep(1, ncol(X)))

#' Test for outliers
#'
#' This function tests if the number of observations outside the simulatio envelope are larger or smaller than expected
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via \code{\link{simulateResiduals}} or by \code{\link{createDHARMa}} for simulations created outside DHARMa
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" (default) compared to the simulated null hypothesis
#' @param margin whether to test for outliers only at the lower, only at the upper, or both sides (default) of the simulated data distribution
#' @param plot if T, the function will create an additional plot
#' @details DHARMa residuals are created by simulating from the fitted model, and comparing the simulated values to the observed data. It can occur that all simulated values are higher or smaller than the observed data, in which case they get the residual value of 0 and 1, respectively. I refer to these values as simulation outliers, or simply outliers.
#'
#' Because no data was simulated in the range of the observed value, we don't know "how strongly" these values deviate from the model expectation, so the term "outlier" should be used with a grain of salt - it's not a judgment about the magnitude of a deviation from an expectation, but simply that we are outside the simulated range, and thus cannot say anything more about the location of the residual.
#'
#' Note also that the number of outliers will decrease as we increase the number of simulations. Under the null hypothesis that the model is correct, we expect nData /(nSim +1) outliers at each margin of the distribution. For a reason, consider that if the data and the model distribution are identical, the probability that a given observation is higher than all simulations is 1/(nSim +1).
#'
#' Based on this null expectation, we can test for an excess or lack of outliers. Per default, testOutliers() looks for both, so if you get a significant p-value, you have to check if you have to many or too few outliers. An excess of outliers is to be interpreted as too many values outside the simulation envelope. This could be caused by overdispersion, or by what we classically call outliers. A lack of outliers would be caused, for example, by underdispersion.
#'
#'
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testQuantiles}}
#' @example inst/examples/testOutliersHelp.R
#' @export
testOutliers <- function(simulationOutput, alternative = c("two.sided", "greater", "less"), margin = c("both", "upper", "lower"), plot = T){

  # check inputs
  alternative = match.arg(alternative)
  margin = match.arg(margin)
  data.name = deparse(substitute(simulationOutput)) # remember: needs to be called before ensureDHARMa
  simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")

  # calculation of outliers
  if(margin == "both")  outliers = sum(simulationOutput$scaledResiduals %in% c(0, 1))
  if(margin == "upper") outliers = sum(simulationOutput$scaledResiduals == 1)
  if(margin == "lower") outliers = sum(simulationOutput$scaledResiduals == 0)

  # calculations of trials and H0
  outFreqH0 = 1/(simulationOutput$nSim +1) * ifelse(margin == "both", 2, 1)
  trials = simulationOutput$nObs

  out = binom.test(outliers, trials, p = outFreqH0, alternative = alternative)

  # overwrite information in binom.test

  out$data.name = data.name
  out$margin = margin
  out$method = "DHARMa outlier test based on exact binomial test"

  names(out$statistic) = paste("outliers at", margin, "margin(s)")
  names(out$parameter) = "simulations"
  names(out$estimate) = paste("frequency of outliers (expected:", out$null.value,")")

  out$interval = "tst"

  out$interval =

  if(plot == T) {

    hist(simulationOutput, main = "")

    main = ifelse(out$p.value <= 0.05,
                  "Outlier test significant",
                  "Outlier test n.s.")

    title(main = main, cex.main = 1,
          col.main = ifelse(out$p.value <= 0.05, "red", "black"))

    # legend("center", c(paste("p=", round(out$p.value, digits = 5)), paste("Deviation ", ifelse(out$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(out$p.value < 0.05, "red", "black" ))

  }
  return(out)
}


#' DHARMa dispersion tests
#'
#' This function performs a simulation-based test for over/underdispersion
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via \code{\link{simulateResiduals}} or by \code{\link{createDHARMa}} for simulations created outside DHARMa
#' @param plot whether to plot output
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis. Greater corresponds to overdispersion.
#' @param ... arguments to pass on to \code{\link{testGeneric}}
#' @details The function implements two tests, depending on whether it is applied on a simulation with refit = F, or refit = T.
#'
#' If refit = F, the function tests the sd of the data against the sd of the simulated data.
#'
#' If refit = T, the function compares the approximate deviance (via squared pearson residuals) with the same quantity from the models refitted with simulated data. Applying this is much slower than the previous alternative. Given the computational cost, I would suggest that most users will be satisfied with the standard dispersion test.
#'
#' @note The results of the dispersion test can can differ depending on whether it is evaluated on conditional (= conditional on fitted random effects) or unconditional (= REs are re-simulated) simulations. You can change between conditional or unconditional simulations in  \code{\link{simulateResiduals}} if this is supported by the regression package that you use. The default in DHARMa is to use unconditional simulations, but I have often found that conditional simulations are more sensitive to dispersion problems. I recommend trying both, as neither test should be positive if the dispersion is correct.
#'
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}},  \code{\link{testOutliers}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testQuantiles}}
#' @example inst/examples/testsHelp.R
#' @export
testDispersion <- function(simulationOutput, alternative = c("two.sided", "greater", "less"), plot = T, ...){

  out = list()
  out$data.name = deparse(substitute(simulationOutput))

  alternative <- match.arg(alternative)

  simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")

  if(simulationOutput$refit == F){

    spread <- function(x) sd(x - simulationOutput$fittedPredictedResponse)
    out = testGeneric(simulationOutput, summary = spread, alternative = alternative, methodName = "DHARMa nonparametric dispersion test via sd of residuals fitted vs. simulated", plot = plot, ...)
  } else {

    observed = tryCatch(sum(residuals(simulationOutput$fittedModel, type = "pearson")^2), error = function(e) {
      message(paste("DHARMa: the requested tests requires pearson residuals, but your model does not implement these calculations. Test will return NA. Error message:", e))
      return(NA)
    })
    if(is.na(observed)) return(NA)
    expected = apply(simulationOutput$refittedPearsonResiduals^2 , 2, sum)
    out$statistic = c(dispersion = observed / mean(expected))
    out$method = "DHARMa nonparametric dispersion test via mean deviance residual fitted vs. simulated-refitted"

    p = getP(simulated = expected, observed = observed, alternative = alternative)

    out$alternative = alternative
    out$p.value = p
    class(out) = "htest"

    if(plot == T) {
      #plotTitle = gsub('(.{1,50})(\\s|$)', '\\1\n', out$method)
      xLabel = paste("Simulated values, red line = fitted model. p-value (",out$alternative, ") = ", out$p.value, sep ="")

      hist(expected, xlim = range(expected, observed, na.rm=T ), col = "lightgrey", main = "", xlab = xLabel, breaks = 20, cex.main = 1)
      abline(v = observed, lwd= 2, col = "red")

      main = ifelse(out$p.value <= 0.05,
                    "Dispersion test significant",
                    "Dispersion test n.s.")

      title(main = main, cex.main = 1,
            col.main = ifelse(out$p.value <= 0.05, "red", "black"))
    }
  }

  return(out)
}

#' Simulated overdisperstion tests
#'
#' @details Deprecated, switch your code to using the \code{\link{testDispersion}} function
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via \code{\link{simulateResiduals}} or by \code{\link{createDHARMa}} for simulations created outside DHARMa
#' @param ... additional arguments to \code{\link{testDispersion}}
#' @export
testOverdispersion <- function(simulationOutput, ...){
  message("testOverdispersion is deprecated, switch your code to using the testDispersion function")
  testDispersion(simulationOutput, ...)
}

#' Parametric overdisperstion tests
#'
#' @details Deprecated, switch your code to using the \code{\link{testDispersion}} function. The function will do nothing, arguments will be ignored, the parametric tests is no longer recommend
#'
#' @param ... arguments will be ignored, the parametric tests is no longer recommend
#' @export
testOverdispersionParametric <- function(...){
  message("testOverdispersionParametric is deprecated and no longer recommended, see release notes in DHARMA 0.2.0 - switch your code to using the testDispersion function")
  return(0)
}


#' Tests for zero-inflation
#'
#' This function compares the observed number of zeros with the zeros expected from simulations.
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via \code{\link{simulateResiduals}} or by \code{\link{createDHARMa}} for simulations created outside DHARMa
#' @param ... further arguments to \code{\link{testGeneric}}
#' @details The plot shows the expected distribution of zeros against the observed values, the ratioObsSim shows observed vs. simulated zeros. A value < 1 means that the observed data has less zeros than expected, a value > 1 means that it has more zeros than expected (aka zero-inflation). Per default, the function tests both sides.
#'
#' Some notes about common problems / questions:
#'
#' * Zero-inflation tests after fitting the model are crucial to see if you have zero-inflation. Just because there are a lot of zeros doesn't mean you have zero-inflation, see Warton, D. I. (2005). Many zeros does not mean zero inflation: comparing the goodness-of-fit of parametric models to multivariate abundance data. Environmetrics 16(3), 275-289.
#'
#' * That being said, zero-inflation tests are often not a reliable guide to decide wheter to add a zi term or not. In general, model structures should be decided on ideally a priori, if that is not possible via model selection techniques (AIC, BIC, WAIC, Bayes Factor). A zero-inflation test should only be run after that decision, and to validate the decision that was taken.
#'
#' @note This function is a wrapper for \code{\link{testGeneric}}, where the summary argument is set to function(x) sum(x == 0)
#' @author Florian Hartig
#' @example inst/examples/testsHelp.R
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testQuantiles}}
#' @export
testZeroInflation <- function(simulationOutput, ...){
  countZeros <- function(x) sum( x == 0)
  testGeneric(simulationOutput = simulationOutput, summary = countZeros, methodName = "DHARMa zero-inflation test via comparison to expected zeros with simulation under H0 = fitted model", ... )
}


#' Generic simulation test of a summary statistic
#'
#' This function tests if a user-defined summary differs when applied to simulated / observed data.
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via \code{\link{simulateResiduals}} or by \code{\link{createDHARMa}} for simulations created outside DHARMa
#' @param summary a function that can be applied to simulated / observed data. See examples below
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis
#' @param plot whether to plot the simulated summary
#' @param methodName name of the test (will be used in plot)
#'
#' @details This function tests if a user-defined summary differs when applied to simulated / observed data. the function can easily be remodeled to apply summaries on the residuals, by simply defining f = function(x) summary (x - predictions), as done in \code{\link{testDispersion}}
#'
#' @note The function that you supply is applied on the data as it is represented in your fitted model, which may not always correspond to how you think. This is important in particular when you use k/n binomial data, and want to test for 1-inflation. As an example, if have k/20 observations, and you provide your data via cbind (y, y-20), you have to test for 20-inflation (because this is how the data is represented in the model). However, if you provide data via y/20, and weights = 20, you should test for 1-inflation. In doubt, check how the data is internally represented in model.frame(model), or via simulate(model)
#'
#' @export
#' @author Florian Hartig
#' @example inst/examples/testsHelp.R
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testQuantiles}}
testGeneric <- function(simulationOutput, summary, alternative = c("two.sided", "greater", "less"), plot = T, methodName = "DHARMa generic simulation test"){

  out = list()
  out$data.name = deparse(substitute(simulationOutput))

  simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")

  alternative <- match.arg(alternative)

  observed = summary(simulationOutput$observedResponse)

  simulated = apply(simulationOutput$simulatedResponse, 2, summary)

  p = getP(simulated = simulated, observed = observed, alternative = alternative)

  out$statistic = c(ratioObsSim = observed / mean(simulated))
  out$method = methodName
  out$alternative = alternative
  out$p.value = p


  class(out) = "htest"

  if(plot == T) {
    plotTitle = gsub('(.{1,50})(\\s|$)', '\\1\n', methodName)
    xLabel = paste("Simulated values, red line = fitted model. p-value (",out$alternative, ") = ", out$p.value, sep ="")
   hist(simulated, xlim = range(simulated, observed, na.rm=T ), col = "lightgrey", main = plotTitle, xlab = xLabel, breaks = max(round(simulationOutput$nSim / 5), 20), cex.main = 0.8)
   abline(v = observed, lwd= 2, col = "red")
  }
  return(out)
}


#' Test for temporal autocorrelation
#'
#' This function performs a standard test for temporal autocorrelation on the simulated residuals
#'
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param time the time, in the same order as the data points. If not provided, random values will be created
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis
#' @param plot whether to plot output
#' @details The function performs a Durbin-Watson test on the uniformly scaled residuals, and plots the residuals against time. The DB test was originally be designed for normal residuals. In simulations, I didn't see a problem with this setting though. The alternative is to transform the uniform residuals to normal residuals and perform the DB test on those.
#'
#' If no time values are provided, random values will be created. The sense of being able to run the test with time = NULL (random values) is to test the rate of false positives under the current residual structure (random time corresponds to H0: no spatial autocorrelation), e.g. to check if the test has noninal error rates for particular residual structures (note that Durbin-Watson originally assumes normal residuals, error rates seem correct for uniform residuals, but may not be correct if there are still other residual problems).
#'
#' Testing for temporal autocorrelation requires unique time values - if you have several observations per time value, either use the recalculateResiduals function to aggregate residuals per time step, or extract the residuals from the fitted object, and plot / test each of them independently for temporally repeated subgroups (typical choices would be location / subject etc.). Note that the latter must be done by hand, outside testSpatialAutocorrelation.
#'
#' @note Important to note for all autocorrelation tests (spatial / temporal): the autocorrelation tests are valid to check for residual autocorrelation in models that don't assume such a correlation (in this case, you can use conditional or unconditional simulations), or if there is remaining residual autocorrelation after accounting for it in a spatial/temporal model (in that case, you have to use conditional simulations), but if checking unconditional simulations from a model with an autocorrelation structure on data that corresponds to this model, they will be significant, even if the model fully accounts for this structure.
#'
#' This behavior is not really a bug, but rather originates from the definition of the quantile residuals: quantile residuals are calculated independently per data point, i.e. without consideratin of any correlation structure between data points that may exist in the simulations. As a result, the simulated distributions from a unconditional simulaton will typically not reflect the correlation structure that is present in each single simulation, and the same is true for the subsequently calculated quantile residuals.
#'
#' The bottomline here is that spatial / temporal / other autoregressive models should either be tested based on conditional simulations, or (ideally) custom tests should be used that are not based on quantile residuals, but rather compare the correlation structure in the simulated data with the correlation structure in the observed data.
#'
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testSpatialAutocorrelation}}, \code{\link{testQuantiles}}
#' @example inst/examples/testTemporalAutocorrelationHelp.R
#' @export
testTemporalAutocorrelation <- function(simulationOutput, time = NULL , alternative = c("two.sided", "greater", "less"), plot = T){

  simulationOutput = ensureDHARMa(simulationOutput, convert = T)

  # actually not sure if this is neccessary for dwtest, but seems better to aggregate
  if(any(duplicated(time))) stop("testing for temporal autocorrelation requires unique time values - if you have several observations per time value, either use the recalculateResiduals function to aggregate residuals per time step, or extract the residuals from the fitted object, and plot / test each of them independently for temporally repeated subgroups (typical choices would be location / subject etc.). Note that the latter must be done by hand, outside testSpatialAutocorrelation.")

  alternative <- match.arg(alternative)

  if(is.null(time)){
    time = sample.int(simulationOutput$nObs, simulationOutput$nObs)
    message("DHARMa::testTemporalAutocorrelation - no time argument provided, using random times for each data point")
  }

  out = lmtest::dwtest(simulationOutput$scaledResiduals ~ 1, order.by = time, alternative = alternative)

  if(plot == T) {
    oldpar <- par(mfrow = c(1,2))
    on.exit(par(oldpar))

    plot(simulationOutput$scaledResiduals[order(time)] ~ time[order(time)],
         type = "l", ylab = "Scaled residuals", xlab = "Time", main = "Residuals vs. time")
    acf(simulationOutput$scaledResiduals[order(time)], main = "Autocorrelation")
    legend("topright",
           c(paste(out$method, " p=", round(out$p.value, digits = 5)),
             paste("Deviation ", ifelse(out$p.value < 0.05, "significant", "n.s."))),
           text.col = ifelse(out$p.value < 0.05, "red", "black" ), bty="n")

  }

  return(out)
}


#' Test for spatial autocorrelation
#'
#' This function performs a standard test for spatial autocorrelation on the simulated residuals
#'
#' @param simulationOutput an object of class DHARMa with simulated quantile residuals, either created via \code{\link{simulateResiduals}} or by \code{\link{createDHARMa}} for simulations created outside DHARMa
#' @param x the x coordinate, in the same order as the data points. If not provided, random values will be created
#' @param y the y coordinate, in the same order as the data points. If not provided, random values will be created
#' @param distMat optional distance matrix. If not provided, a distance matrix will be calculated based on x and y. See details for explanation
#' @param alternative a character string specifying whether the test should test if observations are "greater", "less" or "two.sided" compared to the simulated null hypothesis
#' @param plot whether to plot output
#' @details The function performs Moran.I test from the package ape, based on the provided distance matrix of the data points.
#'
#' There are several ways to specify this distance. If a distance matrix (distMat) is provided, calculations will be based on this distance matrix, and x,y coordinates will only used for the plotting (if provided)
#' If distMat is not provided, the function will calculate the euclidian distances between x,y coordinates, and test Moran.I based on these distances.
#'
#' If no x/y values are provided, random values will be created. The sense of being able to run the test with x/y = NULL (random values) is to test the rate of false positives under the current residual structure (random x/y corresponds to H0: no spatial autocorrelation), e.g. to check if the test has nominal error rates for particular residual structures.
#'
#' Testing for spatial autocorrelation requires unique x,y values - if you have several observations per location, either use the recalculateResiduals function to aggregate residuals per location, or extract the residuals from the fitted object, and plot / test each of them independently for spatially repeated subgroups (a typical scenario would repeated spatial observation, in which case one could plot / test each time step separately for temporal autocorrelation). Note that the latter must be done by hand, outside testSpatialAutocorrelation.
#'
#' @note Important to note for all autocorrelation tests (spatial / temporal): the autocorrelation tests are valid to check for residual autocorrelation in models that don't assume such a correlation (in this case, you can use conditional or unconditional simulations), or if there is remaining residual autocorrelation after accounting for it in a spatial/temporal model (in that case, you have to use conditional simulations), but if checking unconditional simulations from a model with an autocorrelation structure on data that corresponds to this model, they will be significant, even if the model fully accounts for this structure.
#'
#' This behavior is not really a bug, but rather originates from the definition of the quantile residuals: quantile residuals are calculated independently per data point, i.e. without consideratin of any correlation structure between data points that may exist in the simulations. As a result, the simulated distributions from a unconditional simulaton will typically not reflect the correlation structure that is present in each single simulation, and the same is true for the subsequently calculated quantile residuals.
#'
#' The bottomline here is that spatial / temporal / other autoregressive models should either be tested based on conditional simulations, or (ideally) custom tests should be used that are not based on quantile residuals, but rather compare the correlation structure in the simulated data with the correlation structure in the observed data.
#'
#' @author Florian Hartig
#' @seealso \code{\link{testResiduals}}, \code{\link{testUniformity}}, \code{\link{testOutliers}}, \code{\link{testDispersion}}, \code{\link{testZeroInflation}}, \code{\link{testGeneric}}, \code{\link{testTemporalAutocorrelation}}, \code{\link{testQuantiles}}
#' @import grDevices
#' @example inst/examples/testSpatialAutocorrelationHelp.R
#' @export
testSpatialAutocorrelation <- function(simulationOutput, x = NULL, y  = NULL, distMat = NULL, alternative = c("two.sided", "greater", "less"), plot = T){

  alternative <- match.arg(alternative)
  data.name = deparse(substitute(simulationOutput)) # needs to be before ensureDHARMa
  simulationOutput = ensureDHARMa(simulationOutput, convert = T)

  if(any(duplicated(cbind(x,y)))) stop("testing for spatial autocorrelation requires unique x,y values - if you have several observations per location, either use the recalculateResiduals function to aggregate residuals per location, or extract the residuals from the fitted object, and plot / test each of them independently for spatially repeated subgroups (a typical scenario would repeated spatial observation, in which case one could plot / test each time step separately for temporal autocorrelation). Note that the latter must be done by hand, outside testSpatialAutocorrelation.")

  if( (!is.null(x) | !is.null(y)) & !is.null(distMat) ) message("both coordinates and distMat provided, calculations will be done based on the distance matrix, coordinates will only be used for plotting")
  # if not provided, fill x and y with random numbers (Null model)
  if(is.null(x)){
    x = runif(simulationOutput$nObs, -1,1)
    message("DHARMa::testSpatialAutocorrelation - no x coordinates provided, using random values for each data point")
  }

  if(is.null(y)){
    y = runif(simulationOutput$nObs, -1,1)
    message("DHARMa::testSpatialAutocorrelation - no x coordinates provided, using random values for each data point")
  }

  # if not provided, create distance matrix based on x and y
  if(is.null(distMat)) distMat <- as.matrix(dist(cbind(x, y)))

  invDistMat <- 1/distMat
  diag(invDistMat) <- 0

  MI = ape::Moran.I(simulationOutput$scaledResiduals, weight = invDistMat, alternative = alternative)

  out = list()
  out$statistic = c(observed = MI$observed, expected = MI$expected, sd = MI$sd)
  out$method = "DHARMa Moran's I test for spatial autocorrelation"
  out$alternative = "Spatial autocorrelation"
  out$p.value = MI$p.value
  out$data.name = data.name

  class(out) = "htest"



  if(plot == T) {
    opar <- par(mfrow = c(1,1))
    on.exit(par(opar))

    col = colorRamp(c("red", "white", "blue"))(simulationOutput$scaledResiduals)
    plot(x,y, col = rgb(col, maxColorValue = 255), main = out$method, cex.main = 0.8 )

    # TODO implement correlogram
  }

  if(plot == T) {


  }
  return(out)
}


getP <- function(simulated, observed, alternative){

  if(alternative == "greater") p = mean(simulated >= observed)
  if(alternative == "less") p = mean(simulated <= observed)
  if(alternative == "two.sided") p = min(min(mean(simulated <= observed), mean(simulated >= observed) ) * 2,1)

  return(p)
}



####################### tests.R

####################### compatibility.R


# New S3 methods

#' Get model response
#'
#' Extract the response of a fitted model
#'
#' The purpose of this function is to savely extract the response (dependent variable) of the fitted model classes
#'
#' @param object a fitted model
#' @param ... additional parameters
#'
#' @example inst/examples/wrappersHelp.R
#'
#' @seealso \code{\link{getRefit}}, \code{\link{getSimulations}}, \code{\link{getFixedEffects}}, \code{\link{getFitted}}
#' @author Florian Hartig
#' @export
getObservedResponse <- function (object, ...) {
  UseMethod("getObservedResponse", object)
}

#' @rdname getObservedResponse
#' @export
getObservedResponse.default <- function (object, ...){
  out = model.frame(object)[,1]

  # check for weights in k/n case
  if(family(object)$family %in% c("binomial", "betabinomial") & "(weights)" %in% colnames(model.frame(object))){
    x = model.frame(object)
    out = out * x$`(weights)`
  }

  # check for k/n binomial
  if(is.matrix(out)){
    if(!(ncol(out) == 2)) securityAssertion("nKcase - wrong dimensions of response")
    if(!(family(object)$family %in% c("binomial", "betabinomial"))) securityAssertion("nKcase - wrong family")

    out = out[,1]
  }

  # observation is factor - unlike lme4 and older, glmmTMB simulates nevertheless as numeric
  if(is.factor(out)) out = as.numeric(out) - 1

  return(out)
}

weightsWarning = "Model was fit with prior weights. These will be ignored in the simulation. See ?getSimulations for details"

#' Get model simulations
#'
#' Wrapper to simulate from a fitted model
#'
#' The purpose of this wrapper for for the simulate function is to return the simulations from a model in a standardized way
#'
#' @param object a fitted model
#' @param nsim number of simulations
#' @param type if simulations should be prepared for getQuantile or for refit
#' @param ... additional parameters to be passed on, usually to the simulate function of the respective model class
#'
#' @return a matrix with simulations
#' @example inst/examples/wrappersHelp.R
#'
#' @seealso \code{\link{getObservedResponse}}, \code{\link{getRefit}}, \code{\link{getFixedEffects}}, \code{\link{getFitted}}
#'
#' @details The function is a wrapper for for the simulate function is to return the simulations from a model in a standardized way.
#'
#' Note: if the model was fit with weights, the function will throw a warning if used with a model class whose simulate function does not include the weightsi in the simulations. Note that the results may or may not be appropriate in this case, depending on how you use the weights.
#'
#'
#' @author Florian Hartig
#' @export
getSimulations <- function (object, nsim = 1 , type = c("normal", "refit"), ...) {
  UseMethod("getSimulations", object)
}

#' @rdname getSimulations
#' @export
getSimulations.default <- function (object, nsim = 1, type = c("normal", "refit"), ...){

  type <- match.arg(type)

  out = simulate(object, nsim = nsim , ...)

  if (type == "normal"){
    if(family(object)$family %in% c("binomial", "betabinomial")){
      if("(weights)" %in% colnames(model.frame(object))){
        x = model.frame(object)
        out = out * x$`(weights)`
      } else if (is.matrix(out[[1]])){
        # this is for the k/n binomial case
        out = as.matrix(out)[,seq(1, (2*nsim), by = 2)]
      } else if(is.factor(out[[1]])){
        if(nlevels(out[[1]]) != 2){
          warning("The fitted model has a factorial response with number of levels not equal to 2 - there is currently no sensible application in DHARMa that would lead to this situation. Likely, you are trying something that doesn't work.")
        }
        else{
          out = data.matrix(out) - 1
        }
      }
    }

    if(!is.matrix(out)) out = data.matrix(out)
  } else{
    if(family(object)$family %in% c("binomial", "betabinomial")){
      if (!is.matrix(out[[1]]) & !is.numeric(out)) data.frame(data.matrix(out)-1)
    }
  }

  return(out)
}


#' Extract fixed effects of a supported model
#'
#' A wrapper to extract fixed effects of a supported model
#'
#' @param fittedModel a fitted model
#'
#' @example inst/examples/wrappersHelp.R
#'
#' @importFrom lme4 fixef
#'
#' @seealso \code{\link{getObservedResponse}}, \code{\link{getSimulations}}, \code{\link{getRefit}}, \code{\link{getFitted}}
#' @export
getFixedEffects <- function(fittedModel){

  if(class(fittedModel)[1] %in% c("glm", "lm", "gam", "bam", "negbin") ){
    out  = coef(fittedModel)
  } else if(class(fittedModel)[1] %in% c("glmerMod", "lmerMod", "HLfit")){
    out = fixef(fittedModel)
  } else if(class(fittedModel)[1] %in% c("glmmTMB")){
    out = glmmTMB::fixef(fittedModel)
    out = out$cond
  } else {
    out = coef(fittedModel)
    if(is.null(out)) out = fixef(fittedModel)
  }
  return(out)
}

#' Get model refit
#'
#' Wrapper to refit a fitted model
#'
#' @param object a fitted model
#' @param newresp the new response that should be used to refit the model
#' @param ... additional parameters to be passed on to the refit or update class that is used to refit the model
#'
#' @details The purpose of this wrapper is to standardize the refit of a model. The behavior of this function depends on the supplied model. When available, it uses the refit method, otherwise it will use update. For glmmTMB: since version 1.0, glmmTMB has a refit function, but this didn't work, so I switched back to this implementation, which is a hack based on the update function.
#'
#' @example inst/examples/wrappersHelp.R
#'
#' @seealso \code{\link{getObservedResponse}}, \code{\link{getSimulations}}, \code{\link{getFixedEffects}}
#' @author Florian Hartig
#' @export
getRefit <- function (object, newresp, ...) {
  UseMethod("getRefit", object)
}

#' @rdname getRefit
#'
#' @importFrom lme4 refit
#'
#' @export
getRefit.default <- function (object, newresp, ...){
  refit(object, newresp, ...)
}

#' Get model fitted
#'
#' Wrapper to get the fitted value a fitted model
#'
#' The purpose of this wrapper is to standardize extract the fitted values
#'
#' @param object a fitted model
#' @param ... additional parameters to be passed on, usually to the simulate function of the respective model class
#'
#' @example inst/examples/wrappersHelp.R
#'
#' @seealso \code{\link{getObservedResponse}}, \code{\link{getSimulations}}, \code{\link{getRefit}}, \code{\link{getFixedEffects}}
#'
#' @author Florian Hartig
#' @export
getFitted <- function (object, ...) {
  UseMethod("getFitted", object)
}

#' @rdname getFitted
#' @export
getFitted.default <- function (object,...){
  fitted(object, ...)
}

#' has NA
#'
#' checks if the fitted model excluded NA values
#'
#' @param object a fitted model
#'
#' @details Checks if the fitted model excluded NA values
#'
#' @export


# hasNA <- function(object){
#   x = rownames(model.frame(object))
#   if(length(x) < as.numeric(x[length(x) ])) return(TRUE)
#   else return(FALSE)
# }

######### LM #############

#' @rdname getRefit
#' @export
getRefit.lm <- function(object, newresp, ...){

  newData <-model.frame(object)

  if(is.vector(newresp)){
    newData[,1] = newresp
  } else if (is.factor(newresp)){
    # Hack to make the factor binomial case work
    newData[,1] = as.numeric(newresp) - 1
  } else {
    # Hack to make the binomial n/k case work
    newData[[1]] = NULL
    newData = cbind(newresp, newData)
  }

  refittedModel = update(object, data = newData)
  return(refittedModel)
}


hasWeigths.lm <- function(object, ...){
  if(length(unique(object$prior.weights)) != 1) return(TRUE)
  else return(FALSE)
}


######### GLM #############

#' @rdname getSimulations
#' @export
getSimulations.negbin<- function (object, nsim = 1, type = c("normal", "refit"), ...){
  if("(weights)" %in% colnames(model.frame(object))) warning(weightsWarning)
  getSimulations.default(object = object, nsim = nsim, type = type, ...)
}


######## MGCV ############

# This function overwrites the standard fitted function for GAM

#' @rdname getFitted
#' @export
getFitted.gam <- function(object, ...){
  class(object) = "glm"
  out = stats::fitted(object, ...)
  names(out) = as.character(1:length(out))
  out
}

# Check that this works
# plot(fitted(fittedModelGAM), predict(fittedModelGAM, type = "response"))


######## lme4 ############


#' @rdname getSimulations
#' @export
getSimulations.lmerMod <- function (object, nsim = 1, type = c("normal", "refit"), ...){

  if("(weights)" %in% colnames(model.frame(object))) warning(weightsWarning)
  getSimulations.default(object = object, nsim = nsim, type = type, ...)
}


######## glmmTMB ######

#' @rdname getRefit
#' @export
getRefit.glmmTMB <- function(object, newresp, ...){
  newData <-model.frame(object)

  # hack to make update work - for some reason, glmmTMB wants the matrix embedded in the df for update to work  ... should be solved ideally, see https://github.com/glmmTMB/glmmTMB/issues/549
  if(is.matrix(newresp)){
    tmp = colnames(newData[[1]])
    newData[[1]] = NULL
    newData = cbind(newresp, newData)
    colnames(newData)[1:2] = tmp
  } else {
    newData[[1]] = newresp
  }
  refittedModel = update(object, data = newData)
  return(refittedModel)
}


# glmmTMB simulates normal counts (and not proportions in any case, so the check for the other models is not needed), see #158
# note that if observation is factor - unlike lme4 and older, glmmTMB simulates nevertheless as numeric

#' @rdname getSimulations
#' @export
getSimulations.glmmTMB <- function (object, nsim = 1, type = c("normal", "refit"), ...){

  if("(weights)" %in% colnames(model.frame(object)) & ! family(object)$family %in% c("binomial", "betabinomial")) warning(weightsWarning)

  type <- match.arg(type)

  out = simulate(object, nsim = nsim, ...)

  if (type == "normal"){
    if (is.matrix(out[[1]])){
      # this is for the k/n binomial case
      out = as.matrix(out)[,seq(1, (2*nsim), by = 2)]
    }
    if(!is.matrix(out)) out = data.matrix(out)
  }else{

    # check for weights in k/n case
    if(family(object)$family %in% c("binomial", "betabinomial")){
      if("(weights)" %in% colnames(model.frame(object))){
        w = model.frame(object)
        w = w$`(weights)`
        tmp <- function(x)x/w
        out = apply(out, 2, tmp)
        out = as.data.frame(out)
      }
      else if(is.matrix(out[[1]]) & !is.matrix(model.frame(object)[[1]])){
        out = as.data.frame(as.matrix(out)[,seq(1, (2*nsim), by = 2)])
      }
    }






    # matrixResp =
    #
    # if(matrixResp & !is.null(ncol(newresp))){
    #   # Hack to make the factor binomial case work
    #   tmp = colnames(newData[[1]])
    #   newData[[1]] = NULL
    #   newData = cbind(newresp, newData)
    #   colnames(newData)[1:2] = tmp
    # } else if(!is.null(ncol(newresp))){
    #   newData[[1]] = newresp[,1]
    # } else {
    #   newData[[1]] = newresp
    # }


    # if (out$modelClass == "glmmTMB" & ncol(simulations) == 2*n) simObserved = simulations[,(1+(2*(i-1))):(2+(2*(i-1)))]
  }

  # else securityAssertion("Simulation results produced unsupported data structure", stop = TRUE)

  return(out)
}

#######  spaMM #########

#' @rdname getObservedResponse
#' @export
getObservedResponse.HLfit <- function(object, ...){
  out = spaMM::response(object, ...)

  nKcase = is.matrix(out)
  if(nKcase){
    if(! (family(object) %in% c("binomial", "betabinomial"))) securityAssertion("nKcase - wrong family")
    if(! (ncol(out)==2)) securityAssertion("nKcase - wrong dimensions of response")
    out = out[,1]
  }

  if(!is.numeric(out)) out = as.numeric(out)

  return(out)

}

#' @rdname getSimulations
#' @export
getSimulations.HLfit <- function(object, nsim = 1, type = c("normal", "refit"), ...){

  type <- match.arg(type)

  capture.output({out = simulate(object, nsim = nsim, ...)})

  if(type == "normal"){
    if(!is.matrix(out)) out = data.matrix(out)
  }else{
    out = as.data.frame(out)
  }
  return(out)
}

#' @rdname getRefit
#' @export
getRefit.HLfit <- function(object, newresp, ...) {
  spaMM::update_resp(object, newresp, evaluate = TRUE)
}

####################### compatibility.R

####################### helper.R

#' Modified ECDF function
#'
#' @details ensures symmetric ECDF (standard ECDF is <), and that 0 / 1 values are only produced if the data is strictly < > than the observed data
#'
#' @keywords internal
DHARMa.ecdf <- function (x)
{
  x <- sort(x)
  n <- length(x)
  if (n < 1)
    stop(paste("DHARMa.ecdf - length vector < 1", x))
  vals <- unique(x)
  rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/ (n +1),
                    method = "linear", yleft = 0, yright = 1, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  assign("nobs", n, envir = environment(rval))
  attr(rval, "call") <- sys.call()
  rval
}



#' calculate quantiles
#'
#' calculates residual quantiles from a given simulation
#'
#' @param simulations a matrix with simulations from a fitted model. Rows = observations, columns = replicate simulations
#' @param observed a vector with the observed data
#' @param integerResponse is the response integer-valued. Only has an effect for method = "traditional"
#' @param method the quantile randomization method used. See details
#'
#' @details The function calculates residual quantiles from the simulated data. For continous distributions, this will simply the the value of the ecdf.
#'
#' For discrete data, there are two options implemented.
#'
#' The current default (available since DHARMa 0.3.1) are probability integral transform (PIT-) residuals (Smith, 1985; Dunn & Smyth, 1996; see also see also Warton, et al., 2017).
#'
#' Before DHARMa 0.3.1, a different randomization procedure was used, in which the a U(-0.5, 0.5) distribution was added on observations and simualtions for discrete distributions. For a completely discrete distribution, the two procedures should deliver equivalent results, but the second method has the disadvantage that a) one has to know if the distribution is discrete (DHARMa tries to recognize this automatically), and b) that it leads to inefficiencies for some distributions such as the the Tweedie, which are partly continous, partly discrte (see e.g. https://github.com/florianhartig/DHARMa/issues/168).
#'
#' @references
#'
#' Smith, J. Q. "Diagnostic checks of non-standard time series models." Journal of Forecasting 4.3 (1985): 283-291.
#'
#' Dunn, P.K., & Smyth, G.K. (1996). Randomized quantile residuals. Journal of Computational and Graphical Statistics 5, 236-244.
#'
#' Warton, David I., Loïc Thibaut, and Yi Alice Wang. "The PIT-trap—A “model-free” bootstrap procedure for inference about regression models with discrete, multivariate responses." PloS one 12.7 (2017)
#'
#' @export
getQuantile <- function(simulations, observed, integerResponse, method = c("PIT", "traditional")){

  method = match.arg(method)

  n = length(observed)
  if (nrow(simulations) != n) stop("DHARMa::getquantile: wrong dimension of simulations")
  nSim = ncol(simulations)


  if(method == "traditional"){

    if(integerResponse == F){

      if(any(duplicated(observed))) message("Model family was recognized or set as continuous, but duplicate values were detected in the response. Consider if you are fitting an appropriate model.")

      values = as.vector(simulations)[duplicated(as.vector(simulations))]
      if(length(values) > 0){
        if (all(values%%1==0)){
          integerResponse = T
          message("Model family was recognized or set as continuous, but duplicate values were detected in the simulation - changing to integer residuals (see ?simulateResiduals for details)")
        } else {
          message("Duplicate non-integer values found in the simulation. If this is because you are fitting a non-inter valued discrete response model, note that DHARMa does not perform appropriate randomization for such cases.")
        }

      }
    }

    scaledResiduals = rep(NA, n)
    for (i in 1:n){
      if(integerResponse == T){
        scaledResiduals[i] <- DHARMa.ecdf(simulations[i,] + runif(nSim, -0.5, 0.5))(observed[i] + runif(1, -0.5, 0.5))
      }else{
        scaledResiduals[i] <- DHARMa.ecdf(simulations[i,])(observed[i])
      }
    }

  } else {


    scaledResiduals = rep(NA, n)
    for (i in 1:n){
      min <- sum(simulations[i,] < observed[i]) / length(simulations[i,])
      max <- sum(simulations[i,] <= observed[i]) / length(simulations[i,])
      if (min == max) scaledResiduals[i] = DHARMa.ecdf(simulations[i,])(observed[i])
      else{
        scaledResiduals[i] = runif(1, min, max)
      }
    }
  }

  return(scaledResiduals)
}

#
#
# testData = createData(sampleSize = 200, family = gaussian(),
#                       randomEffectVariance = 0, numGroups = 5)
# fittedModel <- glmmTMB(observedResponse ~ Environment1,
#                    data = testData)
# simulationOutput <- simulateResiduals(fittedModel = fittedModel)
#
# sims = simulationOutput$simulatedResponse
# sims[1, c(1,6,8)] = 0
# any(apply(sims, 1, anyDuplicated))
# getQuantile(simulations = sims, observed = testData$observedResponse, n = 200, integerResponse = F, nSim = 250)
#
#
#



#' Check dot operator
#'
#' @param name variable name
#' @param default variable default
#'
#' @details modified from https://github.com/lcolladotor/dots
#'
#' @keywords internal
checkDots <- function(name, default, ...) {
  args <- list(...)
  if(!name %in% names(args)) {
    ## Default value
    return(default)
  } else {
    ## If the argument was defined in the ... part, return it
    return(args[[name]])
  }
}


securityAssertion <- function(context = "Not provided", stop = F){
  generalMessage = "Message from DHARMa: During the execution of a DHARMa function, some unexpected conditions occurred. Even if you didn't get an error, your results may not be reliable. Please check with the help if you use the functions as intended. If you think that the error is not on your side, I would be grateful if you could report the problem at https://github.com/florianhartig/DHARMa/issues \n\n Context:"
  if (stop == F) warning(paste(generalMessage, context))
  else stop(paste(generalMessage, context))
}

####################### helper.R

####################### plot.R

#' DHARMa standard residual plots
#'
#' This function creates standard plots for the simulated residuals
#' @param x an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param rank if T (default), the values of pred will be rank transformed. This will usually make patterns easier to spot visually, especially if the distribution of the predictor is skewed.
#' @param ... further options for \code{\link{plotResiduals}}. Consider in particular parameters quantreg, rank and asFactor. xlab, ylab and main cannot be changed when using plotSimulatedResiduals, but can be changed when using plotResiduals.
#' @details The function creates two plots. To the left, a qq-uniform plot to detect deviations from overall uniformity of the residuals (calling \code{\link{plotQQunif}}), and to the right, a plot of residuals against predicted values (calling \code{\link{plotResiduals}}). Outliers are highlighted in red (for more on outliers, see \code{\link{testOutliers}}). For a correctly specified model, we would expect
#'
#' a) a straight 1-1 line in the uniform qq-plot -> evidence for an overall uniform (flat) distribution of the residuals
#'
#' b) uniformity of residuals in the vertical direction in the res against predictor plot
#'
#' Deviations of this can be interpreted as for a linear regression. See the vignette for detailed examples.
#'
#' To provide a visual aid in detecting deviations from uniformity in y-direction, the plot of the residuals against the predicted values also performs an (optional) quantile regression, which provides 0.25, 0.5 and 0.75 quantile lines across the plots. These lines should be straight, horizontal, and at y-values of 0.25, 0.5 and 0.75. Note, however, that some deviations from this are to be expected by chance, even for a perfect model, especially if the sample size is small. See further comments on this plot, its interpretation and options, in \code{\link{plotResiduals}}
#'
#' The quantile regression can take some time to calculate, especially for larger datasets. For that reason, quantreg = F can be set to produce a smooth spline instead. This is default for n > 2000.
#'
#' @seealso \code{\link{plotResiduals}}, \code{\link{plotQQunif}}
#' @example inst/examples/plotsHelp.R
#' @import graphics
#' @import utils
#' @export
plot.DHARMa <- function(x, rank = TRUE, ...){

  oldpar <- par(mfrow = c(1,2), oma = c(0,1,2,1))
  on.exit(par(oldpar))

  plotQQunif(x)
  plotResiduals(x, rank = rank, ...)

  mtext("DHARMa residual diagnostics", outer = T)
}


#' Histogram of DHARMa residuals
#'
#' The function produces a histogram from a DHARMa output
#'
#' @param x a DHARMa simulation output (class DHARMa)
#' @param breaks breaks for hist() function
#' @param col col for hist bars
#' @param main plot main
#' @param xlab plot xlab
#' @param cex.main plot cex.main
#' @param ... other arguments to be passed on to hist
#' @seealso \code{\link{plotSimulatedResiduals}}, \code{\link{plotResiduals}}
#' @example inst/examples/plotsHelp.R
#' @export
hist.DHARMa <- function(x,
                        breaks = seq(-0.02, 1.02, len = 53),
                        col = c("red",rep("lightgrey",50), "red"),
                        main = "Hist of DHARMa residuals",
                        xlab = "Residuals (outliers are marked red)",
                        cex.main = 1,
                        ...){

  x = ensureDHARMa(x, convert = T)

  val = x$scaledResiduals
  val[val == 0] = -0.01
  val[val == 1] = 1.01

  hist(val, breaks = breaks, col = col, main = main, xlab = xlab, cex.main = cex.main, ...)
}


#' DHARMa standard residual plots
#'
#' DEPRECATED, use plot() instead
#'
#' @param simulationOutput an object with simulated residuals created by \code{\link{simulateResiduals}}
#' @param ... further options for \code{\link{plotResiduals}}. Consider in particular parameters quantreg, rank and asFactor. xlab, ylab and main cannot be changed when using plotSimulatedResiduals, but can be changed when using plotResiduals.
#' @note This function is deprecated. Use \code{\link{plot.DHARMa}}
#'
#' @seealso \code{\link{plotResiduals}}, \code{\link{plotQQunif}}
#' @export
plotSimulatedResiduals <- function(simulationOutput, ...){
  message("plotSimulatedResiduals is deprecated, please switch your code to simply using the plot() function")
  plot(simulationOutput, ...)
}


#' Quantile-quantile plot for a uniform distribution
#'
#' The function produces a uniform quantile-quantile plot from a DHARMa output
#'
#' @param simulationOutput a DHARMa simulation output (class DHARMa)
#' @param testUniformity if T, the function \code{\link{testUniformity}} will be called and the result will be added to the plot
#' @param testOutliers if T, the function \code{\link{testOutliers}} will be called and the result will be added to the plot
#' @param testDispersion if T, the function \code{\link{testDispersion}} will be called and the result will be added to the plot
#' @param ... arguments to be passed on to \code{\link[gap]{qqunif}}
#'
#' @details the function calls qqunif from the R package gap to create a quantile-quantile plot for a uniform distribution.
#' @seealso \code{\link{plotSimulatedResiduals}}, \code{\link{plotResiduals}}
#' @example inst/examples/plotsHelp.R
#' @export
plotQQunif <- function(simulationOutput, testUniformity = T, testOutliers = T, testDispersion = T, ...){

  simulationOutput = ensureDHARMa(simulationOutput, convert = "Model")

  gap::qqunif(simulationOutput$scaledResiduals,pch=2,bty="n", logscale = F, col = "black", cex = 0.6, main = "QQ plot residuals", cex.main = 1, ...)

  if(testUniformity == TRUE){
    temp = testUniformity(simulationOutput, plot = F)
    legend("topleft", c(paste("KS test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")
  }

  if(testOutliers == TRUE){
    temp = testOutliers(simulationOutput, plot = F)
    legend("bottomright", c(paste("Outlier test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")
  }

  if(testDispersion == TRUE){
    temp = testDispersion(simulationOutput, plot = F)
    legend("center", c(paste("Dispersion test: p=", round(temp$p.value, digits = 5)), paste("Deviation ", ifelse(temp$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(temp$p.value < 0.05, "red", "black" ), bty="n")
  }

}


#' Generic res ~ pred scatter plot with spline or quantile regression on top
#'
#' The function creates a generic residual plot with either spline or quantile regression to highlight patterns in the residuals. Outliers are highlighted in red.
#'
#' @param simulationOutput on object, usually a DHARMa object, from which residual values can be extracted. Alternatively, a vector with residuals or a fitted model can be provided, which will then be transformed into a DHARMa object.
#' @param form optional predictor against which the residuals should be plotted. Default is to used the predicted(simulationOutput)
#' @param quantreg whether to perform a quantile regression on 0.25, 0.5, 0.75 on the residuals. If F, a spline will be created instead. Default NULL chooses T for nObs < 2000, and F otherwise.
#' @param rank if T, the values provided in form will be rank transformed. This will usually make patterns easier to spot visually, especially if the distribution of the predictor is skewed. If form is a factor, this has no effect.
#' @param asFactor should a numeric predictor provided in form be treated as a factor. Default is to choose this for < 10 unique values, as long as enough predictions are available to draw a boxplot.
#' @param smoothScatter if T, a smooth scatter plot will plotted instead of a normal scatter plot. This makes sense when the number of residuals is very large. Default NULL chooses T for nObs < 10000, and F otherwise.
#' @param quantiles for a quantile regression, which quantiles should be plotted
#' @param ... additional arguments to plot / boxplot.
#' @details The function plots residuals against a predictor (by default against the fitted value, extracted from the DHARMa object, or any other predictor).
#'
#' Outliers are highlighted in red (for information on definition and interpretation of outliers, see \code{\link{testOutliers}}).
#'
#' To provide a visual aid in detecting deviations from uniformity in y-direction, the plot function calculates an (optional) quantile regression, which compares the empirical 0.25, 0.5 and 0.75 quantiles (default) in y direction (red solid lines) with the theoretical 0.25, 0.5 and 0.75 quantiles (dashed black line).
#'
#' Asymptotically (i.e. for lots of data / residuals), if the model is correct, theoretical and the empirical quantiles should be identical (i.e. dashed and solid lines should match). A p-value for the deviation is calculated for each quantile line. Significant deviations are highlighted by red color.
#'
#' If form is a factor, a boxplot will be plotted instead of a scatter plot. The distribution for each factor level should be uniformly distributed, so the box should go from 0.25 to 0.75, with the median line at 0.5. Again, chance deviations from this will increases when the sample size is smaller. You can run null simulations to test if the deviations you see exceed what you would expect from random variation. If you want to create box plots for categorical predictors (e.g. because you only have a small number of unique numeric predictor values), you can convert your predictor with as.factor(pred)
#' 
#' @return if quantile tests are performed, the function returns them invisibly.
#'
#' @note The quantile regression can take some time to calculate, especially for larger datasets. For that reason, quantreg = F can be set to produce a smooth spline instead.
#'
#' @seealso \code{\link{plotQQunif}}
#' @example inst/examples/plotsHelp.R
#' @export
plotResiduals <- function(simulationOutput, form = NULL, quantreg = NULL, rank = F, asFactor = NULL, smoothScatter = NULL, quantiles = c(0.25, 0.5, 0.75), ...){


  ##### Checks #####


  a <- list(...)
  a$ylab = checkDots("ylab", "Standardized residual", ...)
  if(is.null(form)){
    a$xlab = checkDots("xlab", ifelse(rank, "Model predictions (rank transformed)", "Model predictions"), ...)
  }

  simulationOutput = ensureDHARMa(simulationOutput, convert = T)
  res = simulationOutput$scaledResiduals
  if(inherits(form, "DHARMa"))stop("DHARMa::plotResiduals > argument form cannot be of class DHARMa. Note that the syntax of plotResiduals has changed since DHARMa 0.3.0. See ?plotResiduals.")

  pred = ensurePredictor(simulationOutput, form)

  ##### Rank transform and factor conversion#####

  if(!is.factor(pred)){

    if (rank == T){
      pred = rank(pred, ties.method = "average")
      pred = pred / max(pred)
    }

    nuniq = length(unique(pred))
    ndata = length(pred)
    if(is.null(asFactor)) asFactor = (nuniq == 1) | (nuniq < 10 & ndata / nuniq > 10)
    if (asFactor) pred = factor(pred)
  }

  ##### Residual scatter plots #####

  if(is.null(quantreg)) if (length(res) > 2000) quantreg = FALSE else quantreg = TRUE

  switchScatter = 10000
  if(is.null(smoothScatter)) if (length(res) > switchScatter) smoothScatter = TRUE else smoothScatter = FALSE

  blackcol = rgb(0,0,0, alpha = max(0.1, 1 - 3 * length(res) / switchScatter))


  # categorical plot
  if(is.factor(pred)){
    do.call(plot, append(list(res ~ pred, ylim = c(0,1), axes = FALSE), a))
  }
  # smooth scatter
  else if (smoothScatter == TRUE) {
    defaultCol = ifelse(res == 0 | res == 1, 2,blackcol)
    do.call(graphics::smoothScatter, append(list(x = pred, y = res , ylim = c(0,1), axes = FALSE, colramp = colorRampPalette(c("white", "darkgrey"))),a))
    points(pred[defaultCol == 2], res[defaultCol == 2], col = "red", cex = 0.5)
  }
  # normal plot
  else{
    defaultCol = ifelse(res == 0 | res == 1, 2,blackcol)
    defaultPch = ifelse(res == 0 | res == 1, 8,1)
    a$col = checkDots("col", defaultCol, ...)
    a$pch = checkDots("pch", defaultPch, ...)
    do.call(plot, append(list(res ~ pred, ylim = c(0,1), axes = FALSE), a))
  }

  axis(1)
  axis(2, at=c(0, 0.25, 0.5, 0.75, 1))

  ##### Quantile regressions #####

  main = checkDots("main", "Residual vs. predicted", ...)
  out = NULL

  if(is.numeric(pred)){
    if(quantreg == F){
      title(main = main, cex.main = 1)
      abline(h = c(0.25, 0.5, 0.75), col = "black", lwd = 0.5, lty = 2)
      try({
        lines(smooth.spline(pred, res, df = 10), lty = 2, lwd = 2, col = "red")
        abline(h = 0.5, col = "red", lwd = 2)
      }, silent = T)
    }else{

      out = testQuantiles(simulationOutput, pred, quantiles = quantiles, plot = F)


      if(any(out$pvals < 0.05, na.rm = TRUE)){
        main = paste(main, "Quantile deviations detected (red curves)", sep ="\n")
        if(out$p.value <= 0.05){
          main = paste(main, "Combined adjusted quantile test significant", sep ="\n")
        } else {
          main = paste(main, "Combined adjusted quantile test n.s.", sep ="\n")
        }
        maincol = "red"
      } else {
        main = paste(main, "No significant problems detected", sep ="\n")
        maincol = "black"
      }


      title(main = main, cex.main = 0.8,
            col.main = maincol)

      for(i in 1:length(quantiles)){

        lineCol = ifelse(out$pvals[i] <= 0.05 & !(is.na(out$pvals[i])), "red", "black")
        filCol = ifelse(out$pvals[i] <= 0.05 & !(is.na(out$pvals[i])), "#FF000040", "#00000020")

        abline(h = quantiles[i], col = lineCol, lwd = 0.5, lty = 2)
        polygon(c(out$predictions$pred, rev(out$predictions$pred)),
                c(out$predictions[,2*i] - out$predictions[,2*i+1], rev(out$predictions[,2*i] + out$predictions[,2*i+1])),
                col = "#00000020", border = F)
        lines(out$predictions$pred, out$predictions[,2*i], col = lineCol, lwd = 2)
      }

      # legend("bottomright", c(paste("Quantile test: p=", round(out$p.value, digits = 5)), paste("Deviation ", ifelse(out$p.value < 0.05, "significant", "n.s."))), text.col = ifelse(out$p.value < 0.05, "red", "black" ), bty="n")

    }
  }
  invisible(out)
}

x = 0.01
x <= 0.05 & !(is.na(x))


#' Ensures the existence of a valid predictor to plot residuals against
#'
#' @param simulationOutput a DHARMa simulation output or an object that can be converted into a DHARMa simulation output
#' @param predictor an optional predictor. If no predictor is provided, will try to extract the fitted value
#' @keywords internal
ensurePredictor <- function(simulationOutput,
                            predictor = NULL){
  if(!is.null(predictor)){

    if(length(predictor) != length(simulationOutput$scaledResiduals)) stop("DHARMa: residuals and predictor do not have the same length. The issue is possibly that you have NAs in your predictor that were removed during the model fit. Remove the NA values from your predictor.")
  } else {

    predictor = simulationOutput$fittedPredictedResponse
    if(is.null(predictor)) stop("DHARMa: can't extract predictor from simulationOutput, and no predictor provided")
  }
  return(predictor)
}




#plot(simulationOutput)

#plot(simulationOutput$observedResponse, simulationOutput$scaledResiduals, xlab = "predicted", ylab = "Residual", main = "Residual vs. predicted")

#plot(simulationOutput$observedResponse, simulationOutput$fittedPredictedResponse - simulationOutput$observedResponse)

#plot(cumsum(sort(simulationOutput$scaledResiduals)))


#plotConventionalResiduals(fittedModel)


#' Conventional residual plot
#'
#' Convenience function to draw conventional residual plots
#'
#' @param fittedModel a fitted model object
#' @export
plotConventionalResiduals <- function(fittedModel){
  opar <- par(mfrow = c(1,3), oma = c(0,1,2,1))
  on.exit(par(opar))
  plot(predict(fittedModel), resid(fittedModel, type = "deviance"), main = "Deviance" , ylab = "Residual", xlab = "Predicted")
  plot(predict(fittedModel), resid(fittedModel, type = "pearson") , main = "Pearson", ylab = "Residual", xlab = "Predicted")
  plot(predict(fittedModel), resid(fittedModel, type = "response") , main = "Raw residuals" , ylab = "Residual", xlab = "Predicted")
  mtext("Conventional residual plots", outer = T)
}




#
#
# if(quantreg == F){
#
#   lines(smooth.spline(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals, df = 10), lty = 2, lwd = 2, col = "red")
#
#   abline(h = 0.5, col = "red", lwd = 2)
#
# }else{
#
#   #library(gamlss)
#
#   # qrnn
#
#   # http://r.789695.n4.nabble.com/Quantile-GAM-td894280.html
#
#   #require(quantreg)
#   #dat <- plyr::arrange(dat,pred)
#   #fit<-quantreg::rqss(resid~qss(pred,constraint="N"),tau=0.5,data = dat)
#
#   probs = c(0.25, 0.50, 0.75)
#
#   w <- p <- list()
#   for(i in seq_along(probs)){
#     capture.output(w[[i]] <- qrnn::qrnn.fit(x = as.matrix(simulationOutput$fittedPredictedResponse), y = as.matrix(simulationOutput$scaledResiduals), n.hidden = 4, tau = probs[i], iter.max = 1000, n.trials = 1, penalty = 1))
#     p[[i]] <- qrnn::qrnn.predict(as.matrix(sort(simulationOutput$fittedPredictedResponse)), w[[i]])
#   }
#
#
#
#   #plot(simulationOutput$fittedPredictedResponse, simulationOutput$scaledResiduals, xlab = "Predicted", ylab = "Residual", main = "Residual vs. predicted\n lines should match", cex.main = 1)
#
#   #lines(sort(simulationOutput$fittedPredictedResponse), as.vector(p[[1]]), col = "red")
#
#   matlines(sort(simulationOutput$fittedPredictedResponse), matrix(unlist(p), nrow = length(simulationOutput$fittedPredictedResponse), ncol = length(p)), col = "red", lty = 1)
#
#   #     as.vector(p[[1]])
#   #
#   #
#   #     lines(simulationOutput$fittedPredictedResponse,p[[1]], col = "red", lwd = 2)
#   #     abline(h = 0.5, col = "red", lwd = 2)
#   #
#   #     fit<-quantreg::rqss(resid~qss(pred,constraint="N"),tau=0.25,data = dat)
#   #     lines(unique(dat$pred)[-1],fit$coef[1] + fit$coef[-1], col = "green", lwd = 2, lty =2)
#   #     abline(h = 0.25, col = "green", lwd = 2, lty =2)
#   #
#   #     fit<-quantreg::rqss(resid~qss(pred,constraint="N"),tau=0.75,data = dat)
#   #     lines(unique(dat$pred)[-1],fit$coef[1] + fit$coef[-1], col = "blue", lwd = 2, lty = 2)
#   #     abline(h = 0.75, col = "blue", lwd = 2, lty =2)
# }

####################### plot.R

####################### random.R

#' Record and restore a random state
#' 
#' The aim of this function is to record, manipualate and restor a random state
#' 
#' @details This function is intended for two (not mutually exclusive tasks)
#' 
#' a) record the current random state
#' 
#' b) change the current random state in a way that the previous state can be restored
#' 
#' @return a list with various infos about the random state that after function execution, as well as a function to restore the previous state before the function execution
#' 
#' @param seed seed argument to set.seed(). NULL = no seed, but random state will be restored. F = random state will not be restored
#' @export
#' @example inst/examples/getRandomStateHelp.R
#' @author Florian Hartig
#' 
getRandomState <- function(seed = NULL){
  
  # better to explicitly access the global RS?
  # current = get(".Random.seed", .GlobalEnv, ifnotfound = NULL)
  
  current = mget(".Random.seed", envir = .GlobalEnv, ifnotfound = list(NULL))[[1]]
  
  if(is.logical(seed) & seed == F){
    restoreCurrent <- function(){}    
  }else{
    restoreCurrent <- function(){
      if(is.null(current)) rm(".Random.seed", envir = .GlobalEnv) else assign(".Random.seed", current , envir = .GlobalEnv)
    }    
  }

  # setting seed
  if(is.numeric(seed)) set.seed(seed)

  # ensuring that RNG has been initialized
  if (is.null(current))runif(1) 
  
  randomState = list(seed, state = get(".Random.seed", globalenv()), kind = RNGkind(), restoreCurrent = restoreCurrent)  
  return(randomState)
}

####################### random.R

######################################### Package DHARMa

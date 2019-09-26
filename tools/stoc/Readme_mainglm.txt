Estimate temporal evolution of population per species  - ExeMainGlmGalaxy.r
This script analyse the temporal evolution of species population and create graphical vizualisation.

Script needs the followings inputs :
 - stoc or community data filtered with at least 4 columns: year, site, species, and abundance with 0. Corresponding to "observed" or predicted 0 abundance.
 May come from the tools "Preprocess population data for evolution trend analyzes" (ExemakeTableAnalyseGalaxy.r) followed by "Filter species with rare and low abundances" (ExeFilteringRareLowabundSPGalaxy.r).
 - species details file with name and indicator status file with at least 2 columns: the species name or species ID (found in the community data or in stoc data) and his status as indicator species
 - file that stocks functions : "FunctTrendSTOCGalaxy.r"


Arguments are :
 - spExclude: list of species (using the the species name or ID) that you want to exclude
 - assessIC : compute and show confidence interval in plots (TRUE / FALSE)
 - analysis custom id


How to execute, eg :
 # all files are available in github repo
 #Exec id=mainglm, return IC on plot, no species excluded
 $ Rscript ExeMainGlmGalaxy.r' Datafilteredfortrendanalysis.tabular tabSpecies.csv 'mainglm' '' 'TRUE' FunctTrendSTOCGalaxy.r


Outputs are created in an Output repo :
GLM gives 1 graph per species and 2 tables:
 - nameofspecies_id.png (one plot per species)
 - tendanceGlobalEspece_id.tabular
 - variationsAnnuellesEspece_id.tabular


R library needed
r-lme4  version 1.1.18.1
r-ggplot2  version 3.0.0
r-speedglm  version 0.3.2
r-arm  version 1.10.1
r-reshape  version 0.8.8
r-data.table  version 1.12.0
r-reshape2   version 1.4.3

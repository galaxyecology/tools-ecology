Estimate temporal evolution of population by specialization group - ExeMainglmParGroupGalaxy.r
This script analyse the temporal evolution by group on and create graphical vizualisation.

Script needs the followings inputs :
 - stoc Yearly variation dataset.
 May come from the "Estimate temporal population evoution by species" tool (ExeMainGlmGalaxy.r).
 - stoc global variation dataset.
 May come from the "Estimate temporal population evoution by species" tool (ExeMainGlmGalaxy.r).
 - species details file with name and indicator status file with at least 2 columns: the species name or species ID (found in the community data or in stoc data) and his status as indicator species
 - file that stocks functions : "FunctTrendSTOCGalaxy.r"
 - A stoc bias tabular file


Arguments are :
 - spExclude: list of species (using the the species name or ID) that you want to exclude
 - analysis custom id


How to execute, eg :
 # all files are available in github repo
 #Exec id=mainglm_group, no species excluded
 $ Rscript ExeMainglmParGroupGalaxy.r yearly_variation.tabular global_variation.tabular tabSpecies.csv 'mainglm_group' '' FunctTrendSTOCGalaxy.r biais.tabular

Outputs are created in an Output repo :
GLM gives 3 tables:
 - donneesGroupes_id.tabular
 - variationsAnnuellesGroupes_id.tabular
 - tendancesGlobalesGroupes_id.tabular


R library needed
r-lme4  version 1.1.18.1
r-ggplot2  version 3.0.0
r-speedglm  version 0.3.2
r-arm  version 1.10.1
r-reshape  version 0.8.8
r-data.table  version 1.12.0
r-reshape2   version 1.4.3

#!/bin/Rscript

args = commandArgs(trailingOnly=TRUE)

#lister les option pour future et paléo, source internet ?
#donner une erreur quand une option est incorrecte

library(sdmpredictors)

layers_modern_fun <- function(argument_number, file_number){
  
  data_terrestrial = as.logical(args[argument_number+1])
  data_marine      = as.logical(args[argument_number+2])
  data_freshwater  = as.logical(args[argument_number+3])
  data_monthly     = as.logical(args[argument_number+4])
  data_version     = as.numeric(args[argument_number+5])
  
  if(data_version==0){data_version <- NULL}
  
  datasets <- list_datasets(terrestrial = data_terrestrial
                            , marine = data_marine
                            , freshwater = data_freshwater)
  
  write.table(list_layers(datasets
                         , terrestrial = data_terrestrial
                         , marine = data_marine
                         , freshwater = data_freshwater
                         , monthly = data_monthly
                         , version = data_version
                         ), file = paste(as.character(file_number),"data_modern.tsv", sep = "_"), sep = "\t", row.names=FALSE)
}

layers_future_fun <- function(argument_number, file_number){
  
  data_terrestrial = as.logical(args[argument_number+1])
  data_marine      = as.logical(args[argument_number+2])
  data_freshwater  = as.logical(args[argument_number+3])
  data_monthly     = as.logical(args[argument_number+4])
  data_version     = as.numeric(args[argument_number+5])
  data_scenario    = as.character(args[argument_number+6])
  data_year        = as.numeric(args[argument_number+7])
  
  if(data_version==0){data_version <- NULL}
  if(data_scenario=="None"){data_scenario <- NA}
  if(data_year==0){data_year <- NA}
  
  datasets <- list_datasets(terrestrial = data_terrestrial
                            , marine = data_marine
                            , freshwater = data_freshwater)
                            
  write.table(list_layers_future(datasets
                                , terrestrial = data_terrestrial
                                , marine = data_marine
                                , freshwater = data_freshwater
                                , monthly = data_monthly
                                , version = data_version
                                , scenario = data_scenario
                                , year = data_year
                                ), file = paste(as.character(file_number),"data_future.tsv", sep = "_"), sep = "\t", row.names=FALSE)
}

layers_paleo_fun <- function(argument_number, file_number){
  
  data_terrestrial = as.logical(args[argument_number+1])
  data_marine      = as.logical(args[argument_number+2])
  data_freshwater  = as.logical(args[argument_number+3])
  data_monthly     = as.logical(args[argument_number+4])
  data_version     = as.numeric(args[argument_number+5])
  data_model_name  = as.character(args[argument_number+6])
  data_epoch       = as.character(args[argument_number+7])
  data_years_ago   = as.numeric(args[argument_number+8])
  
  if(data_version==0){data_versio <- NULL}
  if(data_model_name=="None"){data_model_name <- NA}
  if(data_epoch=="None"){data_epoch <- NA}
  if(data_years_ago==0){data_years_ago <- NA}
  
  datasets <- list_datasets(terrestrial = data_terrestrial
                            , marine = data_marine
                            , freshwater = data_freshwater)
                            
  write.table(list_layers_paleo(datasets
                                , terrestrial = data_terrestrial
                                , marine = data_marine
                                , freshwater = data_freshwater
                                , monthly = data_monthly
                                , version = data_versio
                                , model_name = data_model_name
                                , epoch = data_epoch 
                                , years_ago = data_years_ago
                                ), file = paste(as.character(file_number),"data_paleo.tsv", sep = "_"), sep = "\t", row.names=FALSE)
}


if (length(args)<0){stop("not enough arguments")
}else{
  n <- 1
  for (a in 1:length(args)) {
    if (as.character(args[a]) == "layers_modern"){
      layers_modern_fun(a,n)
      n <- n+1
    }

    if (as.character(args[a]) == "layers_future"){
      layers_future_fun(a,n)
      n <- n+1
    }
  
    if (as.character(args[a]) == "layers_paleo"){
      layers_paleo_fun(a,n)
      n <- n+1
    }
}}







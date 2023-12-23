#15/12/2023 #SEGUINEAU Pauline 

#Load packages

library(EMLassemblyline)

#Load arguments

if (length(commandArgs(trailingOnly = TRUE)) > 0) {
    data_raster <- commandArgs(trailingOnly = TRUE)[1]
}

#Transform arguments

raster =  strsplit(data_raster," ")

#Make templates to describe data raster

template_raster_attributes(path = ".",data.path= "data_files", raster.file = raster[[1]])
template_categorical_variables(path = ".", data.path = "data_files")



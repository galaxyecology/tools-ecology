#15/12/2023 #SEGUINEAU Pauline 

#Load packages

library(EMLassemblyline)

#Load arguments

if (length(commandArgs(trailingOnly = TRUE)) > 0) {
    data_vector <- commandArgs(trailingOnly = TRUE)[1]   
}

#Transform arguments

vector =  strsplit(data_vector," ")

#Make templates to describe data raster

template_vector_attributes(path = ".",data.path= "data_files", vector.file = vector[[1]])
template_categorical_variables(path = ".", data.path = "data_files")



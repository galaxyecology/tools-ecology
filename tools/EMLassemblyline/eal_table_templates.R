#28/11/2023 #SEGUINEAU Pauline 

#Load packages

library(EMLassemblyline)

#Load arguments

if (length(commandArgs(trailingOnly = TRUE)) > 0) {
    data_table <- commandArgs(trailingOnly = TRUE)[1]
}

#Transform arguments

table =  strsplit(data_table," ")

#Make templates to describe data tables : Describes columns of a data table (classes, units, datetime formats, missing value codes) + catégorical variables.

template_table_attributes(path = ".",data.path= "data_files", data.table = table[[1]])
template_categorical_variables(path = ".", data.path = "data_files")


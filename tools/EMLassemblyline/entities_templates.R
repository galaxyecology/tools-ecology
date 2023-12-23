#19/12/2023 #SEGUINEAU Pauline 

#Load arguments

if (length(commandArgs(trailingOnly = TRUE)) > 0) {
    data_objects <- commandArgs(trailingOnly = TRUE)[1]
}

#Transform arguments

data =  strsplit(data_objects," ")[[1]]

#Make entities templates for raster and/or vector and/or other entities data

EMLassemblyline::template_entities(path="output_template",data.path="data_files",data.objects=data)



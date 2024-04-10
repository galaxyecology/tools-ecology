#22/11/2023 #SEGUINEAU Pauline 

###First tool of EML Workflow

#Load packages

library(EMLassemblyline)

#Load arguments

args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  
  license <- args[1]
  file_type <- args[2]
}

#Make templates to describe core features of a data package (abstract, methods, keywords, personnel, license). 

template_core_metadata(path=".",license = license, file.type = file_type)




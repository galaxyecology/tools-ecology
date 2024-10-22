#17/07/2024
#Seguineau Pauline

#Make annotation template
#Load packages

library(EMLassemblyline)

#Load arguments

args = commandArgs(trailingOnly=TRUE)

if(length(args)>0){
  prod <- args[1]
  if (prod == "fr_scratch"){
      select <- args[2]
      data_table <- args[3]
      if(select == "data_table"){
          data_table <- args[3]
      }else if(select == "other_entities"){
          other_entities <- args[3]
      }else if(select == "both"){
          data_table <- args[3]
          other_entities <- args[4]}
  }else{
      eml <- args[2]
  }
}

#Transform arguments

if(prod =="fr_scratch" && select == "both"){
     table = strsplit(data_table," ")
     other = strsplit(other_entities," ")

#Make templates to add annotations from scratch

     template_annotations(
          path ="data_files/",
          data.table = table[[1]],
          other.entity = other[[1]]) 
          
}else if (prod == "fr_scratch" && select == "data_table"){

     table = strsplit(data_table," ")
     template_annotations(
          path ="data_files/",
          data.table = table[[1]]) 

}else if (prod == "fr_scratch" && select == "other_entities"){

     other = strsplit(other_entities," ")
     template_annotations(
          path ="data_files/",
          other.entity = other[[1]]) 

#Make templates to add annotations from EML file

}else{
     template_annotations(
          path= "data_files/",
          eml = eml)}
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          
          

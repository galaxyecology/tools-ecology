#17/09/2024
#Seguineau Pauline 

#EML validate

#Load packages

library(emld)
library(EML)

#Load arguments

args = commandArgs(trailingOnly=TRUE)

if(length(args)>0){
  eml_file <- args[1]
}else{

stop("This tool needs at least one argument.")}

#run eml_validate

eml = read_eml(eml_file)

eml_valid = eml_validate(eml)

if(eml_valid==T){
  cat("Your EML is valid.")
}else if(eml_valid==F && length(attr(eml_valid,""))>1){
  cat("Your EML is not valid. You can improve it by correcting these errors:","\n")
  attr(eml_valid,"")
}else{
  cat("Your EML is not valid. You can improve it by correcting this error:","\n")
  attr(eml_valid,"")
} 

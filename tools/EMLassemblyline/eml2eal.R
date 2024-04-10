##07/06/2023
##Genthon Tanguy
###eml2eal

#load arguments
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0)
{
  stop("This tool needs at least one argument")
}else{
  data <- args[1]
}


EMLassemblyline::eml2eal(data,path=".")

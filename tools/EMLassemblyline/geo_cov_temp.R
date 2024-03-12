#27/11/2023
#Seguineau Pauline
#Make geographic coverage template

#Load packages

library(EMLassemblyline)

#Load arguments

args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  
  data_table <- args[1]
  tablename <- args[2]
  lat_col <- as.numeric(args[3])
  long_col <- as.numeric(args[4])
  site_col <- as.numeric(args[5])
  empty <- args[6]
}

datatable = read.table(data_table,sep="\t",header=T)

latcol = names(datatable[lat_col])
longcol = names(datatable[long_col])
sitecol = names(datatable[site_col])

if (empty == "false"){
   empty = FALSE
}else if (empty=="true"){
   empty=TRUE}


#Make template

template_geographic_coverage(path =".", data.path = "data_files", data.table = tablename, lat.col = latcol, lon.col = longcol, site.col = sitecol, empty = empty)



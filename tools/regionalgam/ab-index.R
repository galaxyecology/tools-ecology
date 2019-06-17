#!/usr/bin/env Rscript
#library("getopt")
#library(devtools)
#library(RegionalGAM)

args = commandArgs(trailingOnly=TRUE)
source(args[1])


tryCatch({input = read.table(args[2], header=TRUE,sep=" ")},finally={input = read.table(args[2], header=TRUE,sep=",")})
pheno = read.table(args[3], header=TRUE,sep="	")

if("TREND" %in% colnames(input)){
    input <- input[input$TREND==1,c("SPECIES","SITE","YEAR","MONTH","DAY","COUNT")]
}
data.index <- abundance_index(input, pheno)
write.table(data.index, file="data.index", row.names=FALSE, sep="	")

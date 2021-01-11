#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
source(args[1])

input = data.frame(data.table::fread(args[2]))
pheno = read.table(args[3], header=TRUE,sep="	")

if("TREND" %in% colnames(input)){
    input <- input[input$TREND==1,c("SPECIES","SITE","YEAR","MONTH","DAY","COUNT")]
}
data.index <- abundance_index(input, pheno)
write.table(data.index, file="data.index", row.names=FALSE, sep="	")

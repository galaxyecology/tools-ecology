#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
source(args[1])


# import file
input = fread(args[2])
# retransform to data.frame to allow the calculation by the fonctions from the package
input = data.frame(input)

if("TREND" %in% colnames(input)){
    input <- input[input$TREND==1,c("SPECIES","SITE","YEAR","MONTH","DAY","COUNT")]
}
data.index <- abundance_index(input, pheno)
write.table(data.index, file="data.index", row.names=FALSE, sep="	")

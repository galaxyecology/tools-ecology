#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source(args[1]) #TODO replace by library(regionalGAM) if available as official package from bioconda
library(data.table)
# import file
input = fread(args[2])
# retransform to data.frame to allow the calculation by the fonctions from the package
input = data.frame(input)
# select only the columns with the specific names (specific format for the function)
dataset1 <- input[,c("SPECIES", "SITE", "YEAR", "MONTH", "DAY", "COUNT")]
pheno <- flight_curve(dataset1, MinVisit = args[3], MinOccur = args[4])

write.table(pheno, file="pheno", row.names=FALSE, sep="	")

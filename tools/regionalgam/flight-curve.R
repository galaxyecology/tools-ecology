#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
source(args[1]) #TODO replace by library(regionalGAM) if available as official package from bioconda
input = data.table::fread(args[2])
dataset1 <- input[,c("SPECIES", "SITE", "YEAR", "MONTH", "DAY", "COUNT")]
pheno <- flight_curve(dataset1, MinVisit = args[3], MinOccur = args[4])

write.table(pheno, file="pheno", row.names=FALSE, sep="	")

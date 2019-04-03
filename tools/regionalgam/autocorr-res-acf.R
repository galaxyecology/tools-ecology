#!/usr/bin/env Rscript
library(nlme)
library(MASS)

args = commandArgs(trailingOnly=TRUE)
load(args[1])

png('output-acf.png')
graph<-acf(residuals(mod,type="normalized"))
invisible(dev.off())

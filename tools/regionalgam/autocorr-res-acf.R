#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)
load(args[1])

png('output-acf.png')
graph<-acf(residuals(mod,type="normalized"),plot=FALSE)
plot(graph, main="Acf residuals")
invisible(dev.off())

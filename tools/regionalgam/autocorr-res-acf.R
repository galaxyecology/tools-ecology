#!/usr/bin/env Rscript
library("Cairo")
args = commandArgs(trailingOnly=TRUE)
load(args[1])

png('output-acf.png',width = 480, height = 480, units = "px",type="cairo",antialias="default")
graph<-acf(residuals(mod,type="normalized"),plot=FALSE)
plot(graph, main="Acf residuals")
invisible(dev.off())

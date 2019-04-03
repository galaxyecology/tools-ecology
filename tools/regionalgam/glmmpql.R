#!/usr/bin/env Rscript
library(nlme)
library(MASS)

args = commandArgs(trailingOnly=TRUE)
input = read.table(args[1], header=TRUE,sep="	") 
glmm.mod_fullyear <- glmmPQL(regional_gam~ as.factor(YEAR)-1,data=input,family=quasipoisson,random=~1|SITE, correlation = corAR1(form = ~ YEAR | SITE),verbose = FALSE)

col.index <- as.numeric(glmm.mod_fullyear$coefficients$fixed)
year <- unique(input$YEAR)

write.table(col.index, file="output-glmmpql", row.names=FALSE, sep=" ")

png('output-plot.png')
plot(year,col.index,type='o', xlab="year",ylab="collated index")
invisible(dev.off())

#!/usr/bin/env Rscript
library(nlme)
library(MASS)

args <- commandArgs(trailingOnly = TRUE)
input <- read.table(args[1], header = TRUE, sep = "	") #input=data.index =ab_index-output
load(args[2])

glmm.mod_fullyear <- glmmPQL(regional_gam ~ as.factor(YEAR)-1, data=input,family=quasipoisson, random=~1|SITE, correlation = corAR1(form = ~ YEAR | SITE), verbose = FALSE)

col.index <- as.numeric(glmm.mod_fullyear$coefficients$fixed)
year <- unique(input$YEAR)

png('output-plot-trend.png')
plot(year,col.index, type ='o',xlab = "year", ylab = "collated index")
abline(mod,lty = 2,col = "red")
invisible(dev.off())

d<-data.frame(year, col.index)
colnames(d) <- c("year", "collated index")
write.table(d, "s_trend.tsv", sep = "	", row.names = FALSE)

cat("Show trend line on abundance plot")

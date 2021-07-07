#!/usr/bin/env Rscript
library(nlme)
library(MASS)

args <- commandArgs(trailingOnly = TRUE)
input1 <- read.table(args[1], header = TRUE)
input2 <- read.table(args[2], header = TRUE, sep = "	")

input1 <- as.matrix(input1)

year <- unique(input2$YEAR)
mod <- gls(input1 ~ year, correlation = corARMA(p = 2))
summary <- summary(mod)

save(mod, file = "mod_adjusted.rda")
capture.output(summary, file = "mod_adjusted-summary.txt")

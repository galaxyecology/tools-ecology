##13/04/2023
##Seguineau Pauline
### Clustering with Clara algorithm

#load library 
library(cluster)
library(dplyr)
library(tidyverse)

#load arguments
args = commandArgs(trailingOnly=TRUE) 
if (length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    data <- args[1]
    enviro <- args[2]
    data.bio <- args[3]
    k <- as.numeric(args[4])
    metric <- args[5]
    sample <- as.numeric(args[6])
}

#load data 
env.data <- read.table(enviro, header=TRUE, sep="\t",dec = ".", na.strings = "-9999")
data.bio <- read.table(data.bio, header=TRUE, sep="\t")
test3 <- read.table(data, header = TRUE, sep="\t") 

######################################################################################################
#Make clustering

k <- k  #number of clusters
test5 <- clara(test3, k, metric = metric,  samples = sample, sampsize = min(nrow(test3), (nrow(data.bio)/nrow(test3))+2*k))

#######################################################################################################
#save results
												
png("sih.png")
plot(silhouette(test5))
dev.off()

clus <- cbind(data.bio[1:nrow(test3), 1:2],test5$clustering)
names(clus) <- c("lat", "long", "cluster")
clus <- cbind(clus,test3,env.data[,3:ncol(env.data)])

write.table(clus[1:3], file = "points_clus.tsv", quote = FALSE, sep="\t", row.names = FALSE)
write.table(clus, file = "clus.tsv", quote = FALSE, sep="\t", row.names = FALSE)






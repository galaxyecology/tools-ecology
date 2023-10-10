# Script to determine the optimal number of clusters thanks to the optimization of the SIH index and to produce the files needed in the next step of clustering

#load packages
library(cluster)
library(dplyr)
library(tidyverse)

#load arguments
args = commandArgs(trailingOnly=TRUE) 
if (length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    enviro <- args[1]
    taxa_list <- args[2]
    preds <- args[3]
    max_k <- as.numeric(args[4])
    metric <- args[5]
    sample <- as.numeric(args[6])
}

#load data 

env.data <- read.table(enviro, header = TRUE, dec = ".", na.strings = "-9999.00") 

##List of modelled taxa used for clustering
tv <- read.table(taxa_list, dec=".", sep=" ", header=F, na.strings = "NA") 
names(tv) <- c("a")

################Grouping of taxa if multiple prediction files entered ################

data_split = str_split(preds,",")
data.bio = NULL

for (i in 1:length(data_split[[1]])) {
data.bio1 <- read.table(data_split[[1]][i], dec=".", sep=" ", header=T, na.strings = "NA")
data.bio <- rbind(data.bio,data.bio1)
remove(data.bio1)
}

names(data.bio) <- c("lat", "long", "pred", "taxon")

#keep selected taxa
data.bio <- data.bio[which(data.bio$taxon %in% tv$a),]

write.table(data.bio,file="data_bio.tsv",sep="\t",quote=F,row.names=F)

#format data

test3 <- matrix(data.bio$pred , nrow = nrow(env.data),  ncol = nrow(data.bio)/nrow(env.data))
test3 <- data.frame(test3)
names(test3) <- unique(data.bio$taxon)

write.table(test3, file="data_to_clus.tsv", sep="\t",quote=F,row.names=F)

#Max number of clusters to test
max_k <- max_k

# Initialization of vectors to store SIH indices
sih_values <- rep(0, max_k)

# Calculation of the SIH index for each number of clusters
for (k in 2:max_k) {
  # Clara execution
  clara_res <- clara(test3, k,  metric =metric,  samples = sample, sampsize = min(nrow(test3), (nrow(data.bio)/nrow(test3))+2*k))
  # Calculation of the SIH index
  sih_values[k] <- clara_res$silinfo$avg.width
}

# Plot SIH Index Chart by Number of Clusters
png("Indices_SIH.png")
plot(2:max_k, sih_values[2:max_k], type = "b", xlab = "Nombre de clusters", ylab = "Indice SIH")
dev.off()

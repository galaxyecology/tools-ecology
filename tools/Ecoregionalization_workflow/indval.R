#Seguineau Pauline
#02/12/2024
#Indicspecies tool

library(dplyr)
library(indicspecies)

#load arguments
args = commandArgs(trailingOnly=TRUE) 
if (length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    clus_pts <- args[1]
    occ <- args[2]
    spe_name <- args[3]
    sign <- args[4]
}

###load data
clus <- read.table(clus_pts, dec=".", sep="\t", header=T,na.strings = "na") #cluster points file (ecoregionalization workflow)
data.col <- read.table(occ, dec=".", sep="\t", header=T,na.strings = "na") #occurrence file (merged table from ecoregionalization workflow)
spe_name = strsplit(spe_name, ",")
spname=NULL

for (n in spe_name) {
spname = cbind(names(data.col[as.numeric(n)]))}

#Rename decimalLatitude and decimalLongitude columns from occurrence file
if ("decimalLatitude" %in% colnames(data.col)) {
   colnames(data.col)[which(colnames(data.col) == "decimalLatitude")] <- "lat"
}
if ("decimalLongitude" %in% colnames(data.col)) {
   colnames(data.col)[which(colnames(data.col) == "decimalLongitude")] <- "long"
}

#Round lat and long of the data.col file to be able to put clus cluster in it
data.col$lat = round(data.col$lat,digits = 2)
data.col$long = round(data.col$long,digits = 2)

# Creates a new "station" column that associates a unique identifier to each latitude-longitude pair
data.col <- data.col %>% mutate(station = as.factor(paste(lat, long, sep = "_")))

# convert "station" to a unique numeric identifier
data.col <- data.col %>%  mutate(station = as.numeric(factor(station)))

#Adding clusters to file
clusta <- merge(data.col,clus, by=c("lat","long"), all.x = TRUE)

#This generated duplicates with different clusters
clusta <- aggregate(clusta, by=list(clusta$station,clusta$cluster), FUN=mean, na.rm=TRUE)
clusta <- na.exclude(clusta)

# indval on all clusters
indval = multipatt(clusta[,spname], clusta$cluster,duleg = TRUE, control = how(nperm=999))
if (sign=="true"){
	capture.output(summary(indval,indvalcomp=TRUE), file = "indval.txt")
}else{
	capture.output(summary(indval,indvalcomp=TRUE, alpha=1), file = "indval.txt")}

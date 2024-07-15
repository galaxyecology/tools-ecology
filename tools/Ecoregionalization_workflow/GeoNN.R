#Date : 09/02/2024
#Author : Seguineau Pauline

#Load libraries
library(tidyr)
library(dplyr)
library(sf)

#load arguments
args = commandArgs(trailingOnly=TRUE) 
if (length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    enviro <- args[1]
    envlong <- as.numeric(args[2])
    envlat <- as.numeric(args[3])
    occu <- args[4]
    occulat <- as.numeric(args[5])
    occulong <- as.numeric(args[6])
}
 
env = read.table(enviro, header = TRUE, sep="\t")
occ = read.table(occu, header = TRUE, sep = "\t")

cols_env = c(names(env[envlong]),names(env[envlat]))
cols_occ = c(names(occ[occulong]),names(occ[occulat]))

###calculate distances### 
#transform tables into sf object

env_sf <- st_as_sf(env, coords = cols_env, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
occ_sf <- st_as_sf(occ, coords = cols_occ, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#Find the indices of env_sf entities closest to each point in occ_sf.

nearest_indices <- st_nearest_feature(occ_sf, env_sf)

nearest_points <- env[nearest_indices, ]

# Calculate distances between env_sf and occ_sf points
distances <- st_distance(env_sf, occ_sf)

#Extract the corresponding distances between occ and env

nearest_distances <- numeric(length(nearest_indices))

for (i in 1:length(nearest_indices)) {
  nearest_distances[i] <- st_distance(env_sf[nearest_indices[i],], occ_sf[i,])
}

#assemble occurrences and environmental parameters in the same file

nearest_points <- nearest_points[, !names(nearest_points) %in% cols_env] #remove lat and long from env to clean data
new_occ = cbind(occ, nearest_points)

#Save the file 

write.table(new_occ, file = "occurrence_env.tsv",sep ="\t",quote = F, row.names = F,col.names = T)

#create an information file with the distances between the points of the two files 

distance_info <- data.frame(
  occ_geometry = occ_sf$geometry,
  env_geometry = env_sf$geometry[nearest_indices],
  distance = nearest_distances
)

colnames(distance_info)[1] <- "occ_geometry"
colnames(distance_info)[2] <- "env_geometry"
colnames(distance_info)[3] <- "Distances (meters)"
#save the information file

write.table(distance_info, file = "infos_file.tsv",sep ="\t",quote = F, row.names = F,col.names = T)




#Author : Seguineau Pauline


#Create a map from cluster

library(sf)
library(tmap)
library(dplyr)

args = commandArgs(trailingOnly=TRUE) 
if (length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    data <- args[1]
}

clus <- read.table(data, header=TRUE, na.strings = "na")

#tmap method

sf_data <- st_as_sf(clus, coords = c("long", "lat"), crs =4326)

grouped_data <- sf_data %>%
  group_by(cluster) %>%
  summarize()

map <- tm_shape(grouped_data) + 
  tm_dots(col = "cluster", palette = "Accent", size = 0.1, title = "écorégions")+
  tm_scale_bar(position = c("right","top"))+
  tm_compass(position = c("right","top"))+
  tm_layout(frame = FALSE,legend.position = c("left","bottom"))+
  tm_xlab("Longitude")+
  tm_ylab("Latitude")+
  tm_grid(alpha = 0.2)

#Save the map 
tmap_save(map, "ecoregions.png")

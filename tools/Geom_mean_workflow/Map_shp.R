library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggspatial)

args = commandArgs(trailingOnly=TRUE) 
if (length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    dataMap <- args[1]
    dataEvo <- args[2]
    title <- args[3]
    legend <- args[4]
    coord <- args[5]
     
}

title <- gsub("\\\\n", "\n", title) 
legend <-gsub("\\\\n", "\n", legend) 
#read data

data_map = st_read(dataMap)
data_evo = read.delim(dataEvo,header=TRUE,sep="\t")

#bring together data

data_fin = bind_cols(data_map,data_evo[2])


# define the data intervals
intervals <- cut(data_fin$Evolution_rate, breaks = c(-Inf, 0, 9, 20, Inf), labels = c("Moins de 0", "0 à 10", "10 à 20", "Plus de 20"))

# Make the map with ggplot2 

if (coord == "true"){
  ggplot(data_fin) +
    geom_sf(aes(fill = intervals)) +
    scale_fill_manual(values = c('#D9F0D3',"#A6DBA0","#5AAE61","#1B7837")) +
    labs(title = title, fill = legend) +
    annotation_scale()
    
  #outuput 
  ggsave("map.pdf", device = "pdf")

}else{
  ggplot(data_fin) +
    geom_sf(aes(fill = intervals)) +
    scale_fill_manual(values = c('#D9F0D3',"#A6DBA0","#5AAE61","#1B7837")) +
    labs(title = title, fill = legend) +
    theme_void()+
    annotation_scale()
    
  #outuput 
  ggsave("map.pdf", device = "cairo_pdf")
}






#!/bin/Rscript

library(sf)
library(tidyr)

args = commandArgs(trailingOnly=TRUE)
output_list <- strsplit(as.character(args[2]), ',')[[1]]



save_as_pdf_fun <- function(){

    pdf(file = 'PDF_output.pdf')
    plot(st_geometry(Transformed_spacial_coordinates))
    dev.off()
}


save_as_shp_fun <- function(){

    write_sf(Transformed_spacial_coordinates, 'shapefile.shp')
}


save_as_image_fun <- function(format){
    
    img_fun = as.list(paste(format, '_output.', format, sep = '', collapse = NULL))
    do.call(format, img_fun)
    plot(st_geometry(Transformed_spacial_coordinates))
    dev.off()
}



if (length(args)<2){stop('Insufficient number of argument, please review command Line')
}else{
    Spacial_coordinates_files <- read_sf(as.character(args[1]), layer = 'shapefile')
    projection <- as.character(args[4])
    
    for (n_arg in 5:length(args)) {
        projection <- paste(projection, as.character(args[n_arg]),sep = '', collapse = NULL)
    }
    
    projection <- paste(projection, ' +no_defs',sep = '', collapse = NULL)
    Transformed_spacial_coordinates <- st_transform(Spacial_coordinates_files, crs = projection)
    for (a in output_list) {
        if (as.character(a) == 'pdf') {
        save_as_pdf_fun()
        next
        
        } else if (as.character(a) == 'shp') {
        save_as_shp_fun()
        next
        
        } else { save_as_image_fun(as.character(a)) }
    }}

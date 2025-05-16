#!/bin/Rscript
# GRID CREATION
args = commandArgs(trailingOnly=TRUE)

library(sf)
library(tidyr)


save_as_pdf_fun <- function(){

    pdf(file = "output.pdf")
    plot(st_geometry(Transformed_spacial_coordinates))
    dev.off()
}


save_as_image_fun <- function(format){

    filename <- paste0("output.", format)
    # Ouvre le bon device graphique selon le format demandé
    switch(format,
           png = png(filename),
           jpeg = jpeg(filename),
           tiff = tiff(filename),
           bmp = bmp(filename),
           stop("Unsupported format: ", format)
    )
    plot(st_geometry(Transformed_spacial_coordinates))
    dev.off()
}


save_as_shp_fun <- function(){
    write_sf(Transformed_spacial_coordinates, "output.shp")
}




if (length(args)<1){stop('please provide spacial coordinates files(.shp files)')
}else{
    Spacial_coordinates_files <- read_sf(as.character(args[1]), layer = 'shapefile')
    output_file <- args[10]

    projection <-    paste('+proj='
                          , as.character(args[2])
                          , ' +lat_0='
                          , as.character(args[3])
                          , ' +lon_0='
                          , as.character(args[4])
                          , ' +x_0='
                          , as.character(args[5])
                          , ' +y_0='
                          , as.character(args[6])
                          , ' +ellps='
                          , as.character(args[7])
                          , ' +datum='
                          , as.character(args[7])
                          , ' +units='
                          , as.character(args[8])
                          , ' +no_defs', sep = "", collapse = NULL) #presonalisation du systeme de références
    
    Transformed_spacial_coordinates <- st_transform(Spacial_coordinates_files, crs = projection)
    
    if (as.character(args[9]) == "pdf") {
        save_as_pdf_fun()

    } else if (as.character(args[9]) == "shp") {
        save_as_shp_fun()
    
    } else { 
        save_as_image_fun(as.character(args[9])) 
    }
}






#write.table(Transformed_spacial_coordinates, file = "output.tabular", sep="\t", row.names=FALSE)
    #enregistrement dans quoi ?
    
#this tool convert shp files to the right format for whatever you like ~a phylodiversity analysis

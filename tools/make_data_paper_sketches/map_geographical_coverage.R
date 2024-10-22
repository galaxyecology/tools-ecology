#From https://github.com/TanguyGen/metaCure/blob/main/R/eml_down.R
#Modified by Seguineau Pauline (2024-10-15)
library(dplyr)
library(xslt)
library(xml2)
library(mapview)
library(leaflet)

args = commandArgs(trailingOnly=TRUE) 
if (length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    data <- args[1]
}

#' map_geographical_coverage
#'Make a map from EML
#' @param eml Metadata using EML standard in XML format
#'
#' @return A map
#' @export

map_geographical_coverage <- function(eml){
  name <- xml2::xml_find_all(eml, "//geographicCoverage/geographicDescription")
  name <- unlist(xml2::as_list(name))

  west <- xml2::xml_find_all(eml, "//geographicCoverage/boundingCoordinates/westBoundingCoordinate")
  west <- as.numeric(unlist(xml2::as_list(west)))

  east <- xml2::xml_find_all(eml, "//geographicCoverage/boundingCoordinates/eastBoundingCoordinate")
  east <- as.numeric(unlist(xml2::as_list(east)))

  north <- xml2::xml_find_all(eml, "//geographicCoverage/boundingCoordinates/northBoundingCoordinate")
  north <- as.numeric(unlist(xml2::as_list(north)))

  south <- xml2::xml_find_all(eml, "//geographicCoverage/boundingCoordinates/southBoundingCoordinate")
  south <- as.numeric(unlist(xml2::as_list(south)))

  geo_info <- data.frame(name = name, west = west, east = east, south = south, north = north)

  map <- leaflet(geo_info) %>%
         addProviderTiles("CartoDB.Positron")  
  
  for (i in 1:nrow(geo_info)) {
    if (geo_info$west[i]==geo_info$east[i] && geo_info$south[i]==geo_info$north[i]){
      map <- map %>% addCircles(lng = geo_info$west[i] , lat= geo_info$south[i], fillColor = "transparent" ) %>% addScaleBar()}
   
     else if(geo_info$west[i]!=geo_info$east[i] && geo_info$south[i]!=geo_info$north[i]){
      map <- map %>% addRectangles(lng1 = geo_info$west[i], lat1 = geo_info$south[i], 
                                   lng2 = geo_info$east[i], lat2 = geo_info$north[i], fillColor = "transparent") %>% addScaleBar()}
    }
   mapview::mapshot(map, file = "map.png",remove_controls = c("zoomControl", "layersControl", "homeButton","drawToolbar", "easyButton"))
}

doc <- read_xml(data)

if (is.na(xml2::xml_find_first(doc, "//geographicCoverage"))){
   mes = "No geographic coverage found, skipping geographic coverage step."
   mes
}else{
 map_geographical_coverage(doc)
}


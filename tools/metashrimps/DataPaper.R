##07/06/2023
##Genthon Tanguy
### Data Paper draft production

#load arguments
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0)
{
  stop("This tool needs at least one argument")
}else{
  path_temp <- args[1]
  data <- args[2]
  edit <- args[3]
}

library(dplyr)
#####################################################################################################
#Functions for creation of Data Papers

#' map_geographical_coverage
#'Make a map from EML
#' @param eml Metadata using EML standard in XML format
#'
#' @return A map
#' @export
map_geographical_coverage <- function(eml,editable=edit){
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

  geo_info <- data.frame(name = name,
                         west = west, east = east,
                         south = south, north = north)

  map <- leaflet::leaflet(geo_info) %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    leaflet::addRectangles(
      lng1 = west, lat1 = south,
      lng2 = east, lat2 = north,
      popup = name,
      fillColor = "transparent"
    )
  if (editable=="true") {
    mapview::mapshot(map,file="map.png")
  }
  mapview::mapshot(map, url = "map.html")
}


##' Render EML metadata into a webpage
##'
##' Pass in an xml file of EML metadata and generate a nice webpage describing
##' the dataset.
##'
##' @title Render EML
##' @param eml A valid Ecological Metadata Language file to be rendered to html.
##' @param open Whether to open the file in a browser. Defaults to TRUE.
##' @param publish_mode TRUE. If TRUE the website is pretty without warnings for weird stuff.
##' @param output_dir directory where will be stored the result file
##' @param encoding "" encoding of the EML file if necessary
##' @return HTML file containing dataset information
render_eml <- function(eml, open = FALSE,
                       publish_mode = TRUE, output_dir = "/docs",
                       encoding = "",editable) {
  eml <- xml2::read_xml(eml, encoding = encoding)
  if (editable=="true"){
    style2 <- xml2::read_xml(paste0(path_temp,"/bootstrap2.xsl"))
    html <- xslt::xml_xslt(eml, style2)
    # make map
    xml2::write_html(html, "DataPaper2.html")
    rmarkdown::pandoc_convert("DataPaper2.html", output="DataPaper.docx", options =c("--standalone"))
    file.copy("DataPaper.docx",".")
  }
  style <- xml2::read_xml(paste0(path_temp,"/bootstrap.xsl"))
  html <- xslt::xml_xslt(eml, style)
  # make map
  map_geographical_coverage(eml)
  xml2::write_html(html, "DataPaper.html")
}

#######################################################################################################
#exportation of Data Paper

render_eml(data,editable=edit)



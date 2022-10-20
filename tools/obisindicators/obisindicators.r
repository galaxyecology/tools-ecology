#Rscript

###########################################
##    Mapping alpha and beta diversity   ##
###########################################

#####Packages : obisindicators
#               dplyr
#               sf
#               ggplot2
#               rnaturalearth
#               rnaturalearthdata
#               viridis
#               dggridr
library(magrittr)

## remotes::install_github("r-barnes/dggridR")
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

# url for the S2 subset

if (length(args) < 4) {
    stop("This tool needs at least 4 argument : longitude, latitude, species and number of records")
}else {
    raster <- args[1]
    hr <- args[2]
    sep <- as.character(args[3])
    longitude <- as.numeric(args[4])
    latitude <- as.numeric(args[5])
    spe <- as.numeric(args[6])
    rec <- as.numeric(args[7])
    crs <- as.numeric(args[8])
    source(args[9])
    source(args[10])
    source(args[11])
}

if (hr == "false") {
  hr <- FALSE
}else {
  hr <- TRUE
}

if (sep == "t") {
   sep <- "\t"
}

if (crs == "0") {
   crs <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
}
#####Import data
occ <- read.table(raster, sep = sep, dec = ".", header = hr, fill = TRUE, encoding = "UTF-8") # occ_1M OR occ_SAtlantic
occ <- na.omit(occ)
#Get biological occurrences
#Use the 1 million records subsampled from the full OBIS dataset
colnames(occ)[longitude] <- c("decimalLongitude")
colnames(occ)[latitude] <- c("decimalLatitude")
colnames(occ)[spe] <- c("species")
colnames(occ)[rec] <- c("records")

#Create a discrete global grid
#Create an ISEA discrete global grid of resolution 9 using the dggridR package:

dggs <- dggridR::dgconstruct(projection = "ISEA", topology = "HEXAGON", res = 9)

#Then assign cell numbers to the occurrence data
occ$cell <- dggridR::dgGEO_to_SEQNUM(dggs, occ$decimalLongitude, occ$decimalLatitude)[["seqnum"]]

#Calculate indicators
#The following function calculates the number of records, species richness, Simpson index, Shannon index, Hurlbert index (n = 50), and Hill numbers for each cell.

#Perform the calculation on species level data
idx <- calc_indicators(occ)
write.table(idx, file = "Index.csv", sep = ",", dec = ".", na = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)

#add cell geometries to the indicators table (idx)
grid_idx <- sf::st_wrap_dateline(dggridR::dgcellstogrid(dggs, idx$cell))
colnames(grid_idx) <- c("cell", "geometry")

grid <- dplyr::left_join(grid_idx,
    idx,
    by = "cell")

#Plot maps of indicators
#Let’s look at the resulting indicators in map form.
#Indice ES(50)
es_50_map <- gmap_indicator(grid, "es", label = "ES(50)", crs = crs)
es_50 <- ggplot2::ggsave("ES_50.png", es_50_map, scale = 0.38, width = 12, height = 7, units = "in", dpi = 300, limitsize = TRUE)

# Shannon index
shannon_map <- gmap_indicator(grid, "shannon", label = "Shannon index", crs = crs)
shannon <- ggplot2::ggsave("Shannon_index.png", shannon_map, scale = 0.38, width = 12, height = 7, units = "in", dpi = 300, limitsize = TRUE)


# Number of records, log10 scale, Geographic projection
records_map <- gmap_indicator(grid, "n", label = "# of records", trans = "log10", crs = crs)
records <- ggplot2::ggsave("Records.png", records_map, scale = 0.38, width = 12, height = 7, units = "in", dpi = 300, limitsize = TRUE)

# Simpson index
simpson_map <- gmap_indicator(grid, "simpson", label = "Simpson index", crs = crs)
simpson <- ggplot2::ggsave("Simpson_index.png", simpson_map, scale = 0.38, width = 12, height = 7, units = "in", dpi = 300, limitsize = TRUE)

# maxp
maxp_map <- gmap_indicator(grid, "maxp", label = "maxp index", crs = crs)
maxp <- ggplot2::ggsave("Maxp.png", maxp_map, scale = 0.38, width = 12, height = 7, units = "in", dpi = 300, limitsize = TRUE)

#Mapping
es_50
shannon
simpson
maxp
records

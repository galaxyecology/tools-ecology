#Rscript

###########################################
##     Retrieve Obis occurences data     ##
###########################################

##### Packages : robis
# https://iobis.github.io/robis/articles/getting-started.html
# Get args
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
    stop("This tool needs at least 1 argument : longitude, latitude, species or taxonID")
}else {
    sname <- args[1]
    taxid <- args[2]
    lat_min <- args[3]
    lat_max <- args[4]
    long_min <- args[5]
    long_max <- args[6]
}

if (lat_min == "0.0" & lat_max == "0.0" & long_min == "0.0" & long_max == "0.0") {
lat_min <- ""
lat_max <- ""
long_min <- ""
long_max <- ""
}

##### Import data
# Get biological occurrences
if (lat_min != "" & sname != "" & taxid != "") {
my_occs <- robis::occurrence(scientificname = sname, taxonid = taxid, geometry = paste("POLYGON ((", long_min, lat_min,  ", ", long_min, lat_max, ", ", long_max, lat_min, ", ", long_max, lat_max, ", ", long_min, lat_min, "))"))
}else if (lat_min != "" & sname != "" & taxid == "") {
my_occs <- robis::occurrence(scientificname = sname, geometry = paste("POLYGON ((", long_min, lat_min,  ", ", long_min, lat_max, ", ", long_max, lat_min, ", ", long_max, lat_max, ", ", long_min, lat_min, "))"))
}else if (lat_min != "" & sname == "" & taxid != "") {
my_occs <- robis::occurrence(taxonid = taxid, geometry = paste("POLYGON ((", long_min, lat_min,  ", ", long_min, lat_max, ", ", long_max, lat_min, ", ", long_max, lat_max, ", ", long_min, lat_min, "))"))
}else if (lat_min != "" & sname == "" & taxid == "") {
my_occs <- robis::occurrence(geometry = paste("POLYGON ((", long_min, lat_min,  ", ", long_min, lat_max, ", ", long_max, lat_min, ", ", long_max, lat_max, ", ", long_min, lat_min, "))"))
}else if (lat_min == "" & sname != "" & taxid != "") {
my_occs <- robis::occurrence(scientificname = sname, taxonid = taxid)
}else if (lat_min == "" & sname == "" & taxid != "") {
my_occs <- robis::occurrence(taxonid = taxid)
}else if (lat_min == "" & sname != "" & taxid == "") {
my_occs <- robis::occurrence(scientificname = sname)
}


# Dispay results

# If empty
if(length(my_occs) == 0) {
cat("\nNo occurrences found.\nLittle tip : Check your input typo, some databases are case sensitive : Genus species.\n")
}


write.table(file = "output.tab", my_occs, sep = "\t", dec = ".", na = "", row.names = FALSE, col.names = TRUE, quote = FALSE)

#Rscript

###########################
##     Anonymization     ##
###########################

#####Packages : tangles

#Load arguments

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    stop("This tool needs at least one argument")
}else{
    table <- args[1]
    hr <- args[2]
    latitude <- as.numeric(args[3])
    longitude <- as.numeric(args[4])
}

if (hr == "false") {
  hr <- FALSE
}else{
  hr <- TRUE
}

#####Import data
data <- read.table(table, sep = "\t", dec = ".", header = hr, fill = TRUE, encoding = "UTF-8")

randomized_data <- tangles::tangles(data = as.matrix(data[, c(latitude, longitude)]), depth = 3, rasterdata = FALSE, raster_object = FALSE, saveTangles = FALSE, path = NULL)
 
data[, c(latitude, longitude)] <- NULL

tab_anon <- data.frame(longitude = randomized_data[[1]]$X, latitude = randomized_data[[1]]$Y)

tab_anon <- cbind(data, tab_anon)

write.table(tab_anon, "anonym_data.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")

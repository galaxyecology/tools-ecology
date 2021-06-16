#Rscript

#########################################################
##    Presence abscence and abundance in environment   ##
#########################################################

#####Packages : ggplot2
#               vegan

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 5){
    stop("This tool needs at least 5 arguments")
}else{
    table <- args[1]
    hr <- args[2]
    abundance <- as.logical(args[3])
    presabs <- as.logical(args[4])
    rarefaction <- as.logical(args[5])
    lat <- as.numeric(args[6])
    long <- as.numeric(args[7])
    ind <- as.character(args[8])
    loc <- as.numeric(args[9])
    num <- as.character(args[10])
    spe <- as.numeric(args[11])
    abond <- as.numeric(args[12])		
}

if (hr == "false") {hr <- FALSE} else {hr <- TRUE}

#####Import data
data <- read.table(table, sep = "\t", dec = ".", header = hr, fill = TRUE, encoding = "UTF-8")

if (abundance) {
collat <- colnames(data)[lat]
collong <- colnames(data)[long]
}

if (presabs) {
colloc <- colnames(data)[loc]
}

if (presabs | rarefaction) {
colspe <- colnames(data)[spe]
data <- data[grep("^$", data[, colspe], invert = TRUE), ]
}

if (abundance| presabs | rarefaction) {
colabond <- colnames(data)[abond]
}

#####Your analysis

####The abundance in the environment####

##Representation of the environment##

## Mapping
graph_map <- function(data, long, lat, abond, ind) {
  cat("\nAbunbance\n", abond, file = "Data_abund.txt", fill = 1, append = TRUE)
  mappy <- ggplot2::ggplot(data, ggplot2::aes_string(x = collong, y = collat, cex = colabond, color = colabond)) +  
  ggplot2::geom_point() + ggplot2::ggtitle(paste("Abundance of", ind, "in the environment")) + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")
  ggplot2::ggsave("mappy.png", mappy, width = 15, height = 9, units = "cm")
}

####Presence/absence/abundance####

## Histogram 
graph_hist <- function (data, col1, col2, col3) {
  cat("\nLocations\n", data[, col1], file = "Locations.txt", fill = 1, append = TRUE)
  if(mult1) {
    for(loc in unique(data[, col1])) {
      data.cut <- data[data[, col1] == loc, ]
      data.cut <- data.cut[data.cut[, col3] > 0, ]
      top <- nrow(data.cut)
      var <- nchar(as.character(round(top * 0.1, digits = 0)))
      step <- round(top * 0.1, digits = ifelse(var == 1, 1, -var + 1))
      graph <- ggplot2::ggplot(data.cut) +
      ggplot2::geom_bar(ggplot2::aes_string(x = col1, fill = col2)) +
      ggplot2::scale_y_continuous(breaks=seq(from = 0, to = top, by = step)) +
      ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold")) +
      ggplot2::ggtitle("Number of presence absence")

      ggplot2::ggsave(paste("number_in_", loc, ".png"), graph)
    }
  }else{
  top <- nrow(data)
  var <- nchar(as.character(round(top * 0.1, digits = 0)))
  step <- round(top * 0.1, digits = ifelse(var == 1, 1, -var + 1))
  graph <- ggplot2::ggplot(data) +
  ggplot2::geom_bar(ggplot2::aes_string(x = col1 , fill = col2)) +
  ggplot2::scale_y_continuous(breaks=seq(from = 0, to = top, by = step)) +
  ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold")) +
  ggplot2::ggtitle("Number of individuals")

  ggplot2::ggsave("number.png", graph)
  }
}

Rare <- function(data, spe, abond, rare, num) {
# Put the data in form
  New_data <- as.data.frame(data[, spe])
  colnames(New_data) <- c("Species")
  New_data$Total <- data[, abond]

  New_data$rarefaction <- as.integer(rare)

  end <- length(unique(New_data$Species))
  out <- vegan::rarecurve(New_data[, 2:3], step = 10, sample = rarefy.sample, label = FALSE)
  names(out) <- paste(unique(New_data$Species), sep = "")

# Coerce data into "long" form.
  protox <- mapply(FUN = function(x, y) {
    mydf <- as.data.frame(x)
    colnames(mydf) <- "value"
    mydf$species <- y
    mydf$subsample <- attr(x, "Subsample")
    mydf <- na.omit(mydf)
    mydf
  }, x = out, y = as.list(names(out)), SIMPLIFY = FALSE)

  xy <- do.call(rbind, protox)
  rownames(xy) <- NULL  # pretty
  
# Plot.

  if(mult2) {
    for(spe in unique(data[, spe])) {
      xy.cut <- xy[xy$species == spe, ]
      top <- max(xy.cut$subsample)
      var <- nchar(as.character(round(top * 0.1, digits = 0)))
      step <- round(top * 0.1, digits = ifelse(var == 1, 1, -var + 1))
      courbe <- ggplot2::ggplot(xy.cut, ggplot2::aes(x = subsample, y = value)) +
      ggplot2::theme_gray() +
      ggplot2::geom_line(size = 1) +
      ggplot2::scale_x_continuous(breaks = seq(from = 0, to = top, by = step)) +
      ggplot2::xlab("Abundance") + ggplot2::ylab("Value") +
      ggplot2::ggtitle("Rarefaction curve")

      ggplot2::ggsave(paste("rarefaction_of_", spe, ".png"), courbe)
    }
  }else{
    top <- max(xy$subsample)
    var <- nchar(as.character(round(top * 0.1, digits = 0)))
    step <- round(top * 0.1, digits = ifelse(var == 1, 1, -var + 1))
    courbe <- ggplot2::ggplot(xy, ggplot2::aes(x = subsample, y = value, color = species)) +
    ggplot2::theme_gray() +
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_x_continuous(breaks = seq(from = 0, to = top, by = step)) +
    ggplot2::xlab("Subsample") + ggplot2::ylab("Value") +
    ggplot2::ggtitle("Rarefaction curves")

    ggplot2::ggsave("rarefaction.png", courbe)
  }
}

if (abundance) {
#Maps
graph_map(data, long = long, lat = lat, abond = abond, ind = ind)
}

if (presabs) {
#Presence absence count
mult1 <- ifelse (length(unique(data[, colloc])) <= 10, FALSE, TRUE)
graph_hist(data, col1 = colloc, col2 = colspe, col3 = colabond)
}

if (rarefaction) {
#Rarefaction

#### Rarefaction indice ####
rarefy.sample <- as.numeric(num)

## Calcul de la rarefaction
rarefaction <- vegan::rarefy(data[, abond], rarefy.sample)

write.table(rarefaction, "Rare.tabular")

mult2 <- ifelse (length(unique(data[, colspe])) <= 30, FALSE, TRUE)
Rare(data, spe = colspe, abond = colabond, rare = rarefaction, num = rarefy.sample)
}

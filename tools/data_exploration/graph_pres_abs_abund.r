#Rscript

#########################################################
##    Presence abscence and abundance in environment   ##
#########################################################

#####Packages : ggplot2
#               vegan

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 5) {
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

if (hr == "false") {
  hr <- FALSE
}else{
  hr <- TRUE
}

#####Import data
data <- read.table(table, sep = "\t", dec = ".", header = hr, fill = TRUE, encoding = "UTF-8")

if (abundance) {
collat <- colnames(data)[lat]
collong <- colnames(data)[long]
}

if (presabs) {
colloc <- colnames(data)[loc]
}

if (presabs | rarefaction | abundance) {
colabond <- colnames(data)[abond]
colspe <- colnames(data)[spe]
data <- data[grep("^$", data[, colspe], invert = TRUE), ]
}

#####Your analysis

####The abundance in the environment####

##Representation of the environment##

## Mapping
graph_map <- function(data, collong, collat, colabond, ind, colspe) {
  cat("\nCoordinates range\n\nLatitude from ", min(data[, collat], na.rm = TRUE), " to ", max(data[, collat], na.rm = TRUE), "\nLongitude from ", min(data[, collong], na.rm = TRUE), " to ", max(data[, collong], na.rm = TRUE), file = "Data_abund.txt", fill = 1, append = TRUE)
  if (mult0) {
    mappy <- ggplot2::ggplot(data, ggplot2::aes_string(x = collong, y = collat, cex = colabond, color = colspe)) +
    ggplot2::geom_point() + ggplot2::ggtitle(paste("Abundance of", ind, "in the environment")) + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")  + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.text = ggplot2::element_text(size = 8)) + ggplot2::guides(cex = ggplot2::guide_legend(reverse = TRUE))

  }else{
    mappy <- ggplot2::ggplot(data, ggplot2::aes_string(x = collong, y = collat, cex = colabond, color = colabond)) +
    ggplot2::geom_point() + ggplot2::ggtitle(paste("Abundance of", ind, "in the environment")) + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")  + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.text = ggplot2::element_text(size = 8)) + ggplot2::guides(cex = ggplot2::guide_legend(reverse = TRUE))
  }
  ggplot2::ggsave("mappy.png", mappy, width = 20, height = 9, units = "cm")
}

####Presence absence abundance####

## Histogram
graph_hist <- function(data, col1, col2, col3) {
  cat("\nLocations\n", unique(data[, col1]), file = "Locations.txt", fill = 1, append = TRUE)
  if (mult1) {
    for (loc in unique(data[, col1])) {
      data_cut <- data[data[, col1] == loc, ]
      data_cut <- data_cut[data_cut[, col3] > 0, ]
      if (length(unique(data_cut[, col2])) <= 40) {
        top <- nrow(data_cut)
        var <- nchar(as.character(round(top * 0.1, digits = 0)))
        step <- round(top * 0.1, digits = ifelse(var == 1, 1, -var + 1))
        graph <- ggplot2::ggplot(data_cut) +
        ggplot2::geom_bar(ggplot2::aes_string(x = col1, fill = col2)) +
        ggplot2::scale_y_continuous(breaks = seq(from = 0, to = top, by = step)) +
        ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold")) +
        ggplot2::ggtitle("Number of presence")

        ggplot2::ggsave(paste("number_in_", loc, ".png"), graph)
      }else{
        cat(paste0("\n", loc, " had more than 40 species and plot isn't readable please select a higher taxon level or cut your data"))
      }
    }
  }else{
  top <- nrow(data)
  var <- nchar(as.character(round(top * 0.1, digits = 0)))
  step <- round(top * 0.1, digits = ifelse(var == 1, 1, -var + 1))
  graph <- ggplot2::ggplot(data) +
  ggplot2::geom_bar(ggplot2::aes_string(x = col1, fill = col2)) +
  ggplot2::scale_y_continuous(breaks = seq(from = 0, to = top, by = step)) +
  ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold")) +
  ggplot2::ggtitle("Number of individuals")

  ggplot2::ggsave("number.png", graph)
  }
}

rare <- function(data, spe, abond, rare, num) {
# Put the data in form
  new_data <- as.data.frame(data[, spe])
  colnames(new_data) <- c("Species")
  new_data$total <- data[, abond]

  new_data$rarefaction <- as.integer(rare)

  end <- length(unique(new_data$Species))
  out <- vegan::rarecurve(new_data[, 2:3], step = 10, sample = rarefy_sample, label = FALSE)
  names(out) <- paste(unique(new_data$Species), sep = "")

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

  if (mult2) {
    for (spe in unique(data[, spe])) {
      xy_cut <- xy[xy$species == spe, ]
      top <- max(xy_cut$subsample)
      var <- nchar(as.character(round(top * 0.1, digits = 0)))
      step <- round(top * 0.1, digits = ifelse(var == 1, 1, -var + 1))
      courbe <- ggplot2::ggplot(xy_cut, ggplot2::aes(x = subsample, y = value)) +
      ggplot2::theme_gray() +
      ggplot2::geom_line(size = 1) +
      ggplot2::scale_x_continuous(breaks = seq(from = 0, to = top, by = step)) +
      ggplot2::xlab("Abundance") + ggplot2::ylab("Value") +
      ggplot2::ggtitle("rarefaction curve")

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
    ggplot2::ggtitle("rarefaction curves")

    ggplot2::ggsave("rarefaction.png", courbe)
  }
}

if (abundance) {
#Maps
mult0 <- ifelse(length(unique(data[, colspe])) > 10, FALSE, TRUE)
graph_map(data, collong = collong, collat = collat, colabond = colabond, ind = ind, colspe = colspe)
}

if (presabs) {
#Presence absence count
mult1 <- ifelse(length(unique(data[, colloc])) <= 10, FALSE, TRUE)
graph_hist(data, col1 = colloc, col2 = colspe, col3 = colabond)
}

if (rarefaction) {
#rarefaction

#### rarefaction indice ####
rarefy_sample <- as.numeric(num)

## Calcul de la rarefaction
rarefaction <- vegan::rarefy(data[, abond], rarefy_sample)

write.table(rarefaction, "rare.tabular")

mult2 <- ifelse(length(unique(data[, colspe])) <= 30, FALSE, TRUE)
rare(data, spe = colspe, abond = colabond, rare = rarefaction, num = rarefy_sample)
}

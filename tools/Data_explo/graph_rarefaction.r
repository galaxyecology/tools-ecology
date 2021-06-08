#Rscript

########################
##    Rarefaction     ##
########################
                                                                       
#####Packages : vegan
#               ggplot2

#####Load arguments

args = commandArgs(trailingOnly = TRUE)

if(length(args)< 2)
{
    stop("This tool needs at least 3 arguments")
}else{
    Table <- args[1]
    HR <- args[2]
    abond <- as.numeric(args[3])
    spe <- as.numeric(args[4])
    num <- as.character(args[5])
	
}

if (HR == "false"){HR <- FALSE} else {HR <- TRUE}

#####Import data
Data <- read.table(Table, sep = "\t", dec = ".", header = HR, fill = TRUE, encoding = "UTF-8")
colabond <- colnames(Data)[abond]
colspe <- colnames(Data)[spe]

Data <- Data[grep("^$", Data[, colspe], invert = TRUE), ]
#### Your Analisys

#### Rarefaction indice ####
rarefy.sample <- as.numeric(num)

#Calcul de la rarefaction
rarefaction <- vegan::rarefy(Data[,abond], rarefy.sample)
#Data$rarefaction <- capture.output(vegan::rarefy(Data[,abond], rarefy.sample))

#cat("\nwrite table with Data and rarefaction. \n--> \"", paste(Data,"\"\n", sep=""), file = "Data.tabular", sep = "", append = TRUE)
write.table(rarefaction, "Data.tabular")

Rare <- function(Data, spe, abond, rare, num){

  #Mettre les données en forme
  New_Data <- as.data.frame(Data[,spe])
  colnames(New_Data) <- c("Species")
  New_Data$Total <- Data[,abond]

  New_Data$rarefaction <- as.integer(rare)


  end <- length(unique(New_Data$Species))
  out <- vegan::rarecurve(New_Data[,2:3], step = 10, sample = rarefy.sample, label = FALSE)
  names(out) <- paste(unique(New_Data$Species), sep = "")

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

  if(mult){
    for(spe in unique(Data[, spe])){ 
      xy.cut <- xy[xy$species == spe, ]
      top <- max(xy.cut$subsample)
      var = nchar(as.character(round(top*0.1, digits = 0)))
      step <- round(top*0.1, digits = ifelse(var == 1, 1, -var + 1))
      courbe <- ggplot2::ggplot(xy.cut, ggplot2::aes(x = subsample, y = value)) +
      ggplot2::theme_gray() +
      #ggplot2::scale_color_discrete(guide = FALSE) +  # turn legend on or off
      ggplot2::geom_line(size = 1) +
      ggplot2::scale_x_continuous(breaks=seq(from = 0, to = top, by = step)) +
      ggplot2::xlab("Abundance") + ggplot2::ylab("Value") +
      ggplot2::ggtitle("Rarefaction curve")
  
      ggplot2::ggsave(paste("rarefaction_of_", spe, ".png"), courbe)
    }
  }else{
    top <- max(xy$subsample)
    var = nchar(as.character(round(top*0.1, digits = 0)))
    step <- round(top*0.1, digits = ifelse(var == 1, 1, -var + 1))
    courbe <- ggplot2::ggplot(xy, ggplot2::aes(x = subsample, y = value, color = species)) +
    ggplot2::theme_gray() +
    #ggplot2::scale_color_discrete(guide = FALSE) +  # turn legend on or off
    ggplot2::geom_line(size = 1) +
    ggplot2::scale_x_continuous(breaks=seq(from = 0, to = top, by = step)) +
    ggplot2::xlab("Subsample") + ggplot2::ylab("Value") +
    ggplot2::ggtitle("Rarefaction curves")
  
    ggplot2::ggsave("rarefaction.png", courbe)
  } 
}
mult <- ifelse(length(unique(Data[, colspe]))<=30, FALSE, TRUE)
Rare(Data, spe = colspe, abond = colabond, rare = rarefaction, num = rarefy.sample)


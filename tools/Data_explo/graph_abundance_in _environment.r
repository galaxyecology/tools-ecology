#Rscript

####################################
##    Adundance in environment    ##
####################################

#####Packages : ggplot2

#####Load arguments

args = commandArgs(trailingOnly = TRUE)

if(length(args)< 2)
{
    stop("This tool needs at least 3 arguments")
}else{
    Table <- args[1]
        HR <- args[2]
	lat <- as.numeric(args[3])
	long <- as.numeric(args[4])
	abond <- as.numeric(args[5])
        ind <- as.character(args[6])
		
}

if (HR == "false"){HR <- FALSE} else {HR <- TRUE}

#####Import data
Data <- read.table(Table, sep = "\t", dec = ".", header = HR, fill = TRUE, encoding = "UTF-8")
collat <- colnames(Data)[lat]
collong <- colnames(Data)[long]
colabond <- colnames(Data)[abond]

#####Your analysis 

####The abundance in the environment####

##Representation of the environment##

#mapping
graph_map <- function(Data, long, lat, abond, ind){
    mappy <- ggplot2::ggplot(Data, ggplot2::aes_string(x = collong, y = collat, cex = colabond, color = colabond)) +  
  ggplot2::geom_point() + ggplot2::ggtitle(paste("Abundance of", ind, "in the environment")) + ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude")
  ggplot2::ggsave("mappy.png", mappy, width=15, height=9, units="cm")
}
graph_map(Data, long = long, lat = lat, abond = abond, ind = ind)


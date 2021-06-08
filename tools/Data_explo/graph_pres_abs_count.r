#Rscript

#####################################
##    Presence absence abundance   ##
#####################################

#####Packages : ggplot2

#####Load arguments

args = commandArgs(trailingOnly = TRUE)

if(length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    Table <- args[1]
        HR <- args[2]
	loc <- as.numeric(args[3])
	spe <- as.numeric(args[4])
        abond <- as.numeric(args[5])
        
}

if (HR == "false"){HR <- FALSE} else {HR <- TRUE}

#####Import data
Data <- read.table(Table, sep = "\t", dec = ".", header = HR, fill= TRUE, encoding = "UTF-8")
colloc <- colnames(Data)[loc]
colspe <- colnames(Data)[spe]
colabond <- colnames(Data)[abond]

Data <- Data[grep("^$", Data[, colspe], invert = TRUE), ]
#####Your analysis

####Presence/absence/abundance####

write.table(loc, "Locations.tabular")

## Histogram 
graph_hist <- function (Data, col1, col2, col3){
  if(mult){
    for(loc in unique(Data[, col1])){ 
      Data.cut <- Data[Data[, col1] == loc, ] 
      Data.cut <- Data.cut[Data.cut[, col3] > 0, ] 
      top <- nrow(Data.cut)
      var = nchar(as.character(round(top*0.1, digits = 0)))
      step <- round(top*0.1, digits = ifelse(var == 1, 1, -var + 1))
      graph <- ggplot2::ggplot(Data.cut) + 
      ggplot2::geom_bar(ggplot2::aes_string(x= col1 , fill = col2)) +
      ggplot2::scale_y_continuous(breaks=seq(from = 0, to = top, by = step)) +
      ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold")) +
      ggplot2::ggtitle("Number of presence absence")
  
      ggplot2::ggsave(paste("number_in_", loc,".png"), graph)
    }
  }else{
  top <- nrow(Data)
  var = nchar(as.character(round(top*0.1, digits = 0)))
  step <- round(top*0.1, digits = ifelse(var == 1, 1, -var + 1))
  graph <- ggplot2::ggplot(Data) + 
  ggplot2::geom_bar(ggplot2::aes_string(x= col1 , fill = col2)) +
  ggplot2::scale_y_continuous(breaks=seq(from = 0, to = top, by = step)) +
  ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold")) +
  ggplot2::ggtitle("Number of individuals")
  
  ggplot2::ggsave("number.png", graph)
  }
}

mult <- ifelse(length(unique(Data[, colloc]))<=10, FALSE, TRUE)
graph_hist(Data, col1 = colloc, col2 = colspe, col3 = colabond)

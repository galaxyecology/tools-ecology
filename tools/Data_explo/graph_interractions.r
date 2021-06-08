#Rscript

##########################################
##    Interractions between variables   ##
##########################################

#####Packages : Cowplot
#				ggplot2

#####Load arguments

args = commandArgs(trailingOnly = TRUE)

if(length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    Table <- args[1]
	HR <- args[2]
	var1 <- as.numeric(args[3])
	var2 <- as.numeric(args[4])
	spe <- as.numeric(args[5])
	var4 <- as.numeric(args[6])
}

if (HR == "false"){HR <- FALSE} else {HR <- TRUE}
#####Import data
Data <- read.table(Table, sep = "\t", dec = ".", header = HR, fill= TRUE, encoding = "UTF-8")
colvar1 <- colnames(Data)[var1]
colvar2 <- colnames(Data)[var2]
colspe <- colnames(Data)[spe]
colvar4 <- colnames(Data)[var4]


#Data <- Data[grep("^$", Data[, colspe], invert = TRUE), ]
#####Your analysis
write(unique(Data[, colspe]), "species.txt")
####Interractions####

graph <- function(Data, var1, var2, var3){
  graph <- ggplot2::ggplot(Data, ggplot2::aes_string(x = var1, y = var2, group = var3, color = var3)) + 
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method=lm, se=FALSE) +
  ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold"))

return(graph)
}

#Put multiple panels
interraction <- function(Data, var1, var2, var3, var4){

  if(mult){
      for(spe in unique(Data[, var3])){ 
      Data.cut <- Data[Data[, var3] == spe, ]
      mult_graph <- graph(Data.cut, var1, var2, var3) + ggplot2::facet_grid(cols = ggplot2::vars(Data.cut[,var4]), scales = "free") + 
      cowplot::background_grid(major = "xy", minor = "none") +
      cowplot::panel_border() + ggplot2::ggtitle("Interractions")
      
      ggplot2::ggsave(paste("interraction_of_", spe, ".png"), mult_graph, width = 10, height = 7)
      }
    }else{
    mult_graph <- graph(Data, var1, var2, var3) + ggplot2::facet_grid(rows = ggplot2::vars(Data[,var3]), cols = ggplot2::vars(Data[,var4]), scales = "free") + 
    cowplot::background_grid(major = "xy", minor = "none") +
    cowplot::panel_border() + ggplot2::ggtitle("Interractions")
  
    ggplot2::ggsave("interraction.png", mult_graph)
  }
}

mult <- ifelse(length(unique(Data[, colspe]))<=6, FALSE, TRUE)
interraction(Data, var1 = colvar1, var2 = colvar2, var3 = colspe, var4 = colvar4)











#Rscript

#########################################
##    Normality of the distribution    ##
#########################################

#####Packages : ggpubr
#               Cowplot
#		ggplot2

#####Load arguments

args = commandArgs(trailingOnly = TRUE)

if(length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    Table <- args[1]
    var1 <- as.numeric(args[2])
    HR <- args[3]
}

if (HR == "false"){HR <- FALSE} else {HR <- TRUE}

#####Import data
Data <- read.table(Table, sep = "\t", dec = ".", header = HR, fill = TRUE, encoding = "UTF-8")
Data <- na.omit(Data)
colvar1 <- colnames(Data)[var1]

#####Your analysis

####Normality of the distribution####

#Histogramm with distribution line
graph_hist <- function(Data, var1){
  graph_hist <- ggplot2::ggplot(Data) + 
  ggplot2::geom_histogram(ggplot2::aes_string(x = var1), binwidth=2, color="black", fill="white") +
  ggplot2::geom_density(ggplot2::aes_string(var1), alpha=0.12, fill="red")

return(graph_hist)
}

#Add the mean dashed line
add_mean <- function(graph, var1){
  graph_mean <- graph + ggplot2::geom_vline(xintercept = mean(Data[,var1]),
              color="midnightblue", linetype="dashed", size=1)

return(graph_mean)
}

#Adding a QQplot
graph_qqplot <- function(Data, var1){
  graph2 <- ggpubr::ggqqplot(Data, var1, color = "midnightblue")

return(graph2)
}

#On suppose que les données sont distribuées normalement lorsque les points suivent approximativement la ligne de référence à 45 degrés.

graph_fin <- function(graph1, graph2){
  graph <- cowplot::plot_grid(graph1, graph2, labels=c("Distribution 1", "Distribution 2"), label_size = 10, hjust = -0.8, ncol = 2, nrow = 1)
  
  ggplot2::ggsave("Norm_distrib.png", graph, width=10, height=7, units="cm")
}

graph_hist1 <- graph_hist(Data, var1 = colvar1)
graph_mean <- add_mean(graph = graph_hist1, var1 = colvar1)
graph_fin(graph1 = graph_mean, graph2 = graph_qqplot(Data, var1 = colvar1))




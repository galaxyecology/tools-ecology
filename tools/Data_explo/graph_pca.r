#Rscript

####################################
##  Principal composante analysis ##
####################################

#####Packages : ggplot2
#		FactoMineR
#		factoextra
#		ggcorrplot
#               dplyr


#####Load arguments

args = commandArgs(trailingOnly = TRUE)

if(length(args)< 2)
{
    stop("This tool needs at least 3 arguments")
}else{
    Table <- args[1]
	HR <- args[2]
        col <- as.numeric(strsplit(args[3], ",")[[1]])
	
}

if (HR == "false"){HR <- FALSE} else {HR <- TRUE}

#####Import data
Data <- read.table(Table, sep = "\t", dec = ".", header = HR, fill = TRUE, encoding = "UTF-8")
Data.active <- Data[col]
#Define the active individuals and the active variables

####PCA method####

active_data <- function(Data){   
  #Calcul of PCA for the active data
  res.pca <- FactoMineR::PCA(Data, graph = FALSE)

return(res.pca)
}

#eigenvalue
eig.val <- capture.output(factoextra::get_eigenvalue(active_data(Data.active)))

cat("\nwrite table with eigenvalue. \n--> \"", paste(eig.val,"\"\n", sep=""), file = "valeurs.txt", sep = "", append = TRUE)

plot_PCA <- function(Data){
  #Correlation circle
  graph_corr <- factoextra::fviz_pca_var(active_data(Data), col.var = "cos2",
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = TRUE #Avoid text overlap
                           )
  ggplot2::ggsave("circle.png", graph_corr)
}

plot_qual <- function(Data){
  #PCA results for variables
  var <- factoextra::get_pca_var(active_data(Data)) 

  #Representation quality
  graph_quality <- ggcorrplot::ggcorrplot(var$cos2[!apply(var$cos2, 1, anyNA),], method = "circle",
  ggtheme = ggplot2::theme_gray,
   colors = c("#00AFBB", "#E7B800", "#FC4E07"))

  ggplot2::ggsave("quality.png", graph_quality)

}
plot_PCA(Data.active)
plot_qual(Data.active)


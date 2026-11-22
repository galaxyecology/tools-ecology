### 1. Libraries
library(FactoMineR)
library(factoextra)
library(vegan)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(ggplotify)
library(formattable)
library(htmltools)
library(webshot)
webshot::install_phantomjs()

#load arguments
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0)
{
  stop("This tool needs at least 1 arguments")
}else{
  path_df<- args[1]
}

Indexes <- read.csv(path_df, sep=",", row.names = 1, h=T) 
new_complexity_indexes<-read.csv("new_complexity_indexes.csv", sep=",", row.names = 1, h=T) 
new_fractal_dimension<-read.csv("new_fractal_dimension.csv", sep=",", row.names = 1, h=T) 
New_Indexes<-cbind(new_complexity_indexes,new_fractal_dimension) 

df <- rbind(Indexes,New_Indexes)

### 3. Compute the MFA and HCPC and plot the new 3D CAD models

res.mfa <- MFA(df,
               ind.sup = c((dim(base)[1]+1):dim(df)[1]),
               group = c(3,3),
               type = c("s","s"), 
               graph = F)

caskm <- cascadeKM(dist(df,"euclidean"), 2, 5, iter = 100, criterion = "ssi")
res_caskm <- data.frame(t(caskm$results))
clust <- substr(row.names(res_caskm),1,1)
clust_caskm <- cbind.data.frame(ssi=res_caskm$ssi,clust=as.numeric(clust))
nb_clust <- clust_caskm %>%
  filter(ssi == max(ssi)) %>%
  slice(1)  # In case of ties, select the first row

res.hcpc <- HCPC(res.mfa, nb.clust = nb_clust$clust,graph = F, method = "ward")

col_group_var <- c("#C42267","#F8333C")
col_cluster <- colorRampPalette(c("#A7C957","#598A14"))

MFA_var <- fviz_mfa_var(res.mfa, palette = col_group_var,  repel = TRUE, ggtheme = theme_few())

MFA_clust <- fviz_cluster(res.hcpc,
                          cex = 1,
                          palette = col_cluster(4),  
                          ggtheme = theme_few(),
                          repel = TRUE,           
                          show.clust.cent = F,     
                          main = "Cluster map",
                          geom = "point",
                          addEllipses = TRUE,
                          ellipse.type = "convex",
                          ellipse.level = 0.95,
                          ellipse.alpha = 0.2,
                          ellipse.linetype = "dashed",
                          label = "size",
                          axes = c(1, 2))
MFA_clust_with_sup <- fviz_add(MFA_clust, 
                               res.mfa$ind.sup$coord,
                               geom = c("point", "text"),
                               color = "blue",
                               fill = "blue",
                               repel = TRUE)
plot_mfa_hcpc <- grid.arrange(MFA_clust_with_sup, 
             MFA_var, 
             ncol = 2)

# save the mfa plot
ggsave("MFA_clusters.png", plot_mfa_hcpc, width = 10, height = 5)

### 4. Extract MCI from the mfa coordinates the reference models and min and max values

MCI_base <- data.frame(row.names=row.names(base),
                      MCI=round(res.mfa$ind$coord[,1],3))
MCI_ind_sup <- data.frame(row.names=row.names(ind_sup),
                          MCI=round(res.mfa$ind.sup$coord[,1],3))

Indexes_MCI_base <- cbind(base,MCI_base)
Indexes_MCI_ind_sup <- cbind(ind_sup,MCI_ind_sup)

REF <- Indexes_MCI_base %>%
  slice_max(MCI, n = 1, with_ties = FALSE)

MCI_min <- min(Indexes_MCI_base$MCI)
MCI_max <- max(Indexes_MCI_base$MCI)

Indexes_MCI_toplot <- rbind("REF"=REF,Indexes_MCI_ind_sup)
Indexes_MCI_toplot <- round(Indexes_MCI_toplot,3)

### 5. Plot the new models on the Multiscale Complexity Index (MCI) gradient bar

Pal_MCI <- colorRampPalette(c("#DFEBC1","#598A14"))

legend_image <- as.raster(matrix(Pal_MCI(100), nrow=1))
windowsFonts(A = windowsFont("Helvetia")) 

plot_MCI <- as.ggplot(function(){
  par(fig=c(0.05,1,0.2,1), mar = c(0,0,0,2))
  plot(c(1,3)~c(MCI_min,MCI_max),type = 'n', axes = F,xlab = '', ylab = '')
  rasterImage(legend_image,MCI_min,1,MCI_max,2)
  segments(x0=c(MCI_ind_sup$MCI),y0=1,y1=2,col="black")
  segments(x0=c(MCI_max),y0=1,y1=2,col="black")
  text(x=seq(MCI_min,MCI_max,l=5), y = 0.8 , 
       labels = round(seq(MCI_min,MCI_max,l=5),2), cex=1, col="#343A40")
  text(x=c(MCI_ind_sup$MCI), y=2.2, 
       labels = row.names(ind_sup), srt=45, cex=1, col="#343A40")
  text(x=c(MCI_max), y=2.2, 
       labels = "REF", srt=45, cex=1, col="#343A40")
  text(x=2.4, y=1.5, 
       labels = "MCI", srt=90, cex=1.5, col="#343A40")
})
plot_MCI

# save the gradient bar plot
png(file = "MCI.png", width = 8, height = 4, units = "in", res = 300, bg = "white")
print(plot_MCI)
dev.off()

### 5. Create the table with formattable

plot_Indexes_table <- formattable(Indexes_MCI_toplot,
                              list(`R`= color_tile("#F9DCE9", "#C42267"),
                                   `Ht`= color_tile("#F9DCE9", "#C42267"),
                                   `J`= color_tile("#F9DCE9", "#C42267"),
                                   `Pt`= color_tile("#FED8DA", "#F8333C"),
                                   `C`= color_tile("#FED8DA", "#F8333C"),
                                   `Dt`= color_tile("#FED8DA", "#F8333C"),
                                   `MCI` = color_tile("#DFEBC1","#598A14")
                              )
)

# save the table 
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}
plot_Indexes_table
export_formattable(plot_Indexes_table,"Indexes_table.png")

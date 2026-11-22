
library(FactoMineR)
library(factoextra)
library(ggthemes)
library(ggplot2)
library(gridExtra)
library(vegan)

#load arguments
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0)
{
  stop("This tool needs at least 1 arguments")
}else{
  path_df<- args[1]
}

Indexes <- read.csv(path_df, sep=",", row.names = 1, h=T)
base_len<-nrow(Indexes)

new_complexity_indexes<-read.csv("new_complexity_indexes.csv", sep=",", row.names = 1, h=T)
new_fractal_dimension<-read.csv("new_fractal_dimension.csv", sep=",", row.names = 1, h=T)
New_Indexes<-cbind(new_complexity_indexes,new_fractal_dimension)


Indexes<-rbind(Indexes,New_Indexes)
len<-nrow(Indexes)
### I. MFA


res.mfa <- MFA(Indexes,
               # ind.sup = c()
               group = c(3,3),
               type = c("s","s"), 
               graph = F,
               ind.sup = c(base_len:len))

group_col <- c("#1c4587","#b45f06")

MFA_var <- fviz_mfa_var(res.mfa, palette = group_col, ggtheme = theme_few())
MFA_ind <- fviz_mfa_ind(res.mfa, ggtheme = theme_few())

## II. determine the clusters

### Lines below are commented as already computed and not used by the final scripts steps!

# caskm <- cascadeKM(dist(Indexes,"euclidean"), 2, 5, iter = 100, criterion = "ssi")
# plot(caskm)


### III. HCPC


# res.hcpc <- HCPC(res.mfa, nb.clust = 4,graph = F, method = "ward")

# my_palette <- alpha(c("#843734","#444444","#a38064","#086464"),0.5)

# MFA_clust <- fviz_cluster(res.hcpc,
                          cex = 1,
                          palette = my_palette,              
                          ggtheme = theme_few(),
                          repel = TRUE,           
                          show.clust.cent = T,     
                          main = "Cluster map")


### IV. Compute the Multiscale Complexity Index (MCI)

Coord <- as.data.frame(res.mfa$ind$coord)
Coord$cluster <- res.hcpc$data.clust$clust

MCI <- ggplot(Coord) +
  aes(x = cluster, y = Dim.1, fill = cluster) +
  geom_boxplot() +
  scale_fill_manual(values = my_palette) +
  labs(title = "Multiscale Complexity Index")+
  theme_few()


### V. Plot the MFA and the clusters along the MCI


grid.arrange(MFA_ind, 
             MFA_var,
             MCI, 
             nrow = 2, 
             layout_matrix = cbind(c(1,1),c(2,3)))

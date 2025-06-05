#!/bin/Rscript
# phyloregions

args = commandArgs(trailingOnly=TRUE)
#args = c("input/matrix_file", "input/tree_file.txt", "input/grid_aq_3_PF.shp")

# library
library(phyloregion)
library(ape)
library(Matrix)
library(SparseArray)
library(sf)
library(sp)
library(raster)
library(dplyr)


save_sf <- function(){
  #st_write(phyloreg_sf[,-(3:5)], paste0(tempdir(), "/", "output.shp"), delete_layer = TRUE)
  write_sf(phyloreg_sf, "output.shp")
}


if (length(args)<5){stop('Usage : sparseMatrix.csv tree.txt grid.shp nb_clust clust_method')
}else{
  # read enter files  
  comm_tree <- read.tree(args[1])

  comm_matrix <- readSparseCSV(args[2], sep = "\t")
  comm_matrix <- as(comm_matrix,"dgCMatrix")
  
  grid <- read_sf(args[3])

  nb_clust <- as.integer(args[4])

  clust_method <- toString(args[5])
  
  # calculate phylogenetic Beta diversity - a phylogenetic distance matrix between grid cells
  phylo_beta <- phylobeta(comm_matrix, comm_tree, index.family = "sorensen")
  
  #select the less distorting clustering method, best fitting between phylogenetic distances in phylobeta matrix
  # and raw distances from branch lengths of the tree
  select_linkage(phylo_beta[[1]])
  
  #select optimal number of clusters with selected method
  optim <- optimal_phyloregion(phylo_beta[[3]])
  print(paste("the best number of cluster is :", optim$optimal$k))
  #plot(optim$df$k, optim$df$ev, pch = 20)  # k - nbr of clusters VS explained variance given k
  # k has to be selected by a user
  
  # pass the grid cell to spatial format 
  grid_sp <- as_Spatial(grid)
  #proj4string(grid_sp)
  
  # calculate phyloregions clusters
  y <- phyloregion(phylo_beta[[3]], pol = grid_sp, k = nb_clust, method = clust_method)
  #summary(y)
  #phylo_nmds <- y$NMDS
  
  # take an shp spatial file for phyloregions and put it to sf format
  phyloreg_sf <- y$pol
  # print(st_crs(phyloreg_sf))
  #plot(phyloreg_sf)
  phyloreg_sf <- st_as_sf(phyloreg_sf, crs = st_crs(grid))
  # print(st_crs(phyloreg_sf))

  names(phyloreg_sf)[3:8] <- c("R_val", "G_val", "B_val", "r_code", "g_code", "b_code")


  save_sf()
}


# sf_recup <- read_sf("output.shp")


#!/bin/Rscript
# Matice Phylo/Commu

args = commandArgs(trailingOnly=TRUE)
#args = c( "pycnogonida_tree_2024", "pycnogonida_occ_2024.csv")

library(phyloregion)
library(ape)
library(Matrix)
library(SparseArray)

#install.packages("remotes")
#remotes::install_github("Bioconductor/SparseArray")

save_as_tabular_fun <- function(){
  #writeMM(obj = comm_matrix, file = "output.mtx")
  writeSparseCSV(comm_matrix, out_grid, sep="\t", transpose=FALSE, write.zeros=FALSE)
}


save_as_tree_fun <- function(){
  write.tree(comm_tree, file = out_tree)
}


if (length(args)<4){stop('Usage : input_tree_file input_grid_file output_tree_file output_grid_file')
}else{
  tree <- read.tree(args[1])
  grid <- read.csv2(args[2], dec = ".", sep = "\t", header = T, na.strings = "")
  out_tree <- args[3]
  out_grid <- args[4]
  
  sparse_grid <- long2sparse(grid, grid = "grids", species = "newscientificname")
  
  com.data <- match_phylo_comm(tree, sparse_grid, delete_empty_rows = F)
  
  comm_matrix <- com.data$comm
  #test
  #sum_matrix <- summary(comm_matrix)
  
  comm_tree <- com.data$phy

  save_as_tabular_fun()
  save_as_tree_fun()
}

#comm_matrix_recup <- readSparseCSV("output")
#comm_matrix_recup <- as(comm_matrix_recup,"dgCMatrix")

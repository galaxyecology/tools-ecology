#!/bin/Rscript
# Indice de phylodiversite


# args 
args = commandArgs(trailingOnly=TRUE)
#args = c("comm_matrix", "comm_tree.txt")

# library
library(Matrix)
library(ape)
library(phyloregion)
library(SparseArray)

# functions
write_results <- function(){
  write.table(ses_PD, output_file, sep = "\t", row.names=FALSE)
}

# verif args
if (length(args)<5){stop('Usage : input_tree_file input_matrix_file random_seed clustering_model output_file')
}else{
  # read enter file
  comm_tree <- read.tree(args[1])

  comm_matrix <- readSparseCSV(args[2] , sep="\t")
  comm_matrix <- as(comm_matrix,"dgCMatrix")

  seed <- args[3]

  model <- args[4]

  output_file <- args[5]

  # rdm + compute
  set.seed(seed)
  ses_PD <- PD_ses(comm_matrix, comm_tree, model = model, reps = 999)
  
  # write res 
  write_results()
}

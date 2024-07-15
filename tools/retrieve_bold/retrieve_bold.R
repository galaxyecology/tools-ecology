#!/bin/Rscript

library(bold)

args = commandArgs(trailingOnly=TRUE)

raw_marker_list <- paste(args[2],args[3],args[4],args[5],args[6], sep= ",")
marker_list_W_none <- unique(strsplit(raw_marker_list, ",")[[1]])
marker_list <- marker_list_W_none[marker_list_W_none != "None"]
cat("researched marker(s):", marker_list, "\n\n")

#Functions to retrieve the subtaxa of each family ((get)subtaxa) and search in Bold and download the available sequences of each subtaxa (get_fasta)
get_fasta<-function(taxon,filename,arg_mark){
  bold_res<-bold_seqspec(taxon=taxon)
  cat(taxon, "marker list:", unique(bold_res$markercode), "\n")
  x <- data.frame()
  for (mark in arg_mark){x <- rbind(x, bold_res[bold_res$markercode == mark,])}
  if (dim(x)[1] == 0){return(cat("no sequences were found with selected marker(s) for", taxon, "see existing marker list above",  "\n"))}
  x[x==""]  <- NA 
  b_acc <- x$processid
  b_tax <- ifelse(!is.na(x$species_name),x$species_name,ifelse(!is.na(x$genus_name),x$genus_name,ifelse(
    !is.na(x$family_name),x$family_name,ifelse(
      !is.na(x$order_name),x$order_name,ifelse(
        !is.na(x$class_name),x$class_name,x$phylum_name)))))
  b_mark <- x$markercode
  n_acc  <- ifelse(!is.na(x$genbank_accession),ifelse(!is.na(x$genbank_accession),paste0("|",x$genbank_accession),""),"")
  
  seq <- x$nucleotides
  seqname <- paste(b_acc,b_tax,b_mark,sep="|")
  seqname <- paste0(seqname,n_acc)
  Y <- cbind(seqname,seq)
  colnames(Y) <- c("name","seq")
  fastaLines = c()
  for (rowNum in 1:nrow(Y)){
    fastaLines = c(fastaLines, as.character(paste(">", Y[rowNum,"name"], sep = "")))
    fastaLines = c(fastaLines,as.character(Y[rowNum,"seq"]))
  }
  writeLines(fastaLines,filename)
}



taxlist  <- readLines(file(as.character(args[1])))

for (i in 1:length(taxlist)) {
    cat("Processing ", taxlist[i], "\n")
    tryCatch({
        get_fasta(taxlist[i],paste0(taxlist[i],"bold",".fasta"),marker_list)}, 
        error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
   )
}














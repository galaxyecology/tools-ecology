#28/11/2023
#Seguineau Pauline
#Make taxonomic coverage template

#Load packages

library(EMLassemblyline)

#Load arguments

args = commandArgs(trailingOnly=TRUE)
if(length(args)>0){
  data_taxa <- args[1]
  taxa_table <- args[2]
  taxa_col <- as.numeric(args[3])
  taxa_name_type <- args[4]
  authority <- as.numeric(args[5])
  authority2 <- as.numeric(args[6])
  authority3 <- as.numeric(args[7])
  empty <- args[8]
}

#transfom arguments
taxatable = read.table(data_taxa,header=T,sep="\t")
taxacol = names(taxatable[taxa_col])

if (authority2 == 0 && authority3==0){
   authority_f = authority}
   
if(authority2 == 0 && authority3 != 0){
   authority_f = c(authority,authority3)}

if (authority2 !=0 && authority3==0){
   authority_f = c(authority,authority2)}

if (authority3 !=0 && authority2 !=0){
   authority_f = c(authority,authority2,authority3)}
   
if (empty == "false"){
   empty = FALSE
}else if (empty=="true"){
   empty=TRUE}


#Make template

template_taxonomic_coverage(path =".", data.path = "data_files", taxa.table = taxa_table, taxa.col = taxacol, taxa.name.type = taxa_name_type , taxa.authority = authority_f, empty = empty)

#Date : 22/05/2024
#Author : Seguineau Pauline & Yvan Le Bras 

#Load libraries
library(tidyr)

#load file 

args = commandArgs(trailingOnly=TRUE) 
if (length(args)==0)
{
  stop("This tool needs at least one argument")
}else{
  input_file <- args[1]
  names_from <-  as.integer(args[2])
  values_from <-  as.integer(args[3])
  values_fill<- type.convert(args[4], as.is = TRUE)
  funct <- switch(args[5],
                  "sum" = sum,
                  "mean" = mean,
                  "median" = median,
                  "length" = length,
                  "n_distinct" = function(x) length(unique(x)),
                  "max" = max,
                  "min" = min,
                  stop("Unknown aggregation function")
  )
  replace_occ<- as.logical(args[6])
}

file = read.table(input_file,header=T, sep = "\t")

#Run pivot_wider function
pivot_file = pivot_wider(data = file,
                        names_from =  all_of(names_from),
                        values_from = all_of(values_from),
                        values_fill = values_fill,
                        values_fn = funct)

#Replace all occurences >= 1 by 1 to have only presence (1) or absence (0) data
if (replace_occ){
  for(c in 3:length(pivot_file)){
      pivot_file[c][pivot_file[c]>=1] <- 1}}

write.table(pivot_file, "pivot_file.tabular", sep = "\t", quote = F, row.names = F)

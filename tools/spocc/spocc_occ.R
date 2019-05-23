#!/usr/bin/Rscript

library(spocc)


##Def functions :
help<-function(){
    cat("HELP\n")
    cat("Spocc::occ, Search on a single species name, or many. And search across a single or many data sources.\n\n")
    cat("Need 3 args :\n")
    cat("    - query : (character) One to many scientific names.\n")
    cat("    - from : (character)  Data source to get data from, any combination of gbif, bison, inat,ebird, ecoengine and/or vertnet.\n")
    cat("    - limit : (numeric) Number of records to return. This is passed across all sources.\n")
    q("no")
}

formatSpName <- function(spName) paste(strsplit(spName, split=' ')[[1]], collapse='_') ###Wallace function

####################################

args = commandArgs(trailingOnly=TRUE)

#Help display
if(args[1]=="-h" || args[1]=="--help" || length(args)<3){help()}

#Get args
sname<-args[1]
dbase_input<-args[2]
max<-as.integer(args[3])

#Get all databases
bases<-strsplit(dbase_input,",")
dbase<-c()
for (base in bases){
    dbase<-c(dbase,base)
}

#Get occurrences
results <- spocc::occ(query=sname, from=dbase, limit=max, has_coords=TRUE)

#Dispay results
if(length(dbase)==1){
    results_data <- results[[dbase[1]]]$data[[formatSpName(sname)]]
}else{
    res <- occ2df(results)
    results_data <- res
}

results_data<-as.matrix(results_data)

#If empty
if(length(results_data)==0){cat("\nNo occurrences found.\nLittle tip : Check your input typo, some databases are case sensitive : Genus species.\n")}

#Write them
write.table(file="output.tab",results_data,sep="\t",row.names=FALSE)

q('no')

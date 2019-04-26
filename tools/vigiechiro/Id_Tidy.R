suppressMessages(library(data.table))

args <- commandArgs(trailingOnly = TRUE)
#print(args)
filename=args[2]

DataPar=fread(args[1]) #ids
DataPar$participation=substr(filename,nchar(filename)-40,nchar(filename)-17)
#DataPar$participation=substr(args[1],nchar(args[1])-40,nchar(args[1])-17)
test1=duplicated(cbind(DataPar$'nom du fichier',DataPar$tadarida_taxon))
test2=(DataPar$tadarida_taxon=="empty")
DataPar=subset(DataPar,(!test1)|(test2))
DataPar$tadarida_probabilite[DataPar$tadarida_probabilite==""]="0"
DataPar$tadarida_probabilite=as.numeric(DataPar$tadarida_probabilite)

#write.table(DataPar,paste0(substr(args[1],nchar(args[1])-40,nchar(args[1])-17),"-DataCorrC2.csv"),row.names=F,sep="\t")
write.table(DataPar,"Id_tidy.tabular",row.names=F,sep="\t")

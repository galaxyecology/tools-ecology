#!/usr/bin/env Rscript

suppressMessages(library(data.table))
suppressMessages(library(randomForest))
args <- commandArgs(trailingOnly = TRUE)

set.seed(1) #To test reproductibility

filename=args[3]
if (exists("ClassifEspC2b")==F){load(args[2])}

DataPar=fread(args[1],na.strings="") #id to be corrected
DataPar$participation=substr(filename,nchar(filename)-40,nchar(filename)-17)
test1=duplicated(cbind(DataPar$'nom du fichier',DataPar$tadarida_taxon))
test2=(DataPar$tadarida_taxon=="empty")
DataPar=subset(DataPar,(!test1)|(test2))
DataPar$tadarida_probabilite[DataPar$tadarida_probabilite==""]="0"
DataPar$tadarida_probabilite=as.numeric(DataPar$tadarida_probabilite)


#table counting number of contacts per species 
nbcT=as.matrix(table(DataPar$participation,DataPar$tadarida_taxon))

DataPar$tadarida_probabilite=as.numeric(DataPar$tadarida_probabilite)

#generating input variables for second layer classification

Q25=vector()
Q50=vector()
Q75=vector()
Q90=vector()
Q95=vector()
Q98=vector()
Q100=vector()
compt=0
PropSp=nbcT[0,]
VoteO=DataPar[0,]
for (j in 1:nlevels(as.factor(DataPar$tadarida_taxon)))
    {
      Datasub2=subset(DataPar,DataPar$tadarida_taxon==levels(as.factor(DataPar$tadarida_taxon))[j])
      
      Q25=c(Q25,rep(quantile(Datasub2$tadarida_probabilite,0.25),nrow(Datasub2)))
      Q50=c(Q50,rep(quantile(Datasub2$tadarida_probabilite,0.50),nrow(Datasub2)))
      Q75=c(Q75,rep(quantile(Datasub2$tadarida_probabilite,0.75),nrow(Datasub2)))
      Q90=c(Q90,rep(quantile(Datasub2$tadarida_probabilite,0.90),nrow(Datasub2)))
      Q95=c(Q95,rep(quantile(Datasub2$tadarida_probabilite,0.95),nrow(Datasub2)))
      Q98=c(Q98,rep(quantile(Datasub2$tadarida_probabilite,0.98),nrow(Datasub2)))
      Q100=c(Q100,rep(max(Datasub2$tadarida_probabilite),nrow(Datasub2)))
      Ncont1=nrow(Datasub2)
      VoteO=rbind(VoteO,Datasub2)
      PropSp0=nbcT/Ncont1
      PropSp=rbind(PropSp,PropSp0[rep(seq_len(nrow(PropSp0)),nrow(Datasub2)),])
      compt=compt+nrow(Datasub2)
      #print(paste(compt,levels(as.factor(DataPar$tadarida_taxon))[j]))
    }

VoteC2=cbind(VoteO,PropSp,Q25,Q50,Q75,Q90,Q95,Q98,Q100)


#editing column titles to identify var of type "proportion d'abondances"
for (i in 15:(ncol(VoteC2)-7))
{
  colnames(VoteC2)[i]=paste0(names(VoteC2)[i],"_prop")
}

#Add missing species
EspForm=subset(row.names(ClassifEspC2b$importance)
               ,substr(row.names(ClassifEspC2b$importance)
                       ,nchar(row.names(ClassifEspC2b$importance))-4
                       ,nchar(row.names(ClassifEspC2b$importance)))
               =="_prop")
test=match(EspForm,colnames(VoteC2))
EspM=subset(EspForm,is.na(test))
Zeros=matrix(nrow=nrow(VoteC2),ncol=length(EspM))
Zeros[is.na(Zeros)]=0
colnames(Zeros)=EspM
VoteC2=cbind(VoteC2,Zeros)

ListDV=levels(as.factor(DataPar$'nom du fichier'))
#calcule les probabilités max par espèce et par fichier
#(utile pour corriger les erreurs dues à la coexistence de taxons dans le même fichier
#ex: cris sociaux de Pipistrelles identifiées comme autre chose (Noctule, oreillard...))
#comptue max proba per species and files
#(useful to correct errors that came from multiple taxons in the same file
#eg ; Pipistrelles socials shouting identified as something else (Noctule, oreillard..))

MaxI=tapply(DataPar$tadarida_probabilite
            ,INDEX=list(c(DataPar$'nom du fichier'),c(DataPar$tadarida_taxon))
            ,FUN=max)
MaxI2=as.data.frame(cbind(row.names(MaxI),MaxI))
for (i in 2:ncol(MaxI2))
{
  MaxI2[,i]=as.numeric(as.character(MaxI2[,i]))
}
MaxI2[is.na(MaxI2)]=0

#édition des titres de colonne pour identifier les variables de type "indices max"
#editing col titles to identify "indices max" variables
for (i in 2:(ncol(MaxI2)))
{
  colnames(MaxI2)[i]=paste0(names(MaxI2)[i],"_maxI")
}


#add missing species
EspForm=subset(row.names(ClassifEspC2b$importance)
               ,substr(row.names(ClassifEspC2b$importance)
                       ,nchar(row.names(ClassifEspC2b$importance))-4
                       ,nchar(row.names(ClassifEspC2b$importance)))
               =="_maxI")
test=match(EspForm,colnames(MaxI2))
EspM=subset(EspForm,is.na(test))
Zeros=matrix(nrow=nrow(MaxI2),ncol=length(EspM))
Zeros[is.na(Zeros)]=0
colnames(Zeros)=EspM
MaxI2=cbind(MaxI2,Zeros)




#indice de confiance à l'echelle de l'observation (groupe de cris identifié comme provenant d'une seule espèce par la première couche)
#Confidence indice on obs scale (shoutings groups identified as comming from a single species from the first layer)
if(exists("IdS3")){rm(IdS3)}
for (i in 1:nlevels(as.factor(DataPar$tadarida_taxon)))
{
  Idsub=subset(DataPar,DataPar$tadarida_taxon==levels(as.factor(DataPar$tadarida_taxon))[i])
  IdS2=cbind('nom du fichier'=Idsub$'nom du fichier',tadarida_taxon=Idsub$tadarida_taxon,prob=Idsub$tadarida_probabilite)
  colnames(IdS2)[3]=paste(levels(as.factor(DataPar$tadarida_taxon))[i])
  if(exists("IdS3")){IdS3=merge(IdS3,IdS2,all=T)}else{IdS3=IdS2}
}

for (i in 3:ncol(IdS3))
{
  IdS3[,i]=as.numeric(as.character(IdS3[,i]))
}

#édition des titres de colonne pour identifier les variables de type "indices de l'observation"
#editing col titles to identify "indices de l'observation" variables
for (i in 3:(ncol(IdS3)))
{
  colnames(IdS3)[i]=paste0(names(IdS3)[i],"_ValI")
}

IdS3[is.na(IdS3)]=0

#add missing species
EspForm=subset(row.names(ClassifEspC2b$importance)
               ,substr(row.names(ClassifEspC2b$importance)
                       ,nchar(row.names(ClassifEspC2b$importance))-4
                       ,nchar(row.names(ClassifEspC2b$importance)))
               =="_ValI")
test=match(EspForm,colnames(IdS3))
EspM=subset(EspForm,is.na(test))
Zeros=matrix(nrow=nrow(IdS3),ncol=length(EspM))
Zeros[is.na(Zeros)]=0
colnames(Zeros)=EspM
IdS3=cbind(IdS3,Zeros)

#on merge les prop d'espèces, les quantiles et les indices par fichiers et par observations
#merge species probabilities, quantiles and indice per files and per obs
VoteC3=merge(VoteC2,MaxI2,by.x="nom du fichier",by.y="V1")
VoteC4=merge(VoteC3,IdS3,by=c("nom du fichier","tadarida_taxon"))
VoteC4$temps_fin=as.numeric(as.character(VoteC4$temps_fin))
VoteC4$temps_debut=as.numeric(as.character(VoteC4$temps_debut))
VoteC4$frequence=as.numeric(as.character(VoteC4$frequence_mediane))
VoteC4$durseq=VoteC4$temps_fin-VoteC4$temps_debut

ProbEsp_C2b=predict(ClassifEspC2b,VoteC4,type="prob",norm.votes=TRUE)
ProbEsp_C2bs=predict(ClassifEspC2b,VoteC4,type="response",norm.votes=TRUE)

colnum=match("participation",colnames(VoteC4))
DataCorrC2=cbind(VoteC4[,1:colnum],ProbEsp_C2b,ProbEsp_C2bs)
DataCorrC2=DataCorrC2[order(DataCorrC2$tadarida_probabilite,decreasing=T),]
DataCorrC2=DataCorrC2[order(DataCorrC2$'nom du fichier'),]

DataCorrC2$ProbEsp_C2bs=as.character(DataCorrC2$ProbEsp_C2bs)
DataCorrC2$ProbEsp_C2bs[is.na(DataCorrC2$ProbEsp_C2bs)]="empty"

fout_name="output.tabular"

write.table(DataCorrC2,file=fout_name,row.names=FALSE,sep="\t",quote=FALSE,na="NA")

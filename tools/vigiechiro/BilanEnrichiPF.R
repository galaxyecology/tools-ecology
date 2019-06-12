#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

#print(args)

EchelleErreur=c("","POSSIBLE","PROBABLE","SUR")
EchelleNumErreur=c(99,50,10,1)

suppressMessages(library(data.table))
suppressMessages(library(DT))
suppressMessages(library(htmlwidgets))

f2p <- function(x) #get date-time data from recording file names
{
  if (is(x)[1] == "data.frame") {pretemps <- vector(length = nrow(x))}
  op <- options(digits.secs = 3)
  pretemps <- paste(substr(x, nchar(x) - 18, nchar(x)-4), ".", substr(x, nchar(x) - 2, nchar(x)), sep = "")
  strptime(pretemps, "%Y%m%d_%H%M%OS",tz="UTC")-7200
}



IdC2=fread(args[1],encoding="UTF-8")

if(substr(IdC2$`nom du fichier`[1],2,2)!="a")
{
#  print("Protocole non conforme, ce script doit etre lance uniquement pour un protocole Point Fixe")
  print("Wrong protocol, please only use this tool for a \'Point Fixe\' protocol.")
}else{
  refPF=fread(args[2],encoding="UTF-8")
  GroupList=fread(args[3],encoding="UTF-8")
  
  IdC2$ConfV[is.na(IdC2$ConfV)]=""
  
  
  #compute error risk by species (minimum error among files)
  #to be replaced by glm outputs if I'll have time
  RisqueErreurT=aggregate(IdC2$IdProb,by=list(IdC2$IdExtrap)
                          ,FUN=function(x) round((1-max(x))*100))
  barplot(RisqueErreurT$x,names.arg=RisqueErreurT$Group.1,las=2)
  #compute error risk accoring to observer/validator (a little dirty because it relies on alphabetical order of confidence classes: POSSIBLE < PROBABLE < SUR)
  RisqueErreurOV0=match(IdC2$ConfV,EchelleErreur)
  RisqueErreurOV=aggregate(RisqueErreurOV0,by=list(IdC2$IdExtrap)
                           ,FUN=max) 
  RisqueErreurOV2=EchelleNumErreur[RisqueErreurOV$x]
  #compute minimum error risk between man and machine
  RisqueErreur=pmin(RisqueErreurT$x,RisqueErreurOV2)
  
  #compute number of files validated per species
  FichValid=aggregate(IdC2$IdV,by=list(IdC2$IdExtrap,IdC2$'nom du fichier')
                      ,FUN=function(x) sum(x!="")) 
  NbValid2=aggregate(FichValid$x,by=list(FichValid$Group.1),FUN=function(x) sum(x>0))
  
  DiffC50=vector() # to store the median of confidence difference between unvalidated records and validated ones
  DiffT50=vector() # to store the median of time difference between unvalidated records and validated ones
  for (j in 1:nlevels(as.factor(IdC2$IdExtrap)))
  {
    IdSp=subset(IdC2
                ,IdC2$IdExtrap==levels(as.factor(IdC2$IdExtrap))[j])
    IdSp=IdSp[order(IdSp$IdProb),]
    IdSpV=subset(IdSp,IdSp$IdV!="")
    if(nrow(IdSpV)>0)
    {
      cuts <- c(-Inf, IdSpV$IdProb[-1]-diff(IdSpV$IdProb)/2, Inf)
      CorrC=findInterval(IdSp$IdProb, cuts)
      CorrC2=IdSpV$IdProb[CorrC]
      DiffC=abs(IdSp$IdProb-CorrC2)
      DiffC50=c(DiffC50,median(DiffC))
      
      IdSp=IdSp[order(IdSp$TimeNum),]
      IdSpV=subset(IdSp,IdSp$IdV!="")
      cuts <- c(-Inf, IdSpV$TimeNum[-1]-diff(IdSpV$TimeNum)/2, Inf)
      CorrT=findInterval(IdSp$TimeNum, cuts)
      CorrT2=IdSpV$TimeNum[CorrT]
      DiffT=abs(IdSp$TimeNum-CorrT2)
      DiffT50=c(DiffT50,median(DiffT))
    }else{
      DiffC50=c(DiffC50,Inf)
      DiffT50=c(DiffT50,Inf)
    }
  }
  #compute an index of validation effort per species
  EffortV=1/DiffC50/DiffT50
  EffortClass=(EffortV>0.0005)+(EffortV>0.005)+RisqueErreurOV$x
  cbind(RisqueErreurOV,EffortV,DiffC50,DiffT50)
  #barplot(EffortClass-1,names.arg=NbValid2$Group.1,las=2)
  ClassEffortV=c("-","FAIBLE","SUFFISANT","SUFFISANT","FORT","FORT")
  EffortClassMot=ClassEffortV[EffortClass]
  
  
  #get date-night
  pourDateNuit=IdC2$TimeNum-16*3600 #bricolage-decalage de 12 heures pour ramener a la date du debut de nuit
  DateNuit=as.Date.POSIXct(pourDateNuit) # date of the beginning of the night
  DateJour=as.Date.POSIXct(IdC2$TimeNum) # date (UTC+0)
  IdC2$DateNuit=DateNuit
  IdC2$DateJour=DateJour
  NbNuit=as.numeric(max(IdC2$DateNuit)-min(IdC2$DateNuit))+1
  
  #compare activity / reference frame
  ActMoy=aggregate(IdC2$`nom du fichier`
                   ,by=list(IdC2$IdExtrap),FUN=function(x) length(x)/NbNuit)
  ListSpref=match(ActMoy$Group.1,refPF$Espece)
  Subref=refPF[ListSpref]
  QualifAct=vector()
  for (k in 1:nrow(ActMoy))
  {
    if(is.na(Subref$Q25[k]))
    {
      QualifAct=c(QualifAct,NA)
    }else{
      cuts=cbind(-Inf,as.numeric(Subref$Q25[k]),as.numeric(Subref$Q75[k])
                 ,as.numeric(Subref$Q98[k]),Inf)
      
      QualifAct=c(QualifAct,findInterval(ActMoy$x[k],cuts,left.open=T))
    }
  }
  ClassAct=c("FAIBLE","MODEREE","FORTE","TRES FORTE")
  QualifActMot=ClassAct[QualifAct]
  
  #organize the csv summary
  SummPart0=cbind(Esp=levels(as.factor(IdC2$IdExtrap))
                  ,RisqueErreur,NbValid=NbValid2$x,EffortValid=EffortClassMot
                  ,Contacts_Nuit=round(ActMoy$x),Niveau_Activite=QualifActMot)
  
  
  InfoSp=c("GroupFR","NomFR","Scientific name","Esp")
  GroupShort=GroupList[,..InfoSp]
  SummPart=merge(GroupShort,SummPart0,by="Esp")
  IndexGroupe=c("Autre","Sauterelle","Chauve-souris")
  SummPart$IndexSumm=match(SummPart$GroupFR,IndexGroupe)
  SummPart=SummPart[with(SummPart
                         ,order(IndexSumm,as.numeric(Contacts_Nuit),decreasing=T)),]
  colnames(SummPart)=c("Code","Groupe","Nom francais","Nom scientifique"
                       ,"Risque d'erreur (%)","Nb Validations"
                       ,"Effort de validation","Nb de Contacts par Nuit"
                       ,"Niveau d'Activite","TriGroupe")
  
  #to do: extend colors to other columns to improve readability
  SummHTML=datatable(SummPart, rownames = FALSE) %>%
    formatStyle(columns = c("Code","Groupe","Nom francais","Nom scientifique","Risque d'erreur (%)"),valueColumns="Risque d'erreur (%)", 
                background = styleInterval(c(1, 10, 50), c("white", "khaki", "orange", "orangered"))) %>%
    formatStyle(columns = "Effort de validation", 
                background = styleEqual(c("-","FAIBLE","SUFFISANT","FORT"), c("white", "cyan", "royalblue", "darkblue"))) %>%
    formatStyle(columns = c("Nb de Contacts par Nuit","Niveau d'Activite"),valueColumns="Niveau d'Activite",
                background = styleEqual(c("FAIBLE","MODEREE","FORTE","TRES FORTE"), c("palegoldenrod", "greenyellow", "limegreen", "darkgreen")))
  
  saveWidget(SummHTML,"output-summary.html")
#  write.csv2(SummPart,"output-summary.tabular",row.names=F)
  write.table(SummPart,"output-summary.tabular",row.names=F,sep="\t")
  
  #compute number of files validated per night/hour
  IdC2$Heure=sapply(IdC2$`nom du fichier`,FUN=function(x) substr(x,nchar(x)-9,nchar(x)-8))
  
  ActNuit=aggregate(IdC2$`nom du fichier`,by=list(IdC2$IdExtrap,IdC2$Session),FUN=length)
  ListSpref=match(ActNuit$Group.1,refPF$Espece)
  Subref=refPF[ListSpref]
  
  
  QualifActN=vector()
  for (k in 1:nrow(ActNuit))
  {
    if(is.na(Subref$Q25[k]))
    {
      QualifActN=c(QualifActN,NA)
    }else{
      cuts=cbind(-Inf,as.numeric(Subref$Q25[k]),as.numeric(Subref$Q75[k])
                 ,as.numeric(Subref$Q98[k]),Inf)
      
      QualifActN=c(QualifActN,findInterval(ActNuit$x[k],cuts,left.open=T))
    }
  }
  ActNuit$QualifActN=QualifActN
  
  ActNuitT=dcast(data=ActNuit,formula=Group.1~Group.2
                 ,value.var="x")
  ActNuitT[is.na(ActNuitT)]=0
  RefNuitT=dcast(data=ActNuit,formula=Group.1~Group.2
                 ,value.var="QualifActN")
  ARNuit=merge(ActNuitT,RefNuitT,by="Group.1")
  
  SummPartshort=cbind(SummPart[,c(1:5)],TriGroupe=SummPart[,TriGroupe])
  SummPartN=merge(SummPartshort,ARNuit,by.x="Code",by.y="Group.1")
  SummPartN=SummPartN[order(TriGroupe,decreasing=T),]
  
  test=grepl(".x",colnames(SummPartN))
  colnames(SummPartN)=mapply(FUN=function(x,y) if(y){substr(x,1,2)}else{x}
                             ,colnames(SummPartN),test)
  ListNuit=subset(colnames(SummPartN),test)
  ListRef=subset(colnames(SummPartN),grepl(".y",colnames(SummPartN)))
  testHide=match(ListRef,colnames(SummPartN))-1
  #to do: extend colors to other columns to improve readability
  SummHTMLN=datatable(SummPartN, rownames = FALSE,options = list(
    columnDefs = list(list(targets = testHide,visible = FALSE)))) %>%
    formatStyle(columns = c("Code","Groupe","Nom francais","Nom scientifique","Risque d'erreur (%)"),valueColumns="Risque d'erreur (%)", 
                background = styleInterval(c(1, 10, 50), c("white", "khaki", "orange", "orangered"))) %>%
    formatStyle(columns = ListNuit,valueColumns=ListRef, 
                background = styleEqual(c(1,2,3,4), c("palegoldenrod", "greenyellow", "limegreen", "darkgreen")))
  
  saveWidget(SummHTMLN,"output-nightly.html")
#  write.csv2(SummPartN,"output-nightly.tabular",row.names=F)
  write.table(SummPartN,"output-nightly.tabular",row.names=F,sep="\t")
  
  
  #summary by hour
  ActMoyH=dcast(data=IdC2,formula=IdExtrap~Heure
                ,fun.aggregate=length)
  ActMoyHA=aggregate(IdC2$`nom du fichier`
                     ,by=list(IdC2$IdExtrap,IdC2$Heure)
                     ,FUN=length)
  
  test=(as.numeric(colnames(ActMoyH))>16)
  ColDebut=subset(colnames(ActMoyH),test)
  ColFin=subset(colnames(ActMoyH),test==F)
  ListH=c(ColDebut,ColFin)
  neworder=c("IdExtrap",ColDebut,ColFin)
  ActMoyH=ActMoyH[,..neworder]
  
  SummPartH=merge(SummPartshort,ActMoyH,by.x="Code",by.y="IdExtrap")
  SummPartH=SummPartH[order(TriGroupe,decreasing=T),]
  
  
  brks <- quantile(ActMoyHA$x, probs = seq(.05, .95, .05), na.rm = TRUE)-1
  clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
  
  
  SummHTMLH=datatable(SummPartH, rownames = FALSE) %>%
    formatStyle(columns = c("Code","Groupe","Nom francais","Nom scientifique","Risque d'erreur (%)"),valueColumns="Risque d'erreur (%)", 
                background = styleInterval(c(1, 10, 50), c("white", "khaki", "orange", "orangered"))) %>%
    formatStyle(columns=ListH, backgroundColor = styleInterval(brks, clrs))
  
  saveWidget(SummHTMLH,"output-hourly.html")
#  write.csv2(SummPartH,"output-hourly.tabular",row.names=F)
  write.table(SummPartH,"output-hourly.tabular",row.names=F,sep="\t")
  
}

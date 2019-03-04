suppressMessages(library(data.table))
suppressMessages(library(DT))
suppressMessages(library(htmlwidgets))

args <- commandArgs(trailingOnly = TRUE)
EchelleErreur=c("NA","POSSIBLE","PROBABLE","SUR")
EchelleNumErreur=c(99,50,10,1)

   
IdC2=fread(args[1])
refRP=fread(args[2])
GroupList=fread(args[3])


if(substr(IdC2$`nom du fichier`[1],2,2)!="i")
{
  stop("Protocole non conforme, ce script doit etre lance pour un protocole Routier ou Pedestre",call.=FALSE)
}
  
Routier=grepl("-",substr(IdC2$`nom du fichier`[1],4,7))
#compute error risk by species (minimum error among files)
#to be replaced by glm outputs if I'll have time
RisqueErreurT=aggregate(IdC2$IdProb,by=list(IdC2$IdExtrap),FUN=function(x) ceiling((1-max(x-0.0001))*100))
#barplot(RisqueErreurT$x,names.arg=RisqueErreurT$Group.1,las=2)
#compute error risk accoring to observer/validator (a little dirty because it relies on alphabetical order of confidence classes: POSSIBLE < PROBABLE < SUR)
RisqueErreurOV0=match(IdC2$ConfV,EchelleErreur)
RisqueErreurOV=aggregate(RisqueErreurOV0,by=list(IdC2$IdExtrap)
                         ,FUN=max) 
RisqueErreurOV2=EchelleNumErreur[RisqueErreurOV$x]
#compute minimum error risk between man and machine
RisqueErreur=pmin(RisqueErreurT$x,RisqueErreurOV2,na.rm=TRUE)

#compute number of files validated per species
FichValid=aggregate(IdC2$IdV,by=list(IdC2$IdExtrap,IdC2$'nom du fichier')
                                 ,FUN=function(x) sum(!is.na(x))) 
NbValid2=aggregate(FichValid$x,by=list(FichValid$Group.1),FUN=function(x) sum(x>0))

DiffC50=vector() # to store the median of confidence difference between unvalidated records and validated ones
DiffT50=vector() # to store the median of time difference between unvalidated records and validated ones
for (j in 1:nlevels(as.factor(IdC2$IdExtrap)))
{
  IdSp=subset(IdC2
              ,IdC2$IdExtrap==levels(as.factor(IdC2$IdExtrap))[j])
  IdSp$IdProb[is.na(IdSp$IdProb)]=0
  IdSp=IdSp[order(IdSp$IdProb),]
  IdSpV=subset(IdSp,!is.na(IdSp$IdV))
  if(nrow(IdSpV)>0)
  {
  cuts <- c(-Inf, IdSpV$IdProb[-1]-diff(IdSpV$IdProb)/2, Inf)
  CorrC=findInterval(IdSp$IdProb, cuts)
  CorrC2=IdSpV$IdProb[CorrC]
  DiffC=abs(IdSp$IdProb-CorrC2)
  DiffC50=c(DiffC50,median(DiffC))
  
  IdSp=IdSp[order(IdSp$TimeNum),]
  IdSpV=subset(IdSp,!is.na(IdSp$IdV))
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
#cbind(RisqueErreurOV,EffortV,DiffC50,DiffT50)
#barplot(EffortClass-1,names.arg=NbValid2$Group.1,las=2)
ClassEffortV=c("-","FAIBLE","SUFFISANT","SUFFISANT","FORT","FORT")
EffortClassMot=ClassEffortV[EffortClass]


#compare activity / reference frame
FileInfo=as.data.table(tstrsplit(IdC2$`nom du fichier`,"-"))
IdC2$Tron=FileInfo$V4

MicTempsInfo=as.data.table(tstrsplit(as.data.frame(FileInfo)[,(ncol(FileInfo))],"_"))
MicDroit=(as.data.frame(MicTempsInfo)[,(ncol(MicTempsInfo)-2)]=="1")
IdC2$MicDroit=MicDroit

testTempsFin=aggregate(IdC2$temps_fin,by=list(MicDroit),FUN=max)
testTempsFin$Direct=(testTempsFin$x>0.5)
testTF2=sum((testTempsFin$x>0.5))
if(testTF2>1){stop("Probleme stereo : les 2 canaux semblent etre en enregistrement direct")}
IdC2M=merge(IdC2,testTempsFin,by.x="MicDroit",by.y="Group.1")

ActMoy=aggregate(IdC2$`nom du fichier`
                 ,by=list(IdC2M$IdExtrap,IdC2M$Direct),FUN=length)
ListSpref=match(levels(as.factor(ActMoy$Group.1)),refRP$Espece)
Subref=refRP[ListSpref]
if(Routier)
{
  Subref=Subref[,c(1:17)]
}else{
  Subref=Subref[,c(1,18:33)]
}
QualifActE=vector()
QualifActD=vector()

for (k in 1:nlevels(as.factor(ActMoy$Group.1)))
{
  Actsub=subset(ActMoy,ActMoy$Group.1==levels(as.factor(ActMoy$Group.1))[k])
  if(is.na(Subref[k,2]))
  {
    QualifActE=c(QualifActE,NA)
    QualifActD=c(QualifActD,NA)
    }else{
      ActE=subset(Actsub,Actsub$Group.2==F)
      if(nrow(ActE)==0)
      {
        QualifActE=c(QualifActE,NA)
        
      }else{
        cuts=cbind(-Inf,as.numeric(Subref[k,6]),as.numeric(Subref[k,7])
               ,as.numeric(Subref[k,8]),Inf)
        QualifActE=c(QualifActE,findInterval(ActE$x,cuts,left.open=T))
      }
      ActD=subset(Actsub,Actsub$Group.2==T)
      if(nrow(ActD)==0)
      {
        QualifActD=c(QualifActD,NA)
        
      }else{
        cuts=cbind(-Inf,as.numeric(Subref[k,14]),as.numeric(Subref[k,15])
                   ,as.numeric(Subref[k,16]),Inf)
        QualifActD=c(QualifActD,findInterval(ActD$x,cuts,left.open=T))
      }
      
      }
}
ClassAct=c("FAIBLE","MODEREE","FORTE","TRES FORTE")
QualifActMotE=ClassAct[QualifActE]
QualifActMotD=ClassAct[QualifActD]

#compute activity by nights (to be completed)
#ActNuit=aggregate(IdC2M$`nom du fichier`,by=list(IdC2M$DateNuit,IdC2M$IdExtrap),FUN=length)
ActED=dcast(data=ActMoy,formula=Group.1~Group.2,value=x)
ActED[is.na(ActED)]=0
#organize the csv summary
SummPart0=cbind(Esp=levels(as.factor(IdC2M$IdExtrap))
                ,RisqueErreur,NbValid=NbValid2$x,EffortValid=EffortClassMot)

test=match("FALSE",colnames(ActED))
if(is.na(test)==F)
{
  SummPart0=cbind(SummPart0,Contacts_Expansion=ActED$'FALSE'
                  ,Niveau_Activite_Expansion=QualifActMotE)
}else{
  SummPart0=cbind(SummPart0,Contacts_Expansion=""
                  ,Niveau_Activite_Expansion="")
}
test=match("TRUE",colnames(ActED))
if(is.na(test)==F)
{
  
  SummPart0=cbind(SummPart0,Contacts_Direct=ActED$'TRUE'
                ,Niveau_Activite_Direct=QualifActMotD)
}else{
  SummPart0=cbind(SummPart0,Contacts_Direct=""
                  ,Niveau_Activite_Direct="")
}

InfoSp=c("GroupFR","NomFR","Scientific name","Esp")
GroupShort=GroupList[,..InfoSp]
SummPart=merge(GroupShort,SummPart0,by="Esp")
IndexGroupe=c("Autre","Sauterelle","Chauve-souris")
SummPart$IndexSumm=match(SummPart$GroupFR,IndexGroupe)
SummPart=SummPart[with(SummPart
                       ,order(IndexSumm,as.numeric(Contacts_Direct),as.numeric(Contacts_Expansion),decreasing=T)),]
colnames(SummPart)=c("Code","Groupe","Nom francais","Nom scientifique"
                     ,"Risque d'erreur (%)","Nb Validations"
                     ,"Effort de validation","Nb de Contacts en expansion"
                     ,"Niveau d'Activite en expansion"
                     ,"Nb de Contacts en direct"
                     ,"Niveau d'Activite en direct","TriGroupe")

#to do: extend colors to other columns to improve readability
SummHTML=datatable(SummPart, rownames = FALSE) %>%
  formatStyle(columns = c("Code","Groupe","Nom francais","Nom scientifique","Risque d'erreur (%)"),valueColumns="Risque d'erreur (%)", 
              background = styleInterval(c(1, 10, 50), c("white", "khaki", "orange", "orangered"))) %>%
  formatStyle(columns = "Effort de validation", 
              background = styleEqual(c("-","FAIBLE","SUFFISANT","FORT"), c("white", "cyan", "royalblue", "darkblue"))) %>%
  formatStyle(columns = c("Nb de Contacts en expansion","Niveau d'Activite en expansion"),valueColumns="Niveau d'Activite en expansion", 
              background = styleEqual(c("FAIBLE","MODEREE","FORTE","TRES FORTE"), c("palegoldenrod", "greenyellow", "limegreen", "darkgreen"))) %>%
  formatStyle(columns = c("Nb de Contacts en direct","Niveau d'Activite en direct"),valueColumns="Niveau d'Activite en direct", 
              background = styleEqual(c("FAIBLE","MODEREE","FORTE","TRES FORTE"), c("palegoldenrod", "greenyellow", "limegreen", "darkgreen")))


saveWidget(SummHTML,"output-summaryRP.html")
write.table(SummPart,"output-summaryRP.tabular",row.names=F,sep="\t",quote=FALSE)

#summary for each point/transect

#compute number of files validated per species
IdC2M$Canal=sapply(IdC2M$Direct,FUN=function(x) if(x){"Direct"}else{"Expansion"})

ActMoyTA=aggregate(IdC2M$`nom du fichier`,by=list(IdC2M$IdExtrap,IdC2M$Canal,IdC2M$Session),FUN=length)
ActMoyT=dcast(data=IdC2M,formula=IdExtrap+Canal~Session
              ,fun.aggregate=length)
SummPartshort=cbind(SummPart[,c(1:5)],TriGroupe=SummPart[,TriGroupe])
SummPartTron=merge(SummPartshort,ActMoyT,by.x="Code",by.y="IdExtrap")
SummPartTron=SummPartTron[order(TriGroupe,decreasing=T),]

ListSession=levels(as.factor(IdC2M$Session))
brks <- quantile(ActMoyTA$x, probs = seq(.05, .95, .05), na.rm = TRUE)-1
clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
{paste0("rgb(255,", ., ",", ., ")")}


#to do: extend colors to other columns to improve readability
SummHTMLT=datatable(SummPartTron, rownames = FALSE) %>%
  formatStyle(columns = c("Code","Groupe","Nom francais","Nom scientifique","Risque d'erreur (%)"),valueColumns="Risque d'erreur (%)", 
              background = styleInterval(c(1, 10, 50), c("white", "khaki", "orange", "orangered"))) %>%
  formatStyle(columns=ListSession, backgroundColor = styleInterval(brks, clrs))

saveWidget(SummHTMLT,"output-detailRP.html")
write.table(SummPartTron,"output-detailRP.tabular",row.names=F,sep="\t",quote=FALSE)

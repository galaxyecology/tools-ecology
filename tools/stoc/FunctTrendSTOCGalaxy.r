#!/usr/bin/env Rscript


##################################################################################################################################
############## FUNCTION TO CALCULATE AND PLOT EVOLUTION OF SPECIES POPULATION  function:main.glm    ##############################
##################################################################################################################################

#### Based on Romain Lorrillière R script
#### Modified by Alan Amosse and Benjamin Yguel for integrating within Galaxy-E

##### workes with the R version 3.5.1 (2018-07-02)
##### Package used with the version:
#suppressMessages(library(lme4))  version 1.1.18.1
#suppressMessages(library(ggplot2))  version 3.0.0
#suppressMessages(library(speedglm))  version 0.3.2
#suppressMessages(library(arm))  version 1.10.1
#suppressMessages(library(reshape))  version 0.8.8
#suppressMessages(library(data.table))  version 1.12.0
#suppressMessages(library(reshape2))   version 1.4.3



######################################### debut de la fonction makeTableAnalyse / stard of the function makeTableAnalyse
## mise en colonne des especes  et rajout de zero mais sur la base des carrés selectionné sans l'import  /  Species are placed in separated columns and addition of zero on plots where at least one selected species is present 

makeTableAnalyse <- function(data) {
    tab <- reshape(data
                  ,v.names="abond"
                  ,idvar=c("carre","annee")      
                  ,timevar="espece"
                  ,direction="wide")
    tab[is.na(tab)] <- 0               ###### remplace les na par des 0 / replace NAs by 0 

    colnames(tab) <- sub("abond.","",colnames(tab))### remplace le premier pattern "abond." par le second "" / replace the column names "abond." by ""
    return(tab)
}

######################################### fin de la fonction makeTableAnalyse / end of the function makeTableAnalyse





############################################# les fonctions qui filtrent les données pas suffisantes pour analyses fiables / The filtering functions removing species with not enough data to perform accurate analyses

filter_absent_species<-function(tab){
##################### Filtre les espèces jamais présentes (abondance=0) / Filter of species with 0 abundance
#################################################################################  PARTIE POTENTIELLEMENT ISOLABLE ET INSERABLE AVANT LA BOUCLE = permet de gagner du temps sur la boucle car supprime sps pas vu, donc pas repris par la boucle
    
    ## Fait la somme des abondances totales par espèce / calculate the sum of all abundance per species
    if(ncol(tab)==3) {
	tabSum <- sum(tab[,3])## cas d'une seule especes (problème de format et manip un peu differente)  / when selecting only one species, use a different method
	names(tabSum) <- colnames(tab)[3]
    } else {  ## cas de plusieurs espèce/ when selecting more than one species
        tabSum <- colSums(tab[,-(1:2)])
    }
    ## colNull= espece(s) toujours absente /species with 0 total abundance
    colNull <- names(which(tabSum==0))
    ## colconserve= espece(s) au moins presente 1 fois/ species at least with 1 presence
    colConserve <- names(which(tabSum>0))
    ## Affichage des espèces rejetees  / show species eliminated for the analyses
    if(length(colNull)>0){
        cat("\n",length(colNull)," Species removed from the analysis, abundance is always 0.\n\n",sep="")  #Espèces enlevées de l'analyse car abondance toujours égale a 0\n\n",sep="")
        #tabNull <- data.frame(Code_espece = colNull, nom_espece = tabsp[colNull,"nom"])
        cat("Removed absent species : \n-",paste(colNull,collapse="\n-"),sep="")
        tab <- tab[,c("carre","annee",colConserve)]
    }
################################################################################ FIN DE LA PARTIE ISOLABLE
    return(tab)  
}




###################### Filtre les especes trop rare pour avoir des analyses robustes i.e. espèce non presente la 1ère année, avec plus de 3 ans consecutif sans données et moins de 3 ans consécutif avec données 
######################  Filter too rare species for accurate analysis i.e.  species absent the first year, with more than 3 consecutive years with 0 abundance, or with less than 3 consecutive years with presence

###
filter_rare_species<-function(tab){ 
    exclude_threshold <- NULL
    ## calcul et filtre pour chaque (colonne) espece / measure and filter for each species
    for(i in 3:ncol(tab)) {
        ## v =abondance par annee / v= abundance per year
        v <- tapply(tab[,i],tab$annee,sum)  ####################    
        ## v0 =presence(1) abscence(0) per year 
        v0 <- ifelse(v>0,1,0)  ##### 
        tx <- paste(v0,collapse="") #### colle les 0 et 1 / stick the 0 and 1 
        
        p <- unlist(strsplit(tx,"0"))#### Enleve les 0, ce qui séparent les sequences de "1", les sequences de "1" = nbre d'années consécutives avec data / remove 0, splitting sequences of "1" which correspond to consecutve year with data (e.g. 111 = 3 years consecutive years with data)
        p <- p[p!=""] #### ne garde pas les partie sans 1 ou 0 dans les sequences
        ## gsSup0 = plus grande serie temporelle de presence =calcul du nbre de 1 consécutif max / calcul of the biggest temporal series which corresponds to the maximum number of consecutive "1"
        gsSup0 <- max(nchar(p))#### 
        ## gsInf0 plus grande serie temporelle d'absccence ou sans données = enlève les 1 séparant sequence de 0 qui correspondent au nbre d'année consecutive sans données / calcul of the biggest temporal series without data which corresponds to max numbzer fo consecutive "0" 
        gsInf0 <- max(nchar(unlist(strsplit(tx,"1")))) ####  
        ## y0is0 absence la premiere annee
        y0is0 <- v0[1]==0  #### True ou false pour presence de "0"(=pas de données) dans la 1ère année / look if the first year of the time sequence analyzed has no data 
        ## seuil d'exclusion / exclusion threshold  
        exclude_threshold <- c(exclude_threshold,as.vector(ifelse( y0is0 | gsInf0 > 3 | gsSup0 < 3 ,"exclu","bon")))  ############## exclu sps absente la 1ère année, avec plus de 3 ans consécutifs sans données, et avec moins de 3 années consécutives sans données / indicate if the max consecutive year with data and without data, as well as whether the first year of the time sequence analyzed has data 
    }
    names(exclude_threshold) <- colnames(tab)[3:ncol(tab)]

    ## colonnes conservees avec assez de données / Column with enough data
    colConserve <- names(exclude_threshold)[exclude_threshold=="bon"]
    
  
    ## colonnes supprimees / Column that will conserved 
    colSupr <- names(exclude_threshold)[exclude_threshold=="exclu"]
    tabCLEAN <- tab[,c("carre","annee",colConserve)] #### Garde les sps à conserver / select only species with enough data 
    lfiltre <- list(tabCLEAN=tabCLEAN,colConserve=colConserve,colSupr=colSupr)
     
################################################################################# 

    ## colConserve espece conservees / extract species that will be kept to print them
    colConserve <- lfiltre$colConserve
    ## colsupr espece trop rare et donc supprimée de l'analyse / extract species that will be deleted to print them
    colSupr <- lfiltre$colSupr
    ## affichage des especes retirer de l'analyse / print species that will be deleted
    if(length(colSupr)>0){
        cat("\n",length(colSupr)," Rare species removed from the analysis.\n\n",sep="")
        #tabSupr <- subset(tabsp,espece %in% colSupr ,select=c("espece","nom"))
        #tabSupr <- tabSupr[order(tabSupr$espece),]
        cat("Removed rare species : \n-",paste(colSupr,collapse="\n-"),sep="")
        
    }
    if(length(colConserve)==0) {
        mess <- "No species available to calculate abundance variation in this dataset."
        stop(mess)
    }
	
    tabCLEAN <- lfiltre$tabCLEAN

                                        #### MARCHE PAS NE SAIT PAS PQUOI
    tabCLEAN <- melt(tabCLEAN, id.vars=c("carre", "annee"))  #### remet le format de base :le nom d'espèce et abondance dans des colonnes séparées / back to the first format of the file: species name and abundance in separated column
    
    colnames(tabCLEAN)[3:4] <- c("espece","abond")
    tabCLEAN$annee <- as.numeric(as.character(tabCLEAN$annee))
################################################################################ 
    return(tabCLEAN)
}

####################################################################################################################### fin des 2 fonctions de filtre des données / end of the two function to filter the data


























############################################################################################ debut de la Function main.glm / start of the function main.glm

main.glm <- function(id="france",donneesAll=dataCLEAN,assessIC= TRUE,tabsp=tabsp,annees=annees,figure=TRUE,description=TRUE,tendanceSurFigure=TRUE, ###### declaration des arguments  listSp=sp était avant declaré avant la fonction mais il me semble que ca marche aussi comme cela
                     seuilOccu=14,seuilAbond=NA) {

    

    filesaveAn <-  paste("Output/",id,"/variationsAnnuellesEspece_",id,".tabular",  ##### Nom du dossier ET fichier de sortie des resultats par année / name of the output file with results for each years
                         sep = "")
    filesaveTrend <-  paste("Output/",id,"/tendanceGlobalEspece_",id,".tabular",   ##### Nom du dossier ET fichier de sortie des resultats pour la période "annee" complete / name of the output file with the results for the period
                            sep = "")
    fileSaveGLMs <-  paste("Output/",id,"/listGLM_",id,sep = "")  #####  Nom du dossier ET fichier de sortie des modèles lineaire generalisés / name of the output file of the generlized linear models


    
     
    seuilSignif <- 0.05  ## seuil de significativite / significancy threshold
    
    
   rownames(tabsp) <- tabsp$espece  ## change nom des lignes de tabsp (table de reference des especes) 
    
    
    ##vpan vecteur des panels de la figure  ###### POUR FAIRE LES GRAPHIQUES
    vpan <- c("Variation abondance")
    if(description) vpan <- c(vpan,"Occurrences","Abondances brutes")
                                        

    ## specifications des variables temporelles necesaires pour les analyses / specification of temporal variable necessary for the analyses
    annee <- sort(unique(donneesAll$annee))
    nbans <- length(annee)
    pasdetemps <- nbans-1
    firstY <- min(annee)
    lastY <- max(annee)
	
	
	
	

    ## Ordre de traitement des especes ### order of species to be analyzed
    spOrdre <- aggregate(abond~espece,data=donneesAll,sum)  #### calcul les sommes des abondances pour ordonner / calculate the sum for the ordination
    spOrdre <- merge(spOrdre,tabsp,by="espece") #### rajoute la colonne avec les abondances totales par espece / add a new column with the sum
    
    spOrdre <- spOrdre[order(as.numeric(spOrdre$indicateur),spOrdre$abond,decreasing = TRUE),] #### mets les especes plus abondantes en premiers (plus long pour faire tourner le modèle) / order the species by abundance, the most abundant species being the less fast analysis
    
    
    listSp <- spOrdre$espece
    i <- 0
    nbSp <- length(listSp)
                                        #	browser()
    ## analyse par espece
### browser()
    ## affichage des especes conservees pour l'analyse  ### PAS SUR QUE CE SOIT ENCORE UTILE
    cat("\n",nbSp," Espèces conservées pour l'analyse\n\n",sep="")
    rownames(tabsp) <- tabsp$espece
    print(tabsp[,1:2])
    #tabCons <- data.frame(Code_espece = listSp, nom_espece = tabsp[as.character(listSp),"nom"])
    #print(tabCons)  
    cat("\n\n",sep="")
    flush.console()


    ## initialisation de la liste de sauvegarde


##browser()
    
    for (sp in listSp) {  ######## Boucle pour analyse par espèce / loop for the analysis by species


        i <- i + 1
          
        d <- subset(donneesAll,espece==sp)  ## d data pour l'espece en court  / cut the data keeping only the i species
        
        #nomSp <- as.character(tabsp[sp,"nom"])  ## info sp
        nomSp <- tabsp$nom[which(tabsp$espece==sp)]  ## info sp
        cat("\n(",i,"/",nbSp,") ",sp," | ", nomSp,"\n",sep="")
        flush.console()

        #indic <- tabsp[sp,"indicateur"] ## indic :espece utilisee pour le calcul des indicateurs par groupe de specialisation / list the species used as species indicators by trophic specialization
        indic <- tabsp$indicateur[which(tabsp$espece==sp)] ## indic :espece utilisee pour le calcul des indicateurs par groupe de specialisation / list the species used as species indicators by trophic specialization
        nb_carre = tapply(rep(1,nrow(d)),d$annee,sum) ## nb_carre nombre de carre suivie par annee / number of plots per year
        
        nb_carre_presence = tapply(ifelse(d$abond>0,1,0),d$annee,sum) ## nb_carre_presence nombre de carre de presence par annee / number the plots where the species were observed
        
        tab2 <- data.frame(annee=rep(annee,2),val=c(nb_carre,nb_carre_presence),LL = NA,UL=NA, ## tab2 table de resultat d'analyse / data.frame of the analyses results
                           catPoint=NA,pval=NA,
                           courbe=rep(c("carre","presence"),each=length(annee)),panel=vpan[2])
        tab2$catPoint <- ifelse(tab2$val == 0,"0",ifelse(tab2$val < seuilOccu,"infSeuil",NA))
        
        abond <- tapply(d$abond,d$annee,sum) ## abond abondance par annee / abundance per year
        
        tab3 <- data.frame(annee=annee,val=abond,LL = NA,UL=NA,catPoint=NA,pval=NA,courbe=vpan[3],panel=vpan[3]) ## table pour la figure / data.frame made to realize the graphical outputs
        tab3$catPoint <- ifelse(tab3$val == 0,"0",ifelse(tab3$val < seuilAbond,"infSeuil",NA))

        ## GLM pour calcul des tendances annuelles de l'evolution des populations / GLM to measure annual tendency of population evolution 
       formule <- as.formula("abond~as.factor(carre)+as.factor(annee)") #### specification du modèle = log lineaire / specifying the model = log linear
       if(assessIC) {##### OPTION A RENTRER AU DEBUT PEUT ËTRE A METTRE DANS LES ARGUMENTS SI LAISSE LE CHOIX SINON L ARG PAR DEFAUT LORS DE LA DECLARATION DE LA FONCTION
           glm1 <- glm(formule,data=d,family=quasipoisson)  ##### fit model lineaire general avec intervalle de confiance disponible / fit linear and generalized model with confidence intervalle available
       } else {
           glm1 <- try(speedglm(formule,data=d,family=quasipoisson())) ##### fit modele lineaire et generaux pour les gros jeux de données / fit of linear and generalized model for large-medium dataset
           if(class(glm1)[1]=="try-error")
               glm1 <- glm(formule,data=d,family=quasipoisson) ##### comprends pas mais je pense que c'est speedglm qui marche pas avec toutes les données
       }
       sglm1 <- summary(glm1)  #### sortie du modele / output of the model
       sglm1 <- coefficients(sglm1) ### coefficient regression de chaque variable avec les résultats des tests statistiques / regression coefficient of each predictive variables with results of the statistical tests
       sglm1 <- tail(sglm1,pasdetemps) #### recupére les derniers elements du modèle avec la taille de l'objet "pasdetemps" car le nombre de coef = nbre d'année et pas les coefficient de regression de la variable carre / retrieve only the coefficient regression of the variable year
       coefan <- as.numeric(as.character(sglm1[,1]))#### coefficient de regression de la variable année (1 pour chaque année)
        
        coefannee <- c(1,exp(coefan))## coefannee vecteur des variation d'abondance par annee avec transformation inverse du log :exp() / regression coefficient of the year back transformed from log(abundance) : exp()
        
		erreuran <- as.numeric(as.character(sglm1[,2])) #### erreur standard sur le coefficient de regression de la variable annee  / standard error on the regression coefficient of the year 
        erreurannee1 <- c(0,erreuran*exp(coefan))## erreur standard par année / the standard error per year  ###### LA J AI UN DOUTE NORMALEMENT INTERVAL DE CONF C CI_lower <- coefficients(lin_mod)[2] - 1.96*summary(lin_mod)$coefficients[2,2]
                                                                                                               ####CI_upper <- coefficients(lin_mod)[2] + 1.96*summary(lin_mod)$coefficients[2,2]
		
        pval <- c(1,as.numeric(as.character(sglm1[,4])))###### p value
        
        ## calcul des intervalle de confiance avec methode de bootstrap pour simuler des coef de regress sur lequel intervalle de conf sont mesurés/ calcul of the confidence interval using bootstrap method to simulate set regression coefficients and s.e.with uncertainty   POURQUOI PAS UTILISE confint.glm() ou boot() ou ci.boot()
        
        if(assessIC) {
        glm1.sim <- sim(glm1)
        ic_inf_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.025),pasdetemps)))
        ic_sup_sim <- c(1,exp(tail(apply(coef(glm1.sim), 2, quantile,.975),pasdetemps)))
        } else {
            ic_inf_sim <- NA
            ic_sup_sim <- NA
 
        }
        
        
        
        tab1 <- data.frame(annee,val=coefannee,  ## tab1 table pour la realisation des figures / table for the graphical outputs  ### 2EME POUR GRAPH ici ce sont le coef de regress annee en fonction des annéés alors que tab3 c'est les abondance en fct des années et tab2 nombre de carré total et avec presence
                           LL=ic_inf_sim,UL=ic_sup_sim,
                           catPoint=ifelse(pval<seuilSignif,"significatif",NA),pval,
                           courbe=vpan[1],
                           panel=vpan[1])
        ## netoyage des intervalle de confiance mal estimés et qd donnees pas suffisantes pour calcul d'IC /cleaning of wrong or biaised measures of the confidence interval
        if(assessIC) {
        tab1$UL <- ifelse( nb_carre_presence==0,NA,tab1$UL)
        tab1$UL <-  ifelse(tab1$UL == Inf, NA,tab1$UL)
        tab1$UL <-  ifelse(tab1$UL > 1.000000e+20, NA,tab1$UL)
        tab1$UL[1] <- 1
        tab1$val <-  ifelse(tab1$val > 1.000000e+20,1.000000e+20,tab1$val)
        }
        ## indice de surdispersion  / overdispersion index
       ## browser()
        if(assessIC) dispAn <- glm1$deviance/glm1$null.deviance else dispAn <- glm1$deviance/glm1$nulldev


        ## tabAn table de sauvegarde des resultats par année / table of the results per year ######  reprends bcp de tabl DIFFERENCE AVEC tab2  c les abondances relatives, alors que nb de carre, nb de carre presnce, p val sont aussi ds tab2
        tabAn <- data.frame(id,code_espece=sp, nom_espece = nomSp,indicateur = indic,annee = tab1$annee,
                            abondance_relative=round(tab1$val,3),
                            IC_inferieur = round(tab1$LL,3), IC_superieur = round(tab1$UL,3),
                            erreur_standard = round(erreurannee1,4),
                            p_value = round(tab1$pval,3),significatif = !is.na(tab1$catPoint),
                            nb_carre,nb_carre_presence,abondance=abond)
        
        ## GLM pour calcul des tendance generale sur la periode avec modele log lineaire / GLM to measure the tendency of population evolution on the studied period with log linear model
        formule <- as.formula(paste("abond~ as.factor(carre) + annee",sep="")) ### 
          #  browser()
    
       
         if(assessIC) {
             md2 <- glm(formule,data=d,family=quasipoisson) }
        else {
                md2 <- try(speedglm(formule,data=d,family=quasipoisson()),silent=TRUE)

                if(class(md2)[1]=="try-error")
                    md2 <- glm(formule,data=d,family=quasipoisson)
            }

        
       smd2 <- summary(md2)       #### sortie du modele / output of the model
       smd2 <- coefficients(smd2) ### coefficient regression de chaque variable avec les résultats des tests statistiques / regression coefficient of each predictive variables with results of the statistical tests
       smd2 <- tail(smd2,1)       ### coefficient regression de variable annee avec les résultats des tests statistiques / regression coefficient of the variable year with results of the statistical tests
       
        
        coefan <- as.numeric(as.character(smd2[,1])) ## tendences sur la periode = coefficient regression de variable annee  / tendency of population evolution on the studied period = regression coefficient of the variable year 
        trend <- round(exp(coefan),3)
        
        pourcentage <- round((exp(coefan*pasdetemps)-1)*100,2) ## pourcentage de variation sur la periode / percentage of population variation on the studied period 
        pval <- as.numeric(as.character(smd2[,4]))
        
        erreuran <- as.numeric(as.character(smd2[,2])) #### récuperer l'erreur standard / retrieve the error 
        ## erreur standard 
        erreurannee2 <- erreuran*exp(coefan)
        
        
        ## calcul des intervalle de confiance avec methode de bootstrap pour simuler des coef de regress sur lequel intervalle de conf sont mesurés/ calculating the confidence interval based on bootstrap method to simulate set regression coefficients and s.e.with uncertainty 
        LL <- NA
        UL <- NA
        if(assessIC) {
            md2.sim <- sim(md2)
            LL <- round(exp(tail(apply(coef(md2.sim), 2, quantile,.025),1)),3)
            UL <- round(exp(tail(apply(coef(md2.sim), 2, quantile,.975),1)),3)
        } else {
            LL <- NA
            UL <- NA
        }
        
        ## tab1t table utile pour la realisation des figures  / table used for the figures
        tab1t <- data.frame(Est=trend,
                            LL , UL,
                            pourcent=pourcentage,signif=pval<seuilSignif,pval)
        
        
        trendsignif <- tab1t$signif
        pourcent <- round((exp(coefan*pasdetemps)-1)*100,3)
        ## mesure de la surdispersion / overdispersion measurment

          if(assessIC) dispTrend <- md2$deviance/md2$null.deviance else dispTrend <- md2$deviance/md2$nulldev


        
        ## classement en categorie incertain /classifying wrong or not reliable results 
       # browser()
        if(assessIC) {
        if(dispTrend > 2 | dispAn > 2 | median( nb_carre_presence)<seuilOccu) catIncert <- "Incertain" else catIncert <-"bon"  ##### en fonction de l'indice de surdispersion et presence < à seuil occurence / based on the overdispersion index and the presence on a minimum number of plots
        vecLib <-  NULL
        if(dispTrend > 2 | dispAn > 2 | median( nb_carre_presence)<seuilOccu) {
            if(median( nb_carre_presence)<seuilOccu) {
                vecLib <- c(vecLib,"espece trop rare")
            }
            if(dispTrend > 2 | dispAn > 2) {
                vecLib <- c(vecLib,"deviance")
            }
        }
        raisonIncert <-  paste(vecLib,collapse=" et ")
        } else {
            catIncert <- NA
            raisonIncert <- NA
        }
        
        
        
        ## affectation des tendence EBCC  / retrieve the trend of population evolution on the studied period
        catEBCC <- NA
        if(assessIC)  catEBCC <- affectCatEBCC(trend = as.vector(trend),pVal = pval,ICinf=as.vector(LL),ICsup=as.vector(UL)) else catEBCC <- NA
        ## table complete de resultats  pour la periode etudiée / complete table with results for the studied period
     #   browser()
        tabTrend <- data.frame(
            id,code_espece=sp,nom_espece = nomSp,indicateur = indic,
            nombre_annees = pasdetemps,premiere_annee = firstY,derniere_annee = lastY,
            tendance = as.vector(trend) ,  IC_inferieur=as.vector(LL) , IC_superieur = as.vector(UL),pourcentage_variation=as.vector(pourcent),
            erreur_standard = as.vector(round(erreurannee2,4)), p_value = round(pval,3),
            significatif = trendsignif,categorie_tendance_EBCC=catEBCC,mediane_occurrence=median( nb_carre_presence) ,
            valide = catIncert,raison_incertitude = raisonIncert)


        if(assessIC)  listGLMsp <- list(list(glm1,glm1.sim,md2,md2.sim)) else  listGLMsp <- list(list(glm1,md2))
        names(listGLMsp)[[1]] <-sp 
        fileSaveGLMsp <- paste(fileSaveGLMs,"_",sp,".Rdata",sep="")
        
        save(listGLMsp,file=fileSaveGLMsp)
        cat("--->",fileSaveGLMsp,"\n")
        flush.console()

        if(sp==listSp[1]) {
            glmAn <- tabAn
            glmTrend <- tabTrend
        } else  {
            glmAn <- rbind(glmAn,tabAn)
            glmTrend <- rbind(glmTrend,tabTrend)
        }
	## les figures     
        if(figure) {
            ## table complete pour la figure en panel par ggplot2
            ## table pour graphe en panel par ggplot2
            if(description)	dgg <- rbind(tab1,tab2,tab3) else dgg <- tab1
            ## les figures     
            
            ggplot.espece(dgg,tab1t,id,serie=NULL,sp,valide=catIncert,nomSp,description,tendanceSurFigure,seuilOccu=14,vpan = vpan,assessIC=assessIC)
            
        }
        
        
        
        
    }
    
    write.table(glmAn,filesaveAn,row.names=FALSE,quote=FALSE,sep="\t",dec=".",fileEncoding="UTF-8")
    cat("--->",filesaveAn,"\n")
    write.table(glmTrend,filesaveTrend,row.names=FALSE,quote=FALSE,sep="\t",dec=".",fileEncoding="UTF-8")
    cat("--->",filesaveTrend,"\n")
    
    
    flush.console()
    
    
    
}
########################################################################################################## Fin de la fonction main.glm / end of the function main.glm















###########################################################################################################  fonction appelée par main.glm renvoyant la categorie European Bird Census Council en fonction des resultats des modèles  / function called by main.glm to classify results depending on the quality of the data and analyses
## renvoie la categorie EBCC de la tendance en fonction
## trend l'estimateur de la tendance / estimation of the trends
## pVal la p value
## ICinf ICsup l intervalle de confiance a 95 pourcent
affectCatEBCC <- function(trend,pVal,ICinf,ICsup){
  catEBCC <- ifelse(pVal>0.05,
                    ifelse(ICinf < 0.95 | ICsup > 1.05,"Incertain","Stable"),
                    ifelse(trend<1,
                           ifelse(ICsup<0.95,"Fort declin","Declin moderee"),
                           ifelse(ICinf>1.05,"Forte augmentation","Augmentation modere")))
  return(catEBCC)
}

############################################################################################################ fin de la fonction renvoyant la categorie EBCC / end of the function main.glm








############################################################################################################ fonction graphique appelée par main.glm / function called by main.glm for graphical output
ggplot.espece <- function(dgg,tab1t,id,serie=NULL,sp,valide,nomSp=NULL,description=TRUE,
                          tendanceSurFigure=TRUE,seuilOccu=14, vpan,assessIC=TRUE) {
  
  #  serie=NULL;nomSp=NULL;description=TRUE;valide=catIncert
  #  tendanceSurFigure=TRUE;seuilOccu=14
  require(ggplot2)
  
  figname<- paste("Output/",id,"/",ifelse(valide=="Incertain","Incertain/",""),
                  sp,"_",id,serie, ".png",
                  sep = "")
  ## coordonnee des ligne horizontal de seuil pour les abondances et les occurences
  hline.data1 <- data.frame(z = c(1), panel = c(vpan[1]),couleur = "variation abondance",type="variation abondance")
  hline.data2 <- data.frame(z = c(0,seuilOccu), panel = c(vpan[2],vpan[2]),couleur = "seuil",type="seuil")
  hline.data3 <- data.frame(z = 0, panel = vpan[3] ,couleur = "seuil",type="seuil")  
  hline.data <- rbind(hline.data1,hline.data2,hline.data3)
  titre <- paste(nomSp)#,"\n",min(annee)," - ",max(annee),sep="")
  
  ## texte de la tendance / text for the population evolution trend
  tab1 <- subset(dgg,panel =="Variation abondance")
  pasdetemps <- max(dgg$annee) - min(dgg$annee) + 1
  if(assessIC){
      txtPente1 <- paste(tab1t$Est,
                     ifelse(tab1t$signif," *",""),"  [",tab1t$LL," , ",tab1t$UL,"]",
                     ifelse(tab1t$signif,paste("\n",ifelse(tab1t$pourcent>0,"+ ","- "),
                                               abs(tab1t$pourcent)," % en ",pasdetemps," ans",sep=""),""),sep="")
  }else{
       txtPente1 <- ifelse(tab1t$signif,paste("\n",ifelse(tab1t$pourcent>0,"+ ","- "),
                                               abs(tab1t$pourcent)," % en ",pasdetemps," ans",sep=""),"")
 
  }
  ## table du texte de la tendance / table of the text for the population evolution trend
  tabTextPent <- data.frame(y=c(max(c(tab1$val,tab1$UL),na.rm=TRUE)*.9),
                            x=median(tab1$annee),
                            txt=ifelse(tendanceSurFigure,c(txtPente1),""),
                            courbe=c(vpan[1]),panel=c(vpan[1]))
  ## les couleurs / the colors
  vecColPoint <- c("#ffffff","#eeb40f","#ee0f59")
  names(vecColPoint) <- c("significatif","infSeuil","0")
  vecColCourbe <- c("#3c47e0","#5b754d","#55bb1d","#973ce0")
  names(vecColCourbe) <- c(vpan[1],"carre","presence",vpan[3])
  vecColHline <- c("#ffffff","#e76060")
  names(vecColHline) <- c("variation abondance","seuil")
  
  col <- c(vecColPoint,vecColCourbe,vecColHline)
  names(col) <- c(names(vecColPoint),names(vecColCourbe),names(vecColHline))
  
  ## si description graphique en 3 panels
  if(description) {
    p <- ggplot(data = dgg, mapping = aes(x = annee, y = val))
    ## Titre, axes ...
    p <- p + facet_grid(panel ~ ., scale = "free") +
      theme(legend.position="none",
            panel.grid.minor=element_blank(),
            panel.grid.major.y=element_blank())  +
      ylab("") + xlab("Annee")+ ggtitle(titre) +
      scale_colour_manual(values=col, name = "" ,
                          breaks = names(col))+
      scale_x_continuous(breaks=min(dgg$annee):max(dgg$annee))
    p <- p + geom_hline(data =hline.data,mapping = aes(yintercept=z, colour = couleur,linetype=type ),
                        alpha=1,size=1.2)
   if(assessIC){ ############# ONLY FOR THE CONFIDENCE INTERVAL
    p <- p + geom_ribbon(mapping=aes(ymin=LL,ymax=UL),fill=col[vpan[1]],alpha=.2) 
    p <- p + geom_pointrange(mapping= aes(y=val,ymin=LL,ymax=UL),fill=col[vpan[1]],alpha=.2)
	}
    p <- p + geom_line(mapping=aes(colour=courbe),size = 1.5)
    p <- p + geom_point(mapping=aes(colour=courbe),size = 3)
    p <- p + geom_point(mapping=aes(colour=catPoint,alpha=ifelse(!is.na(catPoint),1,0)),size = 2)
    p <-  p + geom_text(data=tabTextPent, mapping=aes(x,y,label=txt),parse=FALSE,color=col[vpan[1]],fontface=2, size=4)
    ggsave(figname, p,width=16,height=21, units="cm")
	print (figname)  ##### CAN BE REMOVED IF YOU DO NOT WANT THE GRAPH TO BE PLOTTED
  } else {
    
    p <- ggplot(data = subset(dgg,panel=="Variation abondance"), mapping = aes(x = annee, y = val))
    ## Titre, axes ...
    p <- p + facet_grid(panel ~ ., scale = "free") +
      theme(legend.position="none",
            panel.grid.minor=element_blank(),
            panel.grid.major.y=element_blank())  +
      ylab("") + xlab("Annee")+ ggtitle(titre) +
      scale_colour_manual(values=col, name = "" ,
                          breaks = names(col))+
      scale_x_continuous(breaks=min(dgg$annee):max(dgg$annee))
    p <- p + geom_hline(data =subset(hline.data,panel=="Variation abondance"),mapping = aes(yintercept=z, colour = couleur,linetype=type ),
                        alpha=1,size=1.2)
    
   if(assessIC){ ############# ONLY FOR THE CONFIDENCE INTERVAL
    p <- p + geom_ribbon(mapping=aes(ymin=LL,ymax=UL),fill=col[vpan[1]],alpha=.2) 
    p <- p + geom_pointrange(mapping= aes(y=val,ymin=LL,ymax=UL),fill=col[vpan[1]],alpha=.2)
	}
    p <- p + geom_line(mapping=aes(colour=courbe),size = 1.5)
    p <- p + geom_point(mapping=aes(colour=courbe),size = 3)
    p <- p + geom_point(mapping=aes(colour=catPoint,alpha=ifelse(!is.na(catPoint),1,0)),size = 2)
    p <-  p + geom_text(data=tabTextPent, mapping=aes(x,y,label=txt),parse=FALSE,color=col[vpan[1]],fontface=2, size=4)
    ggsave(figname, p,width=15,height=9,units="cm")
  print (figname) ##### CAN BE REMOVED IF YOU DO NOT WANT THE GRAPH TO BE PLOTTED
  }
}
############################################################################################################ fin fonction graphique / end of function for graphical output




#################################################################################################################### debut de la fonction de moyenne geometrique pondere / start of the geometric weighted mean function 
geometriqueWeighted <- function(x,w=1) exp(sum(w*log(x))/sum(w))
#################################################################################################################### fin de la fonction de moyenne geometrique pondere / end of the geometric weighted mean function 



##################################################################################################################### debut de la fonction analyseGroupe / start of the function analyseGroupe
## Analyse par groupe de specialisation a partir des resulats de variation d'abondance par especes / analysis by specialization group based on results of the analysis of population evolution trend
#


analyseGroupe <- function(id="france",tabsp=tabsp,donnees=donnees,donneesTrend=donneesTrend,ICfigureGroupeSp=TRUE,powerWeight=2,
                          correctionAbondanceNull = 0.000001,
                          groupeNom = c("generaliste","milieux batis","milieux forestiers","milieux agricoles"),
                          groupeCouleur = c("black","firebrick3","chartreuse4","orange")) {
    
    



    ## donnees tendances globales / results of the global trends
    donneesTrend <- subset(donneesTrend, select = c(code_espece,valide,mediane_occurrence))
	
    ## table de reference espece  / reference table for species
    tabsp <- subset(tabsp, select= c(sp,nom,indicateur, specialisation))
    donnees <- merge(donnees,donneesTrend,by="code_espece")
    donnees <- merge(donnees,tabsp,by.x="code_espece",by.y="sp")
    ## table de correspondance de biais en fonction des medianes des occuerences
	
    
    nameFileSpe <-  paste("Output/",id,"/variationsAnnuellesGroupes_",id, ############# Declare le fichier de sortie des variations annuelles par groupe / declare the name of the outputfile for annual population evolution trend by group 
                          ".tabular",sep="" )
    nameFileSpepng <-  paste("Output/",id,"/variationsAnnuellesGroupes_",id, ############# Declare le fichier de sortie graphique des variations annuelles par groupe / declare the name of the graphical output file for annual population evolution trend by group
                             ".png",sep="" )
    
    grpe <- donnees$specialisation
    
    ####### valeur seuil sont obtenues à partir de simulations / threshold values are obtained from simulations
    ff <- function(x,y) max(which(y<=x)) ## fonction pour recherche le poid associé à valeur max parmi valeur seuil d'occurence inferieur ou egale à occurence mediane obs / function to retrieve the weight associated with the max occurence threshold equal or smaller than the occurence mediane observed
     
    IncertW <- ifelse(donnees$valide=="Incertain",tBiais$biais[sapply(as.vector(donnees$mediane_occurrence),ff,y=tBiais$occurrenceMed)],1) ## pr verifier poids de l'espèce dans analyse, récupére seuil occurence minimum pour lequel tendance pas bonne, et compare avec mediane occurence des données  / to check the weight of species in the analysis, this retrieve occurence threshold with wich real occurence measured on data are compared in order to verify the accuracy of the trend measurment
    ## poids du a la qualite de l'estimation
                                        #   erreur_stW <- 1/((donnees$erreur_st+1)^powerWeight)
                                        #	erreur_stW <- ifelse( is.na(donnees$IC_superieur),0,erreur_stW)
    erreur_stW <- ifelse(is.na(donnees$IC_superieur),0,1)#####  si pas d'interval de confiance met 0 et donne un poid de 0 à l'esps (voir ci dessous) /  if no confidence interval calculated give a weight of 0 for the sps 
    ## calcul du poids total de chaque espèce / calcul of the weight of each species 
    W <- IncertW * erreur_stW
    
    ## variable de regroupement pour les calculs par groupe de specialisation et par an / variables gathered to identify group for the calculation (per specialization and per year)
    grAn <- paste(donnees$specialisation,donnees$annee,sep="_")
    ## data frame pour le calcul / dataframe made for the calcul
    dd <- data.frame(grAn,annee = donnees$annee, grpe,W,ab=donnees$abondance_relative,ICinf= donnees$IC_inferieur, ICsup= ifelse(is.na(donnees$IC_superieur),10000,donnees$IC_superieur)) 
    ## table resumer de tous les poids / table to sum up the weights of each species depending on the incertainty in the calcul of the poulation evolution trends
    ddd <- data.frame(code_espece = donnees$code_espece,nom_espece = donnees$nom_espece,annee = donnees$annee, 
                      groupe_indicateur = grpe,
                      poids_erreur_standard = round(erreur_stW,3), poids_incertitude = round(IncertW,3),poids_final = round(W,3),
                      abondance_relative=donnees$abondance_relative,
                      IC_inferieur= donnees$IC_inferieur, 
                      IC_superieur= ifelse(is.na(donnees$IC_superieur),10000,donnees$IC_superieur),
                      valide = donnees$valide, mediane_occurrence = donnees$mediane_occurrence) 

    nomFileResum <- paste("Output/",id,"/donneesGroupes_",id, ###### declaration du nom du repertoire et des fichiers de sortie / declaring the name of the output folder and files  
                          ".tabular",sep="" )
    write.table(ddd,nomFileResum,row.names=FALSE,sep="\t",dec=".",fileEncoding="UTF-8")
    cat("-->",nomFileResum,"\n")
    
    ## calcul des moyennes ponderees par groupe par an et pour les abondance et les IC	/ calcul of weighted means per specialization group and per year for the abundance and confidence interval
    for(j in 5:7) dd[,j] <- ifelse(dd[,j]==0,correctionAbondanceNull,dd[,j])	
    ag <- apply(dd[,5:7], 2,  ######## sur les abondances relatives, les ICinf et ICsup
                function(x) {
                    sapply(split(data.frame(dd[,1:4], x), dd$grAn),  ###### fait les moyennes pondérés par groupe grAn / calculate the weighted mean by group grAn
                           function(y) round(geometriqueWeighted(y[,5], w = y$W),3))
                })
    ##	gg <- subset(dd,as.character(dd$grAn)=="milieux forestier_2014")  #############################################################

    ag <- ifelse(is.na(ag),1,ag)
    ag <- as.data.frame(ag)
    ag$grAn <-  rownames(ag)
    dbon <- subset(donnees,valide=="bon")
    dIncert <- subset(donnees,valide=="Incertain")
    ## calcul nombre d'espece "bonne" pour le calcul / calculating the number of species with low level of incertainty, "good" species 
    bon <- tapply(dbon$nom,dbon$specialisation,FUN=function(X)length(unique(X)) )
    bon <- ifelse(is.na(bon),0,bon)
    tbon <- data.frame(groupe=names(bon),bon)
    ## calcul nombre d'especes "incertaines" pour le calcul / calculating the number of species with high level of incertainty, "bad" species
    Incert <- tapply(dIncert$nom,dIncert$specialisation,FUN=function(X)length(unique(X)) )
    Incert <- ifelse(is.na(Incert),0,Incert)
    tIncert <- data.frame(groupe=names(Incert),Incertain=Incert)

    tIncert <- merge(tIncert,tbon,by="groupe")
    
    ## table de données avec les moyennes ponderees par groupe / table of the data with the weighted mean by group 
    da <- merge(unique(dd[,1:3]),ag,by="grAn")[,-1]
    colnames(da) <- c("annee","groupe","abondance_relative","IC_inferieur","IC_superieur")

    da$annee <- as.numeric(da$annee)
    da <-  merge(da,tIncert,by="groupe") #### ajoute le nombre d'espece "incertaines" et "bonne" aux resultats  / add the number of "good" and "bad" species to the overall resutls
    da <- subset(da, groupe != "non")
    colnames(da)[6:7] <-  c("nombre_especes_incertaines","nombre_espece_bonnes")
    a <- data.frame(id,da)
    write.table(da,file=nameFileSpe,row.names=FALSE,quote=FALSE,sep="\t",dec=".",fileEncoding="UTF-8")

    cat("-->",nameFileSpe,"\n")
    yearsrange <- c(min(da$annee),max(da$annee))
    
    ## figure par ggplot2  / plots with ggplot2
    titre <- paste("Variation de l'indicateur groupe de specialisation",sep="")

    vecCouleur <- setNames(groupeCouleur,groupeNom)
                                        #browser()
    p <- ggplot(data = da, mapping = aes(x = annee, y = abondance_relative, colour=groupe,fill=groupe))
    p <- p + geom_hline(aes(yintercept = 1), colour="white", alpha=1,size=1.2) 
    if(ICfigureGroupeSp)
        p <- p + geom_ribbon(mapping=aes(ymin=IC_inferieur,ymax=IC_superieur),linetype=2,alpha=.1,size=0.1) 
    p <- p + geom_line(size=1.5)
    p <- p +  ylab("") + xlab("Annee")+ ggtitle(titre) 
    if(!is.null(groupeNom)) p <- p + scale_colour_manual(values=vecCouleur, name = "" )+
                                scale_x_continuous(breaks=unique(da$annee))
    if(!is.null(groupeNom)) p <- p +  scale_fill_manual(values=vecCouleur, name="")
    p <- p +  theme(panel.grid.minor=element_blank(), panel.grid.major.y=element_blank()) 
    ggsave(nameFileSpepng, p,width=17,height=10,units="cm")

                                        #   cat(" <==",nameFileSpepng,"\n")
    
    ## calul pour chaque groupe une pente de regression d'evolution des abondances sur la periode étudiée / calculating for each group the regression slope for the abundance evolution on the studied period
    vecSpe <- unique(da$groupe)
    datasum <- data.frame(groupe=NULL,tendance=NULL,pourcentage_variation=NULL)
    for(spe in 1:4){
        # print(spe)
        subtab <- subset(da,groupe==vecSpe[spe])
        if(nrow(subtab)>1) {
            sumlm <- summary(lm(abondance_relative~annee,data=subtab)) ##### recupère les resultats du modèle linéaire / retrieve the results of the linear model
            subdatasum <- data.frame(groupe=vecSpe[spe],
                                     tendance=round(sumlm$coefficients[2,1],3),
                                     pourcentage_variation=round(sumlm$coefficients[2,1]*(nrow(subtab)-1)*100,3)) #### assemble les resultats pour en faire une sortie  /  bring together the results for an output file
            datasum <- rbind(datasum,subdatasum)
            
        }
        
    }
    datasum <- merge(datasum,tIncert,by="groupe") #### 
    datasum <- data.frame(id,datasum)
                                        #datasum$cat_tendance_EBCC <- affectCatEBCC(trend,pVal,ICinf,ICsup
    namefilesum <- paste("Output/",id,"/tendancesGlobalesGroupes_",id,
                         ".tabular",sep="" )
    write.table(datasum,file=namefilesum,row.names=FALSE,quote=FALSE,sep="\t",dec=".",fileEncoding="UTF-8")
    cat("-->",namefilesum,"\n")
}

################################################################################################################## fin de la fonction analyseGroupe / end of the function analyseGroupe







################################################################################################################### debut de la fonction check_file / start of the function check_file
# Fonction pour verifier les données d'entrée / General function to check integrity of input file. Will check numbers and contents of variables(colnames). 
#return an error message and exit if mismatch detected
#Faut rentrer le nom du jeu de données, le nbre et le nom des variables / Enter dataset name,  expected number and names of variables. + an exit error message to guide user.

check_file<-function(dataset,err_msg,vars,nb_vars){
    if(ncol(dataset)!=nb_vars){ #Verifiction de la présence du bon nb de colonnes, si c'est pas le cas= message d'erreur / checking for right number of columns in the file if not = error message
        cat("\nerr nb var\n") 
        stop(err_msg, call.=FALSE)
    }

    for(i in vars){
        if(!(i %in% names(dataset))){
            stop(err_msg,call.=FALSE)
        }
    }
}

#####################################################################################################################


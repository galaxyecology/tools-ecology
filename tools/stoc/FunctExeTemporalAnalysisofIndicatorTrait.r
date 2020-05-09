#!/usr/bin/env Rscript

########################################################################################################################################################################
############## FUNCTION TO CALCULATE AND PLOT VARIATION IN TIME OF INDICATOR OR MEAN TRAIT VALUE OF COMMUNITIES  function:csi_cti_ctri    ##############################
########################################################################################################################################################################

#### Based on Romain Lorrillière R script
#### Modified by Alan Amosse and Benjamin Yguel for integrating within Galaxy-E
### made with R version 3.5.1


suppressMessages(library(RODBC))  ##Version: 1.3-15
suppressMessages(library(reshape))  ##Version: 0.8.8
suppressMessages(library(data.table))  ##Version: 1.12.0
suppressMessages(library(rgdal))  ##Version: 1.3-4
suppressMessages(library(lubridate))  ##Version: 1.7.4
#suppressMessages(library(RPostgreSQL))  ##Version: 0.6-2
suppressMessages(library(doBy))  ##Version: 4.6-2
suppressMessages(library(arm))  ##Version: 1.10-1
suppressMessages(library(ggplot2))  ##Version: 3.1.0
suppressMessages(library(scales))  ##Version: 1.0.0
suppressMessages(library(mgcv))  ##Version: 1.8-24
#suppressMessages(library(visreg))  ##Version: 2.5-0
suppressMessages(library(plyr))  ##Version: 1.8.4
#suppressMessages(library(lme4))  ##Version: 1.1-18-1
#suppressMessages(library(lmerTest))  ##Version: 3.1-0
suppressMessages(library(glmmTMB)) ###Version: 0.2.3


###########
#delcaration des arguments et variables/ declaring some variables and load arguments

args = commandArgs(trailingOnly=TRUE)
#for( i in 1:length(args)){print(args[i])}
if (length(args)<12) {
    stop("At least 12 arguments must be supplied :\n- An input dataset filtered (.tabular). May come from the filter rare species tool.\n- A species detail table (.tabular)\n- A species ssi/sti table.\n- A table with plots coordinates.\n- table with csi calculated before 2001.\n\n", call.=FALSE) #si pas d'arguments -> affiche erreur et quitte / if no args -> error and exit1
} else {
    Datafiltered<-args[1] ###### Nom du fichier avec extension ".typedefichier", peut provenir de la fonction "FiltreEspeceRare" / file name without the file type ".filetype", may result from the function "FiltreEspeceRare"    
    tabSpecies<-args[2] ###### Nom du fichier avec extension ".typedefichier", fichier mis à disposition dans Galaxy-E avec specialisation à l'habitat des especes et si espece considérée comme indicatrice / file name without the file type ".filetype", file available in Galaxy-E containing habitat specialization for each species and whether or not they are considered as indicator  
    tabtrait<-args[3] ##### Nom du fichier avec extension ".typedefichier", fichier mis à disposition dans Galaxy-E avec degre de specialisation de l espece et affinite thermique /file name without the file type ".filetype", file available in Galaxy-E containing specilalization degree as well as thermic preferences
    coordCarre<-args[4] #### Nom du fichier avec extension ".typedefichier", fichier mis à disposition dans Galaxy-E avec les coordonnees gps des carres /file name without the file type ".filetype", file available in Galaxy-E containing gps coordinates of the plots
    Var <- args[5] #### Nom du trait dans fichier de traits "nomdutrait" exemple: "ssi" pour l'indice de specialisation par sps / Name of the trait in the file containing trait data   
    indicator <- args[6] #### Nom de l'indicateur ou du trait par communauté ex pour ssi c'est csi calculé au niveau communauté / Name of the indicator or the trait per community ex: for the ssi, it is the csi measured at the community level  
    methode <- args[7] #### Methode d'analyse de l'evolution du trait ou de l'indicateur, lmer pour modèloe mixte seul ou gam pour generalized additive model / name of the models used to analyze evolution of mean trait or indicator
    dd <- args[8] ##### Nom du fichier si déjà un fichier avec trait moyen par communauté, avec une colonne annee appelé "year" et une colonne plot appelé "carre" correspondant à l'echelle des communautés etudiées / name of the file if a file with the mean trait value per community is already prepared with one column named "year" for the year, one column named "carre" for the plots (the scale of the community measurment)
    id<-args[9]#Id name for output res repo 
    plot_smooth<-args[10]#TRUE or FALSE
    ic<-args[11]#TRUE or FALSE
    source(args[12])
}

#Import des données / Import data 
tabCLEAN <- read.table(Datafiltered,sep="\t",dec=".",header=TRUE) #### charge le fichier de données d abondance / load abundance of species
tabsp <- read.table(tabSpecies,sep="\t",dec=".",header=TRUE)   #### charge le fichier de donnees sur nom latin, vernaculaire et abbreviation, espece indicatrice ou non / load the file with information on species specialization and if species are indicators

vars_tabCLEAN<-c("carre","annee","espece","abond")
err_msg_tabCLEAN<-"The input dataset filtered doesn't have the right format. It need to have the following 4 variables :\n- carre\n- annee\n- espece\n- abond\n"

vars_tabsp<-c("espece","nom","nomscientific","indicateur","specialisation")
err_msg_tabsp<-"\nThe species dataset filtered doesn't have the right format. It need to have the following 4 variables :\n- espece\n- nom\n- nomscientific\n- indicateur\n- specialisation\n"

check_file(tabCLEAN,err_msg_tabCLEAN,vars_tabCLEAN,4)
check_file(tabsp,err_msg_tabsp,vars_tabsp,5)


if(!dd==""){
    vars_dd<-c("carre","year","longitude_grid_wgs84","latitude_grid_wgs84","indic")  #### si vous avez déjà votre tableau d'analyse indic correspond au trait moyen par communauté ou au calcul de l'indicateur
    err_msg_dd<-"\nThe dataset for analysis doesn't have the right format. It need to have the following 5 variables :\n- carre\n- year\n- longitude_grid_wgs84\n- latitude_grid_wgs84\n- indic\n"
    check_file(dd,err_msg_dd,vars_dd,5)
    dd <- read.table(dd,sep="\t",dec=".",header=TRUE) #### charge le fichier pour analyse si déjà construit (voir ci dessus pour les détails ) / load the required file for the analysis if already prepared (see above for details)
}else{
    dd<-NULL
}

spTrait=read.table(tabtrait,sep="\t",dec=".",header=TRUE) ############# species_indicateur_fonctionnel.csv pour le STOC sinon fichier avec traits pour calcul du trait moyen par communauté / file with the trait for the community weighted mean calculation 
coordCarre=read.table(coordCarre,sep="\t",dec=".",header=TRUE) ######## carre.csv  charge les coordonnées des carrés qui sont utilisés comme covariable  / load the gps coordinates of the plots, is used as covariable in the models

dir.create(paste("Output/",sep=""),recursive=TRUE,showWarnings=FALSE)##### Creation du dossier de sortie
#cat(paste("Create Output/","\n",sep=""))

############################# The function


csi_cti_ctri <- function(tabCLEAN=tabCLEAN,coordCarre=coordCarre,spTrait=spTrait,dd=NULL,ic=TRUE, Var="ssi",indicator="csi", methode="gam", ####### Var= nom du trait dans le fichier de trait (pour le calcul du csi, le trait est ssi par exemple); Indicator= nom de l'indicateur ou du trait moyen par communauté ; methode: choisir modele "gam" ou "glmmtmb" ; ic pour calcul de l'interval de confiance plus rapide sans mais moins fiable / Var=name of the trait in the trait file; Indicator= name of the indicator or of the community weighted mean trait used in graphical output and output files; methode is the statistical model use for the analysis "gam" or "glmmtmb" ; ic is for the calculation of confidence interval faster without but less reliable
                          firstYear=NULL,lastYear=NULL,altitude=800,departement=NULL,onf=TRUE,distance_contact=NULL, #### altitude, departement onf, distance de contact = Argument non utilise, se trouvait dans requete sql / altitude, departement onf, distance de contact = not use anymore was in a postgres request
                         spExcluPassage1=c("MOTFLA","SAXRUB","ANTPRA","OENOEN","PHYTRO"),# (Prince et al. 2013 Env. Sc. and Pol.) + "OENOEN","PHYTRO" avis d'expert F. Jiguet, #### Argument non utilise, se trouvait dans requete sql / not use anymore was in a postgres request
                         seuilAbondance=.99,plot_smooth=TRUE, ###### init_1989 si TRUE, option que pour csi et besoin du fichier des csi calculés sur les données avant 2001 (pas forcement fiable car protocole un peu different) / init_1989 if TRUE, only working for csi, and use calculation of csi based on data before 2001 (protocol was bit different, not totally reliable)
                          champSp = "code_sp", sp=NULL,champsHabitat=FALSE, #### Argument non utilise, se trouvait dans requete sql / not use anymore was in a postgres request
                          anglais=FALSE,seuilSignif=0.05,##### #### anglais=FALSE Argument non utilise, se trouvait dans requete sql / not use anymore was in a postgres request
                          couleur="#4444c3",
                          titreY=indicator,titreX="Années",titre=indicator,
                          savePostgres=FALSE,output=FALSE,   ##### OPTION "output" pour afficher le resultat dans R  / OPTION "output" is only to show the result in the R window 
                          operateur=c("Lorrilliere Romain","lorrilliere@mnhn.fr"), encodingSave="ISO-8859-1",fileName="dataCSI",id="France"){ ####### nom des fichiers de sorties et de l'operateur / name of the output files and of the operator


    start <- Sys.time()
    dateExport <- format(start,"%Y-%m-%d")

    if(is.null(firstYear)) firstYear <- 2001
    if(is.null(lastYear)) lastYear <- 9999
    if(is.null(altitude)) altitude <- 10000

    #############################################  calcul de l'indicateur ou du trait moyen pondéré par communauté / Calculation of the weighted mean trait or of the indicator per community i.e. calculation of the csi, cti, ctri (corresponds to Community weighted mean or CWM for traits)
    if (is.null(dd)){
        colnames(spTrait)[colnames(spTrait) == Var] <- "trait"
        spTrait$trait <- spTrait$trait
        ###browser()
        tabCLEAN$trait <- spTrait$trait[match(tabCLEAN$espece,spTrait$pk_species)] ### recupere donnee du trait par espece calcule  / retrieve trait data for each species 
        tabCLEAN=na.omit(tabCLEAN) ##### pour faire les moyennes pondérées sur les espèces avec des données de trait (donc pas de prise en compte des sps sans traits dans l'abondance totale par carré)
        traitcarre <- aggregate(trait*abond~annee+carre,tabCLEAN,sum) ### somme des traits par annee et carre pondere par les abondances / sum of the trait per year and per plots weighted by abundances
        abcarre <- aggregate(abond~annee+carre,tabCLEAN,sum) ### somme des abondances totales par annee et carre / sum of total abundance per year and plots
        indic <- traitcarre[,3]/abcarre[,3]  #### le trait moyen par carre = indicateur par carre et annee / mean trait per plots = indicator per year and plots
        dd <- data.frame(indic,traitcarre$carre,traitcarre$annee) 
        names(dd)[2] <- "carre"
        names(dd)[3] <- "year"
        dd$longitude_grid_wgs84 <- coordCarre$longitude_grid_wgs84[match(dd$carre,coordCarre$pk_carre)] #### recupere coordonnées gps / retrieve gps coordinates
        dd$latitude_grid_wgs84 <- coordCarre$latitude_grid_wgs84[match(dd$carre,coordCarre$pk_carre)]  #### recupere coordonnées gps / retrieve gps coordinates
        dd$id_plot <- dd$carre  ### id_plot nom données aux carrés dans le script /id_plot is use as the name of the plot in the following script
    }else{
        colnames(dd)[colnames == "indicator"] <- "indic"
    }

    ############################################ fin du calcul de l'indicateur ou du trait moyen pondéré par communauté / end of the calculation of the indicator or the mean trait value per community
    annee <- sort(unique(dd$year))
    nban <- length(annee)
    pasdetemps <-nban-1

    if(methode == "gam") {
        cat("Methode: gam\n")
        ## Utilisation des modèles GAMM pour obtenir les tendances d evolution par an du csi cti ou ctri !!!! Marche pas si peu de données !!!!  / Use of GAMM model for the estimation of the annual variations of the csi cti or ctri  !!! does not work with few data !!! 
        cat("\nEstimation de la variation annuelle ",indicator,"~ factor(year)+s(longitude_grid_wgs84,latitude_grid_wgs84,bs='sos'),random=reStruct(object = ~ 1| id_plot,correlation=corAR1(form=~year)\n",sep="")
        gammf <- gamm(indic ~ factor(year)+s(longitude_grid_wgs84,latitude_grid_wgs84,bs="sos"), data=dd,random=reStruct(object = ~ 1| id_plot, pdClass="pdDiag"),correlation=corAR1(form=~year)) #### spline sur les coordonnées, effet aleatoire sur les carres, methode autoregressive sur l'année N-1  / spline on the gps coordinates, random effect on the plots, autoregressive method on the year-1  
#        gammf <- gamm(indic ~ factor(year)+s(longitude_grid_wgs84,latitude_grid_wgs84,bs="sos"), data=dd,random=reStruct(object = ~ 1| id_plot, pdClass="pdDiag"),correlation=corAR1(form=~year)) #### spline sur les coordonnées, effet aleatoire sur les carres, methode autoregressive sur l'année N-1  / spline on the gps coordinates, random effect on the plots, autoregressive method on the year-1 
        sgammf<-summary(gammf$gam)
        coefdata=coefficients(gammf$gam) ### recupere les coefficient de regression de la variable "annee" / retrieve the regression coefficient of the variable "year"
        coefannee <- c(0,sgammf$p.coeff[2:nban])  ### meme chose que au dessus / same as before
        erreuran <- c(0,sgammf$se[2:nban])### recupere les erreurs standard des coefficient de regression de la variable "annee" / retrieve the standard errors of the regression coefficient of the variable "year"
        pval <-  c(1,sgammf$p.pv[2:nban])### recupere les p value de la variable "annee" / retrieve the p value of the variable year
           

        ## calcul des intervalles de confiance  / confidence interval calculation
        if(ic) {
            # gammf.sim <- sim(gammf)  ######################  VERSION ROMAIN mais fct sim() ne marche pas avec GAMM / old version using function sim() but did not work with Gamm models
            # ic_inf_sim <- c(0,tail(apply(coef(gammf.sim), 2, quantile,.025),pasdetemps))
            # ic_sup_sim <- c(0,tail(apply(coef(gammf.sim), 2, quantile,.975),pasdetemps))
            icalpha05 <- as.data.frame(confint(gammf$gam))[2:nban,1:2]  ########## VERSION BENJ 
            ic_inf_sim <- icalpha05[,1]
            ic_inf_sim <- c("NA",ic_inf_sim[1:nban-1])
            ic_sup_sim <- icalpha05[,2]
            ic_sup_sim <- c("NA",ic_sup_sim[1:nban-1])
        } else{
            ic_inf_sim <- "not assessed"
            ic_sup_sim <- "not assessed"
        }

        tabfgamm <- data.frame(model = "gamm factor(year) plot",annee,coef=coefannee,se = erreuran,pval,signif=pval<seuilSignif,Lower_ci=ic_inf_sim,upper_ci=ic_sup_sim,indicator=indicator) #### recupère les resultats des modèles avec interval de confiance / retrieve results of the models used with confidence interval 
        write.table(tabfgamm,paste("Output/",indicator,"_gammParannee_",id,".tabular",sep=""),row.names=FALSE,sep="\t") 
        gg <- ggplot(data=tabfgamm,aes(x=annee,y=coef))
        gg= gg + geom_line(size=1.5,colour=couleur)+ geom_point(size=3,colour=couleur) +  geom_point(size=1.5,colour="white")

        if (ic){       
            gg <- gg + geom_errorbar(aes(ymin=coef-se, ymax=coef+se), width=0,colour=couleur,alpha=0.5) 
            gg <- gg + geom_ribbon(aes(ymin=coef-se, ymax=coef+se),fill = couleur,alpha=.2)
        }
        gg <- gg + labs(y=indicator,x="annee")+scale_x_continuous(breaks=pretty_breaks())
        ggsave(paste("Output/fig",indicator,"_carre_",id,".png",sep=""),gg)


        if(plot_smooth) {   #### Representation graphique de l'evolution annuelle des indicateurs  / Graphical representation of the annual evolution of the indicators
            cat("\nGam pour la figure ",indicator,"~s(year),random=reStruct(object = ~ 1| id_plot,correlation=corAR1(form=~year)\n",sep="")
            ## create a sequence of temperature that spans your temperature  #####not use anymore 
            ## http://zevross.com/blog/2014/09/15/recreate-the-gam-partial-regression-smooth-plots-from-r-package-mgcv-with-a-little-style/  #### method for the plot
            ####dd$yearf=factor(dd$year) PAS BON j'ai modifié la ligne suivante en mettant s(year) et plus s(yearsf)
            gammgg <- gamm(indic ~ s(year), data=dd,random=reStruct(object = ~ 1| id_plot, pdClass="pdDiag"),correlation=corAR1(form=~year))  #### spline sur l'année, effet aleatoire des carres sur ordonnée à l'origine, methode autoregressive sur l'année N-1  / spline on the year, random effect of the plots on the intercept, autoregressive method on the year-1 
            maxyear<-max(dd$year)
            minyear<-min(dd$year)
            year.seq<-sort(unique(c(minyear:maxyear,(seq(minyear, maxyear,length=1000)))))
            year.seq<-data.frame(year=year.seq)
            # predict only the temperature term (the sum of the   ########### ???? not use anymore
            # term predictions and the intercept gives you the overall########### ???? not use anymore
            # prediction)########### ???? not use anymore
            preds<-predict(gammgg$gam, newdata=year.seq, type="terms", se.fit=TRUE)  #### Utilise model pour predire les valeurs de indic sur sequence d'années defini au dessus \ Use of the model to predict value of the indicator in the year sequence define above
            # set up the temperature, the fit and the upper and lower########### ???? not use anymore
            # confidence interval########### ???? not use anymore
            year <-year.seq$year
            realYear <- sort(unique(dd$year))
            fit<-as.vector(preds$fit)
            init <- fit[1]
            fit.up95 <- fit-1.96*as.vector(preds$se.fit)    
            fit.low95 <- fit+1.96*as.vector(preds$se.fit)
            # ggGamData <- data.frame(year=year, csi=fit,ic_low95 = fit.low95, ic_up95 = fit.up95)
            fit <- fit - init ### Réechelonne les predictions du modèle sur la 1ère valeure de la prediction  ? ne sait pas pourquoi
            fit.up95 <- fit.up95 - init  ### Réechelonne IC superieur sur la 1ère valeure de la prediction  ? ne sait pas pourquoi
            fit.low95 <- fit.low95 - init  ### Réechelonne IC inferieur sur la 1ère valeure de la prediction  ? ne sait pas pourquoi
            ggGamData <- data.frame(year=year, indic=fit,ic_low95 = fit.low95, ic_up95 = fit.up95)  ####### Recupère les resultats des modèles / retrieve the results of the models 
   

            ## The ggplot:
            gg <- ggplot(data=ggGamData,aes(x=year,y=indic))+ geom_line(size=1,colour=couleur)
            if (ic) {
                gg <- gg + geom_ribbon(aes(ymin=ic_low95, ymax=ic_up95),fill = couleur,alpha=.2)
            }
            gg <- gg +  geom_point(data = subset(ggGamData,year %in% realYear),size=3,colour=couleur) + geom_point(data = subset(ggGamData,year %in% realYear),size=1.5,colour="white")
            gg <- gg + labs(y=titreY,x=titreX,title=titre)+scale_x_continuous(breaks=pretty_breaks())
            ggsave(paste("Output/fig",indicator,"_plot",id,".png",sep=""),gg)
            #cat("\n--> Output/fig",indicator,"_plot",id,".png\n",sep="")

            tabPredict <- subset(ggGamData,year %in% realYear)########### Tableau des resultats pour ne prendre que les valeurs d'IC pour l'année pas entre les années (spline sur annee) !!!plus utilisé!! / Table of the results not taking confidence interval between year but at each year (because of the spline of year)
            colnames(tabPredict)[1:2] <- c("annee",paste(indicator,"_predict",sep=""))
            tabgamm <- merge(tabfgamm,tabPredict,by="annee") #### Desactivation car merge sortie de modèles différents (le modèle dont on tire les coef de regression pour année, avec spli ne sur les coordonnées geo vs celui pour faire la figure avec splin sur année uniquement)  / not use anymore (as before) because use the results of the restricted model with the spline on the year while the better analysis is on full model with the spline on gps coordinates
            ##tabgamm <-  tabfgamm  #### remplace la ligne au dessus  / replace the line above
            write.table(tabPredict,paste("Output/",indicator,"_gammsmooth",id,".tabular",sep=""),row.names=FALSE,sep="\t")
        }else {
            tabgamm = tabfgamm
        }
        ###### CROCHET deplacer apres le dernier ggplot ici mais dans version Romain CROCHET placé AVANT le dernier ggplot qui n'était pas dans la condition if(init_1989)
        cat("\nEstimation de la tendence  ",indicator,"~ year+s(longitude_grid_wgs84,latitude_grid_wgs84,bs='sos'),random=reStruct(object = ~ 1| id_plot,correlation=corAR1(form=~year)\n")
        gammc <- gamm(indic~year+s(longitude_grid_wgs84,latitude_grid_wgs84,bs="sos"), data=dd,random=reStruct(object = ~ 1| id_plot, pdClass="pdDiag"),correlation=corAR1(form=~year))### spline sur les coordonnées, effet aleatoire des carres sur ordonnée à l'origine, methode autoregressive sur l'année N-1  / spline on the gps coordinates, random effect of the plots on intercept, autoregressive method on the year-1
        sgammc=summary(gammc$gam)
        coefannee <- sgammc$p.coeff[2]  #### coefficient de regression de la variable année / regression coefficient of the variable "year"
        ## erreur standard / standard error
        erreuran <- sgammc$se[2]
        ## p value
        pval <-  sgammc$p.pv[2]

        #### Calcul des intervalles de confiances / calculation of the confidence intervals
        if(ic) {
            # gammc.sim <- sim(gammc)######################  VERSION ROMAIN mais fct sim() marche pas avec Gamm / old version using function sim() but did not work with Gamm models
            # ic_inf_sim <- c(0,tail(apply(coef(gammc.sim), 2, quantile,.025),pasdetemps))
            # ic_sup_sim <- c(0,tail(apply(coef(gammc.sim), 2, quantile,.975),pasdetemps))
            icalpha052 <- as.data.frame(confint(gammc$gam))[2,1:2]  ########## VERSION BENJ
            ic_inf_sim2 <- icalpha052[,1]
            ic_sup_sim2 <- icalpha052[,2]
        } else{
            ic_inf_sim2 <- "not assessed"
            ic_sup_sim2 <- "not assessed"
        }
        tabcgamm <- data.frame(model = "gamm numeric(year) plot",annee = NA,coef = coefannee,se = erreuran,pval,signif = pval<seuilSignif, indicator= indicator , Lower_ci = as.factor(ic_inf_sim2), upper_ci = as.factor(ic_sup_sim2), csi_predict =NA ,ic_low95 =NA,ic_up95=NA)#### recupère les resultats des modèles avec interval de confiance / retrieve results of the models used with confidence interval
        ########### MODIF tabcgamm en remplacant ic_low95 ic_up95 par Lower_ci et upper_ci pour coller avec les sorties des modèles "pour les stats" et non celui utilisé pour le graphe uniquement, et rajout des colonnes spécifique a model ggsmooth pour les garder
#### ai rajouté aussi as.factor(ic) car ne savait pas pourquoi mais tabfgamm sont en facteur et besoin de la meme class pour rbind() ci dessous
        tabcgamm <- tabcgamm[,colnames(tabgamm)]  ##
        tabgamm <- tabgamm[,colnames(tabcgamm)]  #### recupère que les colonnes de tabcgamm donc perds les infos du modèle du ggsmooth si pas declarer dans le tableau tabcgamm (maintenant c fait)
        tabgamm <- rbind(tabgamm,tabcgamm)
        write.table(tabgamm,paste("Output/",indicator,"_gammCOMPLET_",id,".tabular",sep=""),row.names=FALSE,sep="\t")
        #cat("\n  --> Output/",indicator,"_gammPlot_",id,".tabular\n",sep="")
    }

    
    if (methode == "glmmtmb") {
        #cat("Method : lmer \n")
        cat("Method : glmmTMB \n")
        ###################
        ### Utilisation des modèles mixtes pour obtenir les tendances d evolution par an du csi cti ou ctri / Use of mixte model for the estimation of the annual variations of the csi cti or ctri 
        #cat("\nEstimation de la variation annuelle lmer(",indicator,"~ factor(year)+(1|id_plot)\n",sep="")
        cat("\nEstimation de la variation annuelle glmmTMB(",indicator,"~ factor(year)+(1|id_plot)\n",sep="")
        #md.f <- lmer(indic~ factor(year)+(1|id_plot),data=dd)  ##### effet aleatoire liés aux carrés sur l'ordonnée à l'origine / random effects of plots on intercept 
        md.f <- glmmTMB(indic~ factor(year)+(1|id_plot),data=dd) 
        smd.f <- summary(md.f)    
        # coefdata.f <-  as.data.frame(smd.f$coefficients)  ### version pour sortie lmer()
        coefdata.f <-  as.data.frame(smd.f$coefficients$cond[-1,])
        coefdata.f <- data.frame(model="Annual fluctuation", variable = rownames(coefdata.f),coefdata.f)
        # ggdata <<- data.frame(year=c(as.numeric(substr(coefdata.f$variable[-1],13,16))),##### version pour sortie lmer()
        ggdata <- data.frame(year=c(as.numeric(substr(coefdata.f$variable,13,17))),             
        estimate=c(coefdata.f$Estimate),
        se=c(coefdata.f$Std..Error))   #####################  resultat du modèle / results of the models
        #ggdata$estimate <-  ggdata$estimate
        #ggdata$se.supR <- ggdata$estimate +  ggdata$se ############################################################################## METHODE ROMAIN 
        #ggdata$se.infR <- ggdata$estimate -  ggdata$se
        # ggdata$estimate2 <- c(coefdata.f$Estimate[1],coefdata.f$Estimate[1] + coefdata.f$Estimate[-1])
        # ggdata$se.sup2 <- ggdata$estimate2 +  ggdata$se
        # ggdata$se.inf2 <- ggdata$estimate2 -  ggdata$se
        #prof <- profile(md.f)  #### Nouvel interval de confiance avec utilisation du logarithme des ecarts types / logarithms of standard deviations are used, while varianceProf converts from the standard-deviation to the variance scale
        MODconfint <- confint (md.f) #### plus rapide de ne pas passer par la fonction profile et pas indispensable fonctionne aussi directement sur modele mixte md.f  / more rapid using both function profile and confint but works also directly on output of the model 
        se.sup <- MODconfint[2:nban,2]#### [2:nban+2,2] version pour sortie lmer()
        se.inf <- MODconfint[2:nban,1]#### [2:nban+2,2] version pour sortie lmer()
        if (ic) {
            ggdata$se.sup <- se.sup 
            ggdata$se.inf <- se.inf
        } else{
            ggdata$se.sup <- "not assessed"
            ggdata$se.inf <- "not assessed"
        }
        coefdata.f$se.inf <- ggdata$se.inf
        coefdata.f$se.sup <- ggdata$se.sup
        #gg <<- ggplot(ggdata,aes(x=year,y=estimate))+ geom_ribbon(ymin=ggdata$se.infR,ymax=ggdata$se.supR,alpha=.25)+geom_errorbar(ymin=ggdata$se.infR,ymax=ggdata$se.supR,width=0,alpha=.25)+ geom_point() + geom_line() + ylim(min(ggdata$se.infR),max(ggdata$se.supR)) + labs(x="Years",y=paste(indic," variation",sep="")) #####  AVEC INTERVAL ROMAIN
        gg <- ggplot(ggdata,aes(x=year,y=estimate))+ geom_point() + geom_line()  + labs(x="Years",y=paste(indicator," variation",sep="")) #####+ ylim(min(ggdata$se.inf),max(ggdata$se.sup))
        gg <- gg + geom_line(size=1.5,colour=couleur)+ geom_point(size=3,colour=couleur) +  geom_point(size=1.5,colour="white")
        if (ic)	{
            gg <- gg + geom_ribbon(aes(ymin=ggdata$se.inf,ymax=ggdata$se.sup),alpha=.25,fill = couleur) + geom_errorbar(ymin=ggdata$se.inf,ymax=ggdata$se.sup,width=0,alpha=.25)
        }
        ggfile <- paste("Output/",indicator,"_glmmTMB_",id,".png",sep="")
        ggsave(ggfile,gg)

        ############ Estimation de la tendance sur la periode étudiée  / Trends estimation on the time period studied
        #cat("\nEstimation de la tendance lmer(",indicator,"~ year+(1|id_plot)\n",sep="")
        cat("\nEstimation de la tendance glmmTMB(",indicator,"~ year+(1|id_plot)\n",sep="")
        #md.c <- lmer(indic~ year+(1|id_plot),data=dd)##### effet aleatoire liés aux carrés sur l'ordonnée à l'origine / random effects of plots on intercept ### version lmer
        md.c <- glmmTMB(indic~ year+(1|id_plot),data=dd)
        smd.c<-summary(md.c)
        # coefdata.c <-  as.data.frame(smd.c$coefficients) #### pour la version lmer
        coefdata.c <-  as.data.frame(smd.c$coefficients$cond)[2,]
        #profc=profile(md.c) ######### Ajout des intervalles de confiances / addition of the confidence intervals
        if (ic) {
            MODconfint=confint(md.c) ### plus rapide de ne pas passer par profile
            se.inf=MODconfint[2,1]### [4,1] pour la version lmer
            se.sup=MODconfint[2,2]### [4,2] pour la version lmer
        } else{
            se.inf <- "not assessed"
            se.sup <- "not assessed"
        }
        coefdata.c <- data.frame(model = "Linear trend", variable = rownames(coefdata.c),coefdata.c,se.inf,se.sup)
        coefdata <- rbind(coefdata.c,coefdata.f)
        write.table(coefdata,paste("Output/GlmmTMB_coefficient_",indicator,id,".tabular",sep=""),row.names=FALSE,sep="\t")
        write.table(ggdata,paste("Output/ggdata_",indicator,id,".tabular",sep=""),row.names=FALSE,sep="\t")
        smd.file <- paste("Output/summary_lmer_",indicator,"_",id,".txt",sep="")
        #####################
    }
}





################## 
###  Do your analysis

csi_cti_ctri(tabCLEAN=tabCLEAN,coordCarre=coordCarre,spTrait=spTrait,dd=NULL,Var=Var,indicator=indicator,ic=ic,plot_smooth = plot_smooth,methode=methode)#,init_1989 = FALSE)  ##### exemple pour l'indicateur csi sans csi déjà calculé donc à partir du ssi avec interval de confiance et utilisant modele mixte / example for the csi index which is not already calculated from the ssi with confidence interval using the mixte model 


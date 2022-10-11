# author: "Jonathan Richir"
# date: "19 April 2021"


#Rscript

###############################
##  ##
###############################

#####Packages : dplyr
#               tidyr
#               readr
#               writexl
#               stringr
#               readxl
#               tibble
#               lubridate
#               cowplot
#               magrittr
#               rmarkdown
library(magrittr)
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    input_data <- args[1]
    input_data2 <- args[2]
    fiche_val <- args[3]
    fiche_term <- args[4]

}


# load qecb data

qecb <- read.csv2(input_data, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

qecb_next <- read.csv2(input_data2, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

# bind qecb dfs.

qecb <- dplyr::bind_rows(qecb, qecb_next)
rm(qecb_next)

# import csv files ficheterrain

fiche <- read.csv2(fiche_val, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

fiche_next <- read.csv2(fiche_term, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

# bind ficheterrain
fiche <- dplyr::bind_rows(fiche, fiche_next)
rm(fiche_next)

## work on "Fiche terrain"

date_fiche <- as.Date(stringr::str_sub(fiche$date.sortie, end = 10), origin = "1970-01-01")
fiche <- tibble::add_column(fiche, date_fiche, .after = "date.sortie")
rm(date_fiche)

## qecb vs fiche terrain

fiche_red <- dplyr::filter(fiche, fiche$ID.Fiche %in% unique(qecb[, c("id")]))

id_count <- qecb %>% dplyr::group_by(id) %>% dplyr::count()
id_count <- dplyr::rename(id_count, "ID.Fiche" = "id")
id_count <- data.frame(id_count)
fiche_red <- dplyr::left_join(fiche_red, id_count)

# rep fiche terrain information
fiche_expanded <- fiche_red[rep(row.names(fiche_red), fiche_red$n), 1:ncol(fiche_red)]
fiche_expanded <- dplyr::rename(fiche_expanded, "id" = "ID.Fiche")

## merge qecb data and ficheterrain information

qecb <- dplyr::bind_cols(qecb, fiche_expanded)
qecb <- dplyr::rename(qecb, "id_qecb" = "id...1")
qecb <- dplyr::rename(qecb, "id_fiche" = "id...68")

rm(fiche_expanded, fiche_red, id_count)

qecb <- qecb %>% tidyr::separate(date_fiche, c("Year", "Month", "Day"), sep = "-", remove = FALSE)


## quadrat nb : in contrast to ivr df, quadrat number is missing for many observations in qecb df ; but from the Numero.Photo variable (boulder photo associated to field data collection), I could get back most missing quadrat numbers

quadrat <- stringr::str_extract(qecb$Numero.Photo, "Q[12345]")
qecb <- tibble::add_column(qecb, quadrat, .after = "Numero.Photo")
rm(quadrat)

# check
quadrat_bis <- rep(NA, length = nrow(qecb))
qecb <- tibble::add_column(qecb, quadrat_bis, .after = "quadrat")
rm(quadrat_bis)

qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(1, 2) & qecb$Type.Bloc == "Bloc mobile", "Q1", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(3, 4) & qecb$Type.Bloc == "Bloc mobile", "Q2", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(5, 6) & qecb$Type.Bloc == "Bloc mobile", "Q3", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(7, 8) & qecb$Type.Bloc == "Bloc mobile", "Q4", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon %in% c(9, 10) & qecb$Type.Bloc == "Bloc mobile", "Q5", qecb$quadrat_bis)


qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 1 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q1", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 2 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q2", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 3 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q3", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 4 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q4", qecb$quadrat_bis)
qecb$quadrat_bis <- ifelse(qecb$Numéro.Bloc.échantillon == 5 & qecb$Type.Bloc %in% c("Bloc fixé", "Roche en place"), "Q5", qecb$quadrat_bis)

## I create two new variables for Site names, one for data analysis and one for data reporting. Only works for actual ivr df with 22 sites !

# Name for data analysis

qecb <- tibble::add_column(qecb, Site = NA, .after = "ID.Fiche")
unique(qecb$Site)

qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[1], "GDMO_Locmariaquer", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[2], "GDMO_BegLann", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[3], "FOUR_PlateauFour", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[4], "EGMP_GroinCou", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[5], "EGMP_PasEmsembert", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[6], "EGMP_BreeBains", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[7], "EGMP_PerreAntiochat", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[8], "EGMP_Chassiron", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[9], "BASQ_FlotsBleusZP", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[10], "BASQ_FlotsBleusZF", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[11], "GONB_IlotStMichel", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[12], "FINS_Quemenes", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[13], "FINS_SeinGoulenez", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[14], "FINS_SeinKilaourou", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[15], "ARMO_Verdelet", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[16], "ARMO_Piegu", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[17], "ARMO_Bilfot", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[18], "ARMO_IlePlate", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[19], "PDMO_Perharidy", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[20], "BRES_Keraliou", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[21], "FINS_Mousterlin", qecb$Site)
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[22], "FINS_StNicolasGlenan", qecb$Site)

unique(qecb$Site)

# Anne Boulet forgot to specify zone.habitat in 2020. I asked her to correct it in ESTAMP
qecb$Site <- ifelse(qecb$zone.habitat == unique(qecb$zone.habitat)[23], "GDMO_Locmariaquer", qecb$Site)
unique(qecb$Site)
unique(qecb[, c("Site", "zone.habitat")])

# Name for report/plot

qecb <- tibble::add_column(qecb, Site_bis = NA, .after = "Site")

qecb$Site_bis <- ifelse(qecb$Site == "GDMO_Locmariaquer", "Locmariaquer", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "GDMO_BegLann", "Beg Lann", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FOUR_PlateauFour", "Plateau du Four", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "EGMP_GroinCou", "Groin du Cou", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "EGMP_PasEmsembert", "Le Pas d'Emsembert", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "EGMP_BreeBains", "La Brée-les-Bains", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "EGMP_PerreAntiochat", "Le Perré d'Antiochat", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "EGMP_Chassiron", "Chassiron", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "BASQ_FlotsBleusZP", "Les Flots Bleus / zone pêcheurs", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "BASQ_FlotsBleusZF", "Les Flots Bleus / zone familles", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "GONB_IlotStMichel", "Îlot Saint-Michel", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FINS_Quemenes", "Quéménès", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FINS_SeinGoulenez", "Île de Sein - Goulenez", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FINS_SeinKilaourou", "Île de Sein - Kilaourou", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "ARMO_Verdelet", "Îlot du Verdelet", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "ARMO_Piegu", "Piégu", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "ARMO_Bilfot", "Pointe de Bilfot", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "ARMO_IlePlate", "Île Plate", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "PDMO_Perharidy", "Perharidy", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "BRES_Keraliou", "Keraliou", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FINS_Mousterlin", "Pointe de Mousterlin", qecb$Site_bis)
qecb$Site_bis <- ifelse(qecb$Site == "FINS_StNicolasGlenan", "Saint-Nicolas des Glénan", qecb$Site_bis)

unique(qecb[, c("Site", "Site_bis")])


## change some variables to factor

# change 'X..' variables that are indeed % to numeric; https://stackoverflow.com/questions/59410939/apply-function-to-all-variables-with-string-in-name
ix <- grep("^X..", names(qecb))
qecb[ix] <- lapply(qecb[ix], as.numeric)
rm(ix)


## save the final, complete qecb df_

qecb <- qecb[, c(72:107, 1:71)]

saveRDS(qecb, "qecb.RDS")


## qecb df preparation prior qecb calculation

# Several issues to solve in the df first


qecb$Type.Bloc <- factor(qecb$Type.Bloc, levels = c("Bloc mobile", "Bloc fixé", "Roche en place"))

qecb$Face <- factor(qecb$Face, levels = c("face supérieure", "face inférieure"))

qecb <- dplyr::arrange(qecb, Type.Bloc, Face, Numéro.Bloc.échantillon)

qecb <- tibble::add_column(qecb, site_year_month_day = paste0(qecb$Site, ".", qecb$Year, ".", qecb$Month, ".", qecb$Day), .after = "Site_bis")

# save qecb as a new df_ for analysis purpose => several changes to operate to run the code and functions

qecbnew <- qecb

# df with list object nb and corresponding site_year_month_day value to solve for loop issues

df_list_loop <- data.frame("site_year_month_day" = unique(qecbnew$site_year_month_day),
     "loop nb" = c(1:length(unique(qecbnew$site_year_month_day))))

# dplyr::filter for df that makes problem, then eventually correct in the dataframe for wrong coding; brackets (xx) for nb because will change when qecb df_ enlarged.
# these listed boulder field survey error when highlighted when running the loop, that ran into an error ; it was a step by step procedure with solving one listed observation after another when issues appeared. Surely not the best way to proceed, maybe better just to skip these surveys (site + date), but in the present case I wanted to keep most of the observations, therefore I corrected them manually whenever needed.

# list nb (28) - EGMP_BreeBains.2016.04.06
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_La Bree_20160406_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-LaBreeB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_La Bree_20160406_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-LaBreeB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)
unique(qecbnew$Face)

# list nb 33 - EGMP_PerreAntiochat.2016.04.07
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_PerAnt_20160407_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-PerAntB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_PerAnt_20160407_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-PerAntB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)
unique(qecbnew$Face)

# list nb 37 - EGMP_Chassiron.2016.03.09
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Chassiron_20160309&10_VImport.xlsx" & qecbnew$Référence.bloc == "mars16-ChassB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Chassiron_20160309&10_VImport.xlsx" & qecbnew$Référence.bloc == "mars16-ChasB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)
unique(qecbnew$Face)

# list nb 76 - ARMO_Verdelet.2015.03.23
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Verdelet_20150323_VImport.xlsx" & qecbnew$Référence.bloc == "mar15-VerB10inf", "face inférieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)
unique(qecbnew$Face)

# list nb 116 - "GDMO_Locmariaquer.2018.09.10"
qecbnew$Type.Bloc <- as.character(qecbnew$Type.Bloc)
qecbnew$Type.Bloc <- ifelse(qecbnew$ID.Fiche == "2018-09-10-GDMO-CDB-001" & qecbnew$Numero.Photo == "2018-09-10_GDMO_01_CDB-5_sup_392578.jpg", "Roche en place", qecbnew$Type.Bloc)
qecbnew$Type.Bloc <- as.factor(qecbnew$Type.Bloc)
qecbnew$quadrat_bis <- ifelse(qecbnew$ID.Fiche == "2018-09-10-GDMO-CDB-001" & qecbnew$Numero.Photo == "2018-09-10_GDMO_01_CDB-5_sup_392578.jpg", "Q5", qecbnew$quadrat_bis)
qecbnew <- qecbnew %>% dplyr::filter(!(ID.Fiche == "2018-09-10-GDMO-CDB-001" & Numero.Photo == ""))

# Few sites to remove prior running the for loop because it was not just a encoding mistake for one data, but a globally wroing coding for the site + date survey.

qecb_i <- qecbnew  %>% dplyr::filter(site_year_month_day == "FINS_StNicolasGlenan.2016.04.08") # no bloc fixe !
qecbnew <- qecbnew  %>% dplyr::filter(site_year_month_day != "FINS_StNicolasGlenan.2016.04.08")
qecb_i <- qecbnew  %>% dplyr::filter(site_year_month_day == "GDMO_Locmariaquer.2019.09.30") # most faces of blocs mobiles do not correspond to each other; only 3 over 10 boulder have data for both face supérieure and face inférieure
qecbnew <- qecbnew  %>% dplyr::filter(site_year_month_day != "GDMO_Locmariaquer.2019.09.30")
rm(df_list_loop, qecb_i)


# check for species with count within sub-0.1m^2-quadrat (i.e. reduced size quadrat compare to most organisms on boulder to count them, because abundant ; then some extrapolation)

# first for Spirobranchus

qecbnew$Nb.Spirobranchus.lamarckii.total.ini <- qecbnew$Nb.Spirobranchus.lamarckii.total
qecbnew$Nb.Spirobranchus.lamarckii.total <- as.character(qecbnew$Nb.Spirobranchus.lamarckii.total)

table(qecbnew$Nb.Spirobranchus.lamarckii.total)
subset(qecbnew, is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))
nrow(subset(qecbnew, is.na(qecbnew$Nb.Spirobranchus.lamarckii.total)))
subset(qecbnew, is.nan(qecbnew$Nb.Spirobranchus.lamarckii.total))
subset(qecbnew, is.finite(qecbnew$Nb.Spirobranchus.lamarckii.total))
nrow(dplyr::filter(qecbnew, qecbnew$Nb.Spirobranchus.lamarckii.total %in% c(NA, "NaN", "Inf", "-Inf")))

qecbnew_spirobranchus <- (dplyr::filter(qecbnew, Nb.Spirobranchus.lamarckii.total %in% c(NA, "NaN", "Inf", "-Inf")))
qecbnew_spirobranchus[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")] <- sapply(qecbnew_spirobranchus[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], as.character)
(spirobranchus_data <- subset(qecbnew_spirobranchus, !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.1B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.2B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.3B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.4B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.5B))[, c("site_year_month_day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
unique(spirobranchus_data$site_year_month_day)

quemenes <- dplyr::filter(qecbnew, Site == "FINS_Quemenes")
quemenes <- dplyr::arrange(quemenes, date_fiche)
# for Quemenes, issue because for sampling date "FINS_Quemenes.2015.09.30" the 5 counts of Spirobranchus were encoded in 1B instead of total !!! I noticed this issue when mining data (see below), therefore I corrected before running below script for Spirobranchus.
qecbnew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbnew$site_year_month_day == "FINS_Quemenes.2015.09.30" & is.na(qecbnew$Nb.Spirobranchus.lamarckii.total), qecbnew$Nb.Spirobranchus.lamarckii.1B, qecbnew$Nb.Spirobranchus.lamarckii.total)
(quemenes <- dplyr::filter(qecbnew, site_year_month_day == "FINS_Quemenes.2015.09.30")[, c("site_year_month_day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
rm(quemenes)

seinkilaourou <- dplyr::filter(qecbnew, Site == "FINS_SeinKilaourou")
seinkilaourou <- dplyr::arrange(seinkilaourou, date_fiche)
# same issue with SeinKilaourou
qecbnew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbnew$site_year_month_day == "FINS_SeinKilaourou.2015.04.21" & is.na(qecbnew$Nb.Spirobranchus.lamarckii.total), qecbnew$Nb.Spirobranchus.lamarckii.1B, qecbnew$Nb.Spirobranchus.lamarckii.total)
(seinkilaourou <- dplyr::filter(qecbnew, site_year_month_day == "FINS_SeinKilaourou.2015.04.21")[, c("site_year_month_day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])
rm(seinkilaourou)

# some more issues however with "x100"count data

Spirobranchus <- subset(qecbnew, !is.na(qecbnew$Nb.Spirobranchus.lamarckii.1B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.2B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.3B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.4B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.5B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))[, c("site_year_month_day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")]
for (i in c(1:nrow(Spirobranchus))) {
  Spirobranchus$mean.x.100[[i]] <- sum(Spirobranchus[i, c(2:6)], na.rm = TRUE) / sum(!is.na(Spirobranchus[i, c(2:6)])) * 100
}
Spirobranchus$mean.x.100 <- unlist(Spirobranchus$mean.x.100)
Spirobranchus$Nb.Spirobranchus.lamarckii.total <- as.numeric(Spirobranchus$Nb.Spirobranchus.lamarckii.total)
for (i in c(1:nrow(Spirobranchus))) {
  Spirobranchus$diff[[i]] <- Spirobranchus[i, "Nb.Spirobranchus.lamarckii.total"] - Spirobranchus[i, "mean.x.100"]
}
Spirobranchus$diff <- abs(as.integer(Spirobranchus$diff))
Spirobranchus <- dplyr::arrange(Spirobranchus, desc(diff), mean.x.100)
Spirobranchus <- dplyr::arrange(dplyr::filter(Spirobranchus, diff != 0 & mean.x.100 != 0), desc(diff))

# check it all in the qecbnew df

for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.100[[i]] <-
    #ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"),
    sum(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = TRUE) / sum(!is.na(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")])) * 100
  #, qecbnew$Nb.Spirobranchus.lamarckii.total[[i]])
} # sum of only NAs/0 = NaN; so replace NaN by Na
qecbnew$mean.x.100 <- as.character(qecbnew$mean.x.100)

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$site_year_month_day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew_spirobranchus$id_qecb, "_", qecbnew_spirobranchus$site_year_month_day, "_", qecbnew_spirobranchus$Type.Bloc, "_", qecbnew_spirobranchus$Numéro.Bloc.échantillon, "_", qecbnew_spirobranchus$Face))[, "mean.x.100"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.100[[i]] <- ifelse(qecbnew$mean.x.100[[i]] == "NaN", NA, qecbnew$mean.x.100[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$mean.x.100)))
qecbnew$mean.x.100 <- as.integer(qecbnew$mean.x.100)

qecbnew$Nb.Spirobranchus.lamarckii.total <- as.integer(qecbnew$Nb.Spirobranchus.lamarckii.total)
unique(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$mean.x.100)
table(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$mean.x.100)
qecbnew$Nb.Spirobranchus.lamarckii.total.diff <- abs((qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$mean.x.100))
spirobranchus_diff <- qecbnew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
spirobranchus_diff <- dplyr::arrange(spirobranchus_diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
spirobranchus_diff <- dplyr::arrange(dplyr::filter(spirobranchus_diff, Nb.Spirobranchus.lamarckii.total.diff != 0 & mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))

qecbnew$Nb.Spirobranchus.lamarckii.total <- ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total.diff != 0 & qecbnew$mean.x.100 != 0, qecbnew$mean.x.100, qecbnew$Nb.Spirobranchus.lamarckii.total)
spirobranchus_diff <- qecbnew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
spirobranchus_diff$Nb.Spirobranchus.lamarckii.total.diff <- abs(as.integer(spirobranchus_diff$Nb.Spirobranchus.lamarckii.total.diff))
spirobranchus_diff <- dplyr::arrange(spirobranchus_diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
spirobranchus_diff <- dplyr::arrange(dplyr::filter(spirobranchus_diff, Nb.Spirobranchus.lamarckii.total.diff != 0 & mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))
# ok, change made when data x 100 was not correct.

# finally, change NA by mean.x100 for Spirobranchus total
qecbnew$Nb.Spirobranchus.lamarckii.total <- as.character(qecbnew$Nb.Spirobranchus.lamarckii.total)
for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] <- ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), sum(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = TRUE) / sum(!is.na(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")])) * 100, qecbnew$Nb.Spirobranchus.lamarckii.total[[i]])
} # sum of only NAs/0 = NaN; so replace NaN by Na

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$site_year_month_day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew_spirobranchus$id_qecb, "_", qecbnew_spirobranchus$site_year_month_day, "_", qecbnew_spirobranchus$Type.Bloc, "_", qecbnew_spirobranchus$Numéro.Bloc.échantillon, "_", qecbnew_spirobranchus$Face))[, "Nb.Spirobranchus.lamarckii.total"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] <- ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] == "NaN", NA, qecbnew$Nb.Spirobranchus.lamarckii.total[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$Nb.Spirobranchus.lamarckii.total)))
qecbnew$Nb.Spirobranchus.lamarckii.total <- as.integer(qecbnew$Nb.Spirobranchus.lamarckii.total)

unique(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$Nb.Spirobranchus.lamarckii.total.ini)
table(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$Nb.Spirobranchus.lamarckii.total.ini)
qecbnew$Nb.Spirobranchus.lamarckii.total.diff <- abs(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$Nb.Spirobranchus.lamarckii.total.ini)
spirobranchus_diff <- qecbnew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
spirobranchus_diff <- dplyr::arrange(spirobranchus_diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
spirobranchus_diff <- dplyr::arrange(dplyr::filter(spirobranchus_diff, Nb.Spirobranchus.lamarckii.total.diff != 0 & mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))
table(qecbnew$Nb.Spirobranchus.lamarckii.total.diff)
length(na.omit(qecbnew$Nb.Spirobranchus.lamarckii.total))
sum(is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))
length(na.omit(qecbnew$Nb.Spirobranchus.lamarckii.total)) + sum(is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))

qecbnew <- subset(qecbnew, select = -c(Nb.Spirobranchus.lamarckii.total.ini, mean.x.100, Nb.Spirobranchus.lamarckii.total.diff))

rm(qecbnew_spirobranchus, Spirobranchus, spirobranchus_data, spirobranchus_diff)

# do the same for spirorbis

qecbnew$Nb.spirorbis.total.ini <- qecbnew$Nb.spirorbis.total
qecbnew$Nb.spirorbis.total <- as.character(qecbnew$Nb.spirorbis.total)

table(qecbnew$Nb.spirorbis.total)
subset(qecbnew, is.na(qecbnew$Nb.spirorbis.total))
nrow(subset(qecbnew, is.na(qecbnew$Nb.spirorbis.total)))
subset(qecbnew, is.nan(qecbnew$Nb.spirorbis.total))
subset(qecbnew, is.finite(qecbnew$Nb.spirorbis.total))

nrow(dplyr::filter(qecbnew, qecbnew$Nb.spirorbis.total %in% c(NA, "NaN", "Inf", "-Inf")))

qecbnew_spirorbis <- (dplyr::filter(qecbnew, Nb.spirorbis.total %in% c(NA, "NaN", "Inf", "-Inf")))
qecbnew_spirorbis[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")] <- sapply(qecbnew_spirorbis[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], as.character)
(spirobranchus_data <- subset(qecbnew_spirorbis, !is.na(qecbnew_spirorbis$Nb.spirorbis.1A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.2A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.3A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.4A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.5A))[, c("site_year_month_day", "Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")])
unique(spirobranchus_data$site_year_month_day)

# In contrast to Spirobranchus data, no encoding issues for spirorbis data, cfr when sub-quadrat 1A-5A are ALL encoded, NA for total.

# some more issues however with "x200"count data

spirorbis <- subset(qecbnew, !is.na(qecbnew$Nb.spirorbis.1A) & !is.na(qecbnew$Nb.spirorbis.2A) & !is.na(qecbnew$Nb.spirorbis.3A) & !is.na(qecbnew$Nb.spirorbis.4A) & !is.na(qecbnew$Nb.spirorbis.5A) & !is.na(qecbnew$Nb.spirorbis.total))[, c("site_year_month_day", "Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")]
for (i in c(1:nrow(spirorbis))) {
    spirorbis$mean.x.200[[i]] <- sum(spirorbis[i, c(2:6)], na.rm = TRUE) / sum(!is.na(spirorbis[i, c(2:6)])) * 200
}
spirorbis$mean.x.200 <- unlist(spirorbis$mean.x.200)
spirorbis$Nb.spirorbis.total <- as.numeric(spirorbis$Nb.spirorbis.total)
for (i in c(1:nrow(spirorbis))) {
  spirorbis$diff[[i]] <- spirorbis[i, "Nb.spirorbis.total"] - spirorbis[i, "mean.x.200"]
}
spirorbis$diff <- abs(as.integer(spirorbis$diff))
spirorbis <- dplyr::arrange(spirorbis, desc(diff), mean.x.200)
(gonb_ilotstmichel_2015_04_18 <- dplyr::filter(spirorbis, site_year_month_day == "GONB_IlotStMichel.2015.04.18"))
rm(gonb_ilotstmichel_2015_04_18)
spirorbis <- dplyr::arrange(dplyr::filter(spirorbis, diff != 0 & mean.x.200 != 0), desc(diff))

# check it all in the qecbnew df

for (i in c(1:nrow(qecbnew))) {
    qecbnew$mean.x.200[[i]] <-
      #ifelse(qecbnew$Nb.spirorbis.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"),
      sum(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], na.rm = TRUE) / sum(!is.na(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")])) * 200
    #, qecbnew$Nb.spirorbis.total[[i]])
} # sum of only NAs/0 = NaN; so replace NaN by Na
qecbnew$mean.x.200 <- as.character(qecbnew$mean.x.200)

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$site_year_month_day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew_spirorbis$id_qecb, "_", qecbnew_spirorbis$site_year_month_day, "_", qecbnew_spirorbis$Type.Bloc, "_", qecbnew_spirorbis$Numéro.Bloc.échantillon, "_", qecbnew_spirorbis$Face))[, "mean.x.200"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.200[[i]] <- ifelse(qecbnew$mean.x.200[[i]] == "NaN", NA, qecbnew$mean.x.200[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$mean.x.200)))
qecbnew$mean.x.200 <- as.integer(qecbnew$mean.x.200)

qecbnew$Nb.spirorbis.total <- as.integer(qecbnew$Nb.spirorbis.total)
unique(qecbnew$Nb.spirorbis.total - qecbnew$mean.x.200)
table(qecbnew$Nb.spirorbis.total - qecbnew$mean.x.200)
qecbnew$Nb.spirorbis.total.diff <- abs((qecbnew$Nb.spirorbis.total - qecbnew$mean.x.200))
spirorbis_diff <- qecbnew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis_diff <- dplyr::arrange(spirorbis_diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis_diff <- dplyr::arrange(dplyr::filter(spirorbis_diff, Nb.spirorbis.total.diff != 0 & mean.x.200 != 0), desc(Nb.spirorbis.total.diff))

qecbnew$Nb.spirorbis.total <- ifelse(qecbnew$Nb.spirorbis.total.diff != 0 & qecbnew$mean.x.200 != 0, qecbnew$mean.x.200, qecbnew$Nb.spirorbis.total)
spirorbis_diff <- qecbnew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis_diff$Nb.spirorbis.total.diff <- abs(as.integer(spirorbis_diff$Nb.spirorbis.total.diff))
spirorbis_diff <- dplyr::arrange(spirorbis_diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis_diff <- dplyr::arrange(dplyr::filter(spirorbis_diff, Nb.spirorbis.total.diff != 0 & mean.x.200 != 0), desc(Nb.spirorbis.total.diff))
# ok, change made when data x 200 was not correct.

# finally, change NA by mean.x200 for spirorbis total
qecbnew$Nb.spirorbis.total <- as.character(qecbnew$Nb.spirorbis.total)
for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.spirorbis.total[[i]] <- ifelse(qecbnew$Nb.spirorbis.total[[i]] %in% c(NA, "NaN", "Inf", "-Inf"), sum(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], na.rm = TRUE) / sum(!is.na(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")])) * 200, qecbnew$Nb.spirorbis.total[[i]])
} # sum of only NAs/0 = NaN; so replace NaN by Na

sort(dplyr::filter(qecbnew, paste0(qecbnew$id_qecb, "_", qecbnew$site_year_month_day, "_", qecbnew$Type.Bloc, "_", qecbnew$Numéro.Bloc.échantillon, "_", qecbnew$Face) %in% paste0(qecbnew_spirorbis$id_qecb, "_", qecbnew_spirorbis$site_year_month_day, "_", qecbnew_spirorbis$Type.Bloc, "_", qecbnew_spirorbis$Numéro.Bloc.échantillon, "_", qecbnew_spirorbis$Face))[, "Nb.spirorbis.total"])

for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.spirorbis.total[[i]] <- ifelse(qecbnew$Nb.spirorbis.total[[i]] == "NaN", NA, qecbnew$Nb.spirorbis.total[[i]])
}
nrow(subset(qecbnew, is.na(qecbnew$Nb.spirorbis.total)))
qecbnew$Nb.spirorbis.total <- as.integer(qecbnew$Nb.spirorbis.total)

unique(qecbnew$Nb.spirorbis.total - qecbnew$Nb.spirorbis.total.ini)
table(qecbnew$Nb.spirorbis.total - qecbnew$Nb.spirorbis.total.ini)
qecbnew$Nb.spirorbis.total.diff <- abs(qecbnew$Nb.spirorbis.total - qecbnew$Nb.spirorbis.total.ini)
spirorbis_diff <- qecbnew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis_diff <- dplyr::arrange(spirorbis_diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis_diff <- dplyr::arrange(dplyr::filter(spirorbis_diff, Nb.spirorbis.total.diff != 0 & mean.x.200 != 0), desc(Nb.spirorbis.total.diff))
table(qecbnew$Nb.spirorbis.total.diff)
length(na.omit(qecbnew$Nb.spirorbis.total))
sum(is.na(qecbnew$Nb.spirorbis.total))
length(na.omit(qecbnew$Nb.spirorbis.total)) + sum(is.na(qecbnew$Nb.spirorbis.total))

qecbnew <- subset(qecbnew, select = -c(Nb.spirorbis.total.ini, mean.x.200, Nb.spirorbis.total.diff))

rm(qecbnew_spirorbis, spirorbis, spirobranchus_data, spirorbis_diff, i)


# dplyr::filter for abnormal data, based on histogram distribution of data

qecbnewhist_ <- qecbnew
ylab_ <- "fréquence"

hist_ <- qecbnewhist_[, c(
  "Type.Bloc",
  "Face",
  "X..algues.brunes",
  "Strate.algues.brunes",
  "X..algues.rouges",
  "Strate.algues.rouges",
  "X..algues.vertes",
  "Strate.algues.vertes",
  "X..Cladophora",
  "X..Lithophyllum",
  "X..Recouvrement.Sediment",
  #"Type.Sediment"                          ,
  "X..Roche.Nue",
  "Nb.Littorina.obtusata",
  "Nb.Gibbula.cineraria",
  "Nb.Gibbula.pennanti",
  "Nb.Gibbula.umbilicalis",
  "Nb.Phallusia.mamillata",
  "Nb.Tethya.aurantium",
  #"Nb.Spirobranchus.lamarckii.1B"          ,
  #"Nb.Spirobranchus.lamarckii.2B"          ,
  #"Nb.Spirobranchus.lamarckii.3B"          ,
  #"Nb.Spirobranchus.lamarckii.4B"          ,
  #"Nb.Spirobranchus.lamarckii.5B"          ,
  "Nb.Spirobranchus.lamarckii.total",
  #"Nb.spirorbis.1A"                        ,
  #"Nb.spirorbis.2A"                        ,
  #"Nb.spirorbis.3A"                        ,
  #"Nb.spirorbis.4A"                        ,
  #"Nb.spirorbis.5A"                        ,
  "Nb.spirorbis.total",
  "Nb.Crassostrea.gigas",
  "Nb.Ostrea.edulis",
  "X..Mytilus.sp.",
  "X..Hermelles",
  "X..Hydraires",
  "X..Eponges",
  "X..Ascidies.Coloniales",
  "X..Ascidies.Solitaires",
  "X..Bryozoaires.Dresses",
  "X..Balanes.Vivantes",
  #"Commentaires.Avant"                     ,
  "X..Surface.Accolement",
  #"Type.sustrat.observé"                   ,
  #"Commentaires"                           ,
  "Nb.Cancer.pagurus..Tourteau.",
  "Nb.Necora.puber..Etrille.",
  "Nb.Carcinus.maenas..Crabe.vert.",
  "Nb.Nucella.lapilus..Pourpre.",
  "Nb.Eriphia.verrucosa..Crabe.verruqueux.",
  "Nb.Octopus.vulgaris..Poulpe.",
  "Nb.Galathea..Galathées.",
  "Nb.Paracentrotus.lividus..Oursin.",
  "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
  "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
  "Nb.Haliotis.tuberculata..Ormeau.",
  "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
  "Nb.Littorina.littorea..Bigorneau.",
  "Nb.Xantho.pilipes..Xanthe.poilu.",
  "Nb.Mimachlamys.varia..Pétoncle.noir."
)]

par(mfrow = c(2, 3))

sapply(names(hist_[, c(3:ncol(hist_))]),
       function(cname) {
         print(hist(hist_[, c(3:ncol(hist_))][[cname]], main = "", xlab = cname, ylab = ylab_, breaks = length(unique(hist_[, c(3:ncol(hist_))][[cname]]))))
       })

par(mfrow = c(1, 1))

dplyr::filter(qecbnew, X..algues.brunes > 100)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "X..algues.brunes")]
qecbnew$X..algues.brunes <- ifelse(qecbnew$X..algues.brunes > 100, 100, qecbnew$X..algues.brunes)
dplyr::filter(qecbnew, X..algues.rouges > 100)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "X..algues.rouges")]
qecbnew$X..algues.rouges <- ifelse(qecbnew$X..algues.rouges > 100, 100, qecbnew$X..algues.rouges)
dplyr::filter(qecbnew, Nb.Phallusia.mamillata > 10)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.Phallusia.mamillata")]
dplyr::filter(qecbnew, Nb.Tethya.aurantium > 2)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.Tethya.aurantium")]
dplyr::filter(qecbnew, Nb.spirorbis.total > 15000)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.spirorbis.total")]
armo_ileplate <- dplyr::filter(qecbnew, Site == "ARMO_IlePlate" & date_fiche == "2015-10-29")
rm(armo_ileplate)
dplyr::filter(qecbnew, Nb.Nucella.lapilus..Pourpre. > 20)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "Nb.Nucella.lapilus..Pourpre.")]

rm(qecbnewhist_, ylab_, hist_)


## SCRIPT I - NAs <- 0 ; cfr previous comment makes no sense to have NA encoded when the presence of an organism is in reality = 0

# We are facing an issues with NA observations, because either they were not measured/counted, then they are effectively NAs; or these NAs = indeed "0"; but I cannot have NAs for variables that are included in the index determination, cfr if 5+0 = 5, 5+NA = NA; see for example site_year_month_day == "ARMO_Bilfot.2014.04.28", Nb.Spirobranchus.lamarckii.total is NA ...
# I theregore change these NAs by 0

# replace NAs by "0" for variables used in qecb determination
qecbnew[, c("X..algues.brunes",
          "X..algues.rouges",
          "X..Lithophyllum",
          "X..Cladophora",
          "Nb.Littorina.obtusata",
          "Nb.Gibbula.cineraria",
          "Nb.Gibbula.pennanti",
          "Nb.Gibbula.umbilicalis",
          "X..Eponges",
          "X..Ascidies.Coloniales",
          "X..Ascidies.Solitaires",
          "X..Bryozoaires.Dresses",
          "X..algues.vertes",
          "X..Roche.Nue",
          "Nb.spirorbis.total",
          "X..Balanes.Vivantes",
          "Nb.Spirobranchus.lamarckii.total",
          "X..Surface.Accolement")] <- lapply(qecbnew[,
               c("X..algues.brunes",
                 "X..algues.rouges",
                 "X..Lithophyllum",
                 "X..Cladophora",
                 "Nb.Littorina.obtusata",
                 "Nb.Gibbula.cineraria",
                 "Nb.Gibbula.pennanti",
                 "Nb.Gibbula.umbilicalis",
                 "X..Eponges",
                 "X..Ascidies.Coloniales",
                 "X..Ascidies.Solitaires",
                 "X..Bryozoaires.Dresses",
                 "X..algues.vertes",
                 "X..Roche.Nue",
                 "Nb.spirorbis.total",
                 "X..Balanes.Vivantes",
                 "Nb.Spirobranchus.lamarckii.total",
                 "X..Surface.Accolement")],
  function(x) replace(x, is.na(x), 0))

# and also replace NA for bivalve by 0 for EGMP and BASQ surveys cfr for accollement correction later on.

qecbnew$X..Mytilus.sp. <- ifelse((substr(qecbnew$Site, 1, 4) %in% c("EGMP", "BASQ")) & is.na(qecbnew$X..Mytilus.sp.), 0, qecbnew$X..Mytilus.sp.)
qecbnew$Nb.Crassostrea.gigas <- ifelse((substr(qecbnew$Site, 1, 4) %in% c("EGMP", "BASQ")) & is.na(qecbnew$Nb.Crassostrea.gigas), 0, qecbnew$Nb.Crassostrea.gigas)
qecbnew$Nb.Ostrea.edulis <- ifelse((substr(qecbnew$Site, 1, 4) %in% c("EGMP", "BASQ")) & is.na(qecbnew$Nb.Ostrea.edulis), 0, qecbnew$Nb.Ostrea.edulis)


# add a region variable
region <- rep(NA, nrow(qecbnew))
qecbnew <- tibble::add_column(qecbnew, region, .after = "Site_bis")
qecbnew$region <- ifelse(qecbnew$Site %in% c("EGMP_GroinCou", "EGMP_PasEmsembert",    "EGMP_BreeBains", "EGMP_PerreAntiochat", "EGMP_Chassiron", "BASQ_FlotsBleusZP", "BASQ_FlotsBleusZF"), "EGMP.BASQ", "Bretagne")
rm(region)
qecbnew <- dplyr::arrange(qecbnew, region, site_year_month_day, Type.Bloc, Numéro.Bloc.échantillon, Face)

# accolement function according to recent 'retournement'

## before I go further ahead, I have to correct for surface d'accollement for several variable for BM.FI !!

# not the same file name between script qecb script (qecbNew) and this script (qecbNew); doesn't matter, only appears here in the first dplyr::filter lines.

qecbnew <- tibble::add_column(qecbnew, terri_ = substr(qecbnew$Site, 1, 4), .after = "Site_bis")

qecbnew$X..Eponges_ini <- qecbnew$X..Eponges
qecbnew$X..Ascidies.Coloniales_ini <- qecbnew$X..Ascidies.Coloniales
qecbnew$X..Ascidies.Solitaires_ini <- qecbnew$X..Ascidies.Solitaires
qecbnew$X..Bryozoaires.Dresses_ini <- qecbnew$X..Bryozoaires.Dresses
qecbnew$X..Lithophyllum_ini <- qecbnew$X..Lithophyllum
qecbnew$X..Balanes.Vivantes_ini <- qecbnew$X..Balanes.Vivantes

df_bm_fs <- qecbnew %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face supérieure")
df_bm_fi <- qecbnew %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face inférieure")
df_bf <- qecbnew %>% dplyr::filter(Type.Bloc != "Bloc mobile")

`%notin%` <- Negate(`%in%`)

acco_fct <- function(var_) {

  df_bm_fi$var_cor.acco. <<- NA

  for (i in c(1:nrow(df_bm_fi))) {

    df_bm_fi$var_cor.acco.[[i]] <<- if (df_bm_fi$terri_[[i]] %notin% c("EGMP", "BASQ")) {
      ifelse(#df_$Couleur.dominante %in% c("Rouge", "Brune", "Brune-Rouge") ||
        df_bm_fs$Couleur.dominante[[i]] %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée"), df_bm_fi[i, var_] / (100 - df_bm_fi$X..Surface.Accolement[[i]]) * 100, df_bm_fi[i, var_])
    } else {
      ifelse(df_bm_fs$Couleur.dominante[[i]] %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée")
             & df_bm_fi$X..Surface.Accolement[[i]] != 0 # I have to use it in dplyr::filter this time as well for EGMP- BASQ (but not for Bretagne, although could be added, same result); identical/repeated measure for BM.FI and BM.FS
             & df_bm_fs$X..Mytilus.sp.[[i]] == 0, df_bm_fi[i, var_] / (100 - df_bm_fi$X..Surface.Accolement[[i]]) * 100, df_bm_fi[i, var_])
    }

  }

}

# I would only consider colors in c("Rouge", "Brune", "Brune-Rouge") for BM.FI correction [ and not the series c("Blanche-Brune", "Rouge", "Brune", "Blanche-Rouge", "Brune-Rouge", "Rouge-Verte", "Brune-Verte") ] ; and for BM.FS, the list c("Blanche", "Verte", "Colorée") => we do the correction for BM.FI accollement based on BM.FS color !!!


# apply acco_fct to BM.FI variables

apply_acco_fct <- function(var_) {



  show(sort(df_bm_fi[, var_], decreasing = TRUE, index.return = FALSE)[1:50])
  pre_ <- as.vector(df_bm_fi[, var_])
  acco_fct(var_)
  df_bm_fi <<- tibble::add_column(df_bm_fi, var_cor. = df_bm_fi$var_cor.acco., .after = var_)
  show(sort(df_bm_fi$var_cor., decreasing = TRUE, index.return = FALSE)[1:50])
  df_bm_fi$var_cor. <<- as.numeric(ifelse(as.character(df_bm_fi$var_cor.) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_bm_fi$var_cor.)))
  df_bm_fi$var_cor. <<- ifelse(df_bm_fi$var_cor. > 100, 100, df_bm_fi$var_cor.)
  show(sort(df_bm_fi$var_cor., decreasing = TRUE, index.return = FALSE)[1:50])
  show(length(na.omit(which(abs(as.vector(df_bm_fi$var_cor.) - pre_) != 0))) / na.omit(length(df_bm_fi$var_cor.)) * 100)
  par(mfrow = c(1, 3))
  hist(pre_, main = var_, xlab = "pre-corection")
  hist(df_bm_fi$var_cor., main = var_, xlab = "post-corection")
  hist(df_bm_fi[as.vector(which(abs(as.vector(df_bm_fi$var_cor.) - pre_) != 0)), var_], main = var_, xlab = "diff. post-pre != 0")
  par(mfrow = c(1, 1))
  df_bm_fi <<- df_bm_fi[, -which(names(df_bm_fi) %in% c(var_, "var_cor.acco."))]
  colnames(df_bm_fi)[colnames(df_bm_fi) == "var_cor."] <<- var_

  rm(pre_)

}

apply_acco_fct("X..Eponges")
apply_acco_fct("X..Ascidies.Coloniales")
apply_acco_fct("X..Ascidies.Solitaires")
apply_acco_fct("X..Bryozoaires.Dresses")
apply_acco_fct("X..Lithophyllum")
apply_acco_fct("X..Balanes.Vivantes")

qecbnew <- dplyr::bind_rows(df_bm_fs, df_bm_fi)
qecbnew <- dplyr::bind_rows(qecbnew, df_bf)

qecbnew <- dplyr::arrange(qecbnew, region, site_year_month_day, Type.Bloc, Numéro.Bloc.échantillon, Face)

# do remove some more data ...
# "FINS_Quemenes.2020.10.16", bad encoding, I let know Anna Capietto to make changes => was corrected normally, so unactivate next time I download ESTAMP data
qecbnew <- dplyr::filter(qecbnew, site_year_month_day != "FINS_Quemenes.2020.10.16")

# save the final qecbnew df_

saveRDS(qecbnew, "qecbnew.RDS")

ecology_input <- qecbnew

write.table(ecology_input, "Valeurs_stat.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")

## do calculate QECB values now

# create lists

qecb_val_qu_list <- vector("list", length(unique(qecbnew$site_year_month_day)))
qecb_val_list <- vector("list", length(unique(qecbnew$site_year_month_day)))

for (i in c(1:length(unique(qecbnew$site_year_month_day)))) {

  qecb_i <- qecbnew  %>% dplyr::filter(site_year_month_day == unique(qecbnew$site_year_month_day)[[i]])

  (terri_ <- unique(substr(qecb_i$Site, 1, 4)))

  nb. <- as.vector(unlist(intersect(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"], dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["Numéro.Bloc.échantillon"])))

  bloc_nb <- c(
    paste0("Bloc mobile", " - ", "face supérieure", " - ", nb.),
    paste0("Bloc mobile", " - ", "face inférieure", " - ", nb.),
    paste0(as.vector(unlist(dplyr::filter(qecb_i, Type.Bloc != "Bloc mobile")["Type.Bloc"])), " - ", as.vector(unlist(dplyr::filter(qecb_i, Type.Bloc != "Bloc mobile")["Face"])), " - ", as.vector(unlist(dplyr::filter(qecb_i, Type.Bloc != "Bloc mobile")["Numéro.Bloc.échantillon"])))
  )

  qecb_i$Bloc <- paste0(qecb_i$Type.Bloc, " - ", qecb_i$Face, " - ", qecb_i$Numéro.Bloc.échantillon)

  qecb_i <- qecb_i %>% subset(Bloc %in% bloc_nb)
    ## qebm_1


    # vfs_bm Bloc mobile

  {
  df_bm_fs <- qecb_i %>% dplyr::filter(qecb_i$Type.Bloc == "Bloc mobile" & qecb_i$Face == "face supérieure") # to keep a version of it for later on correction for accollement for BM FI
    df_ <- df_bm_fs
    df_ <- dplyr::arrange(df_, Numéro.Bloc.échantillon)

    a_bms <- df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora
    b_bms <-  df_$X..Lithophyllum
    c_bms <- df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis
    d_bms <- df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses
    e_bms <- df_$X..algues.vertes
    f_bms <- df_$X..Roche.Nue

    vfs_bm <- ((df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora)
      +  df_$X..Lithophyllum
      + (df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis)
      + (df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses)
    ) -
      (df_$X..algues.vertes
          +  df_$X..Roche.Nue
      )
    vfs_bm


    # vfi_bm Bloc mobile

    df_ <- qecb_i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face inférieure")
    df_ <- dplyr::arrange(df_, Numéro.Bloc.échantillon)

    df_bm_fi <- df_

    # accolement function according to recent 'retournement'

    `%notin%` <- Negate(`%in%`)

    acco_fct <- function(var_) {

      if (terri_ %notin% c("EGMP", "BASQ")) {
        ifelse(#df_$Couleur.dominante %in% c("Rouge", "Brune", "Brune-Rouge") ||
          df_bm_fs$Couleur.dominante %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée"), df_bm_fi[, var_] / (100 - df_bm_fi$X..Surface.Accolement) * 100, df_bm_fi[, var_])
      } else {
        ifelse(df_bm_fs$Couleur.dominante %in% c("Blanche", "Verte", "Blanche-Verte", "Colorée")
               & df_bm_fi$X..Surface.Accolement != 0 # I have to use it in dplyr::filter this time as well for EGMP- BASQ (but not for Bretagne, altough could be added, same result); identical/repeated measure for BM.FI and BM.FS
               & df_bm_fs$X..Mytilus.sp. == 0, df_bm_fi[, var_] / (100 - df_bm_fi$X..Surface.Accolement) * 100, df_bm_fi[, var_])
      }

    }

    # I would only consider colors in c("Rouge", "Brune", "Brune-Rouge") for BM.FI correction [ and not the series c("Blanche-Brune", "Rouge", "Brune", "Blanche-Rouge", "Brune-Rouge", "Rouge-Verte", "Brune-Verte") ] ; and for BM.FS, the list c("Blanche", "Verte", "Colorée") => we do the correction for BM.FI accollement based on BM.FS color !!!

    df_bm_fs$Couleur.dominante
    df_bm_fs$X..Mytilus.sp.

    df_[, "X..Eponges"]
    df_[, "X..Surface.Accolement"]
    df_[, "X..Eponges"] <- acco_fct("X..Eponges")
    df_[, "X..Eponges"] <- as.numeric(ifelse(as.character(df_[, "X..Eponges"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Eponges"])))
    df_[, "X..Eponges"] <- ifelse(df_[, "X..Eponges"] > 100, 100, df_[, "X..Eponges"])
    df_[, "X..Eponges"]

    df_[, "X..Ascidies.Coloniales"]
    df_[, "X..Surface.Accolement"]
    df_[, "X..Ascidies.Coloniales"] <- acco_fct("X..Ascidies.Coloniales")
    df_[, "X..Ascidies.Coloniales"] <- as.numeric(ifelse(as.character(df_[, "X..Ascidies.Coloniales"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Ascidies.Coloniales"])))
    df_[, "X..Ascidies.Coloniales"] <- ifelse(df_[, "X..Ascidies.Coloniales"] > 100, 100, df_[, "X..Ascidies.Coloniales"])
    df_[, "X..Ascidies.Coloniales"]

    df_[, "X..Ascidies.Solitaires"]
    df_[, "X..Surface.Accolement"]
    df_[, "X..Ascidies.Solitaires"] <- acco_fct("X..Ascidies.Solitaires")
    df_[, "X..Ascidies.Solitaires"] <- as.numeric(ifelse(as.character(df_[, "X..Ascidies.Solitaires"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Ascidies.Solitaires"])))
    df_[, "X..Ascidies.Solitaires"] <- ifelse(df_[, "X..Ascidies.Solitaires"] > 100, 100, df_[, "X..Ascidies.Solitaires"])
    df_[, "X..Ascidies.Solitaires"]

    df_[, "X..Bryozoaires.Dresses"]
    df_[, "X..Surface.Accolement"]
    df_[, "X..Bryozoaires.Dresses"] <- acco_fct("X..Bryozoaires.Dresses")
    df_[, "X..Bryozoaires.Dresses"] <- as.numeric(ifelse(as.character(df_[, "X..Bryozoaires.Dresses"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Bryozoaires.Dresses"])))
    df_[, "X..Bryozoaires.Dresses"] <- ifelse(df_[, "X..Bryozoaires.Dresses"] > 100, 100, df_[, "X..Bryozoaires.Dresses"])
    df_[, "X..Bryozoaires.Dresses"]

    df_[, "X..Lithophyllum"]
    df_[, "X..Surface.Accolement"]
    df_[, "X..Lithophyllum"] <- acco_fct("X..Lithophyllum")
    df_[, "X..Lithophyllum"] <- as.numeric(ifelse(as.character(df_[, "X..Lithophyllum"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Lithophyllum"])))
    df_[, "X..Lithophyllum"] <- ifelse(df_[, "X..Lithophyllum"] > 100, 100, df_[, "X..Lithophyllum"])
    df_[, "X..Lithophyllum"]

    d_bmi <- df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses
    b_bmi <- df_$X..Lithophyllum
    a_bmi <- df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora
    c_bmi <- df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis
    e_bmi <- df_$X..algues.vertes
    f_bmi <- df_$X..Roche.Nue

    vfi_bm <- ((df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses)
      +  df_$X..Lithophyllum
    ) -
      ((df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora)
         + (df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis)
         +  df_$X..algues.vertes
         +  df_$X..Roche.Nue
      )
    vfi_bm

    # vfsi_bm Bloc mobile

    df_ <- qecb_i %>% dplyr::filter(Type.Bloc == "Bloc mobile")
    df_ <- dplyr::arrange(df_, desc(Face), Numéro.Bloc.échantillon)
    num_bloc <- as.vector(sort(unique(df_$Numéro.Bloc.échantillon)))

    g_bmsi <- NA
    h_bmsi <- NA
    l_bmsi <- NA

    df_bm_fs <- dplyr::filter(df_, Face == "face inférieure")
    df_bm_fi <- dplyr::filter(df_, Face == "face inférieure")

    df_bm_fs$Couleur.dominante
    df_bm_fs$X..Mytilus.sp.

    df_[, "X..Balanes.Vivantes"]
    df_[, "X..Surface.Accolement"]
    df_ <- dplyr::mutate(df_, row.nb = dplyr::row_number())
    dplyr::filter(df_, Face == "face inférieure")["row.nb"]
    df_[c(dplyr::filter(df_, Face == "face inférieure")[1, "row.nb"]:unlist(tail(dplyr::filter(df_, Face == "face inférieure"), n = 1)["row.nb"])), "X..Balanes.Vivantes"] <- acco_fct("X..Balanes.Vivantes")
    df_[, "X..Balanes.Vivantes"] <- as.numeric(ifelse(as.character(df_[, "X..Balanes.Vivantes"]) %in% c(NA, "NaN", "-Inf", "Inf"), "0", as.character(df_[, "X..Balanes.Vivantes"])))
    df_[, "X..Balanes.Vivantes"] <- ifelse(df_[, "X..Balanes.Vivantes"] > 100, 100, df_[, "X..Balanes.Vivantes"])
    df_[, "X..Balanes.Vivantes"]

    for (k in c(1:length(na.omit(num_bloc)))) {

      j_ <- num_bloc[k]

      gin_ <- unname(unlist(
        (dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["Nb.spirorbis.total"]
         + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["Nb.spirorbis.total"]
        ) / 1000))

      g_bmsi <<- c(g_bmsi, gin_)

      hin_ <- unname(unlist(
        (dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["X..Balanes.Vivantes"]
         + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["X..Balanes.Vivantes"]
        ) / 100))

      h_bmsi <<- c(h_bmsi, hin_)

      lin_ <- unname(unlist(
        (dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
         + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
        ) / 100))

      l_bmsi <<- c(l_bmsi, lin_) # To avoid error message "Error in I <<- c(I, IIn.) : cannot change value of locked binding for 'I'"

    }

    g_bmsi <- g_bmsi[2:length(g_bmsi)]
    g_bmsi
    h_bmsi <- h_bmsi[2:length(h_bmsi)]
    h_bmsi
    i_bmsi <- l_bmsi[2:length(l_bmsi)]
    i_bmsi

    vfsi_bm <- NA

    for (k in c(1:length(na.omit(num_bloc)))) {

      j_ <- num_bloc[k]

      vfsin_ <- unname(unlist(
        ((dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["Nb.spirorbis.total"]
           + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["Nb.spirorbis.total"]
        ) / 1000)
        -
          (((dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["X..Balanes.Vivantes"]
               + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["X..Balanes.Vivantes"]
            ) / 100)
            + ((dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face supérieure")["Nb.Spirobranchus.lamarckii.total"]
                 + dplyr::filter(df_, Numéro.Bloc.échantillon == j_ & Face == "face inférieure")["Nb.Spirobranchus.lamarckii.total"]
            ) / 100)
          )
      ))

      vfsi_bm <<- c(vfsi_bm, vfsin_)

    }

    vfsi_bm <- vfsi_bm[2:length(vfsi_bm)]
    vfsi_bm


    # qebm_1

    (qebm_1 <- vfs_bm + vfi_bm + vfsi_bm)

    ## qebm_2


    # vrfs_bf moyenne Bloc fixé ; = VDRmoyenne in excel file

    df_ <- qecb_i %>% dplyr::filter(Type.Bloc %in% c("Bloc fixé", "Roche en place"))
    df_ <- dplyr::arrange(df_, Numéro.Bloc.échantillon)

    a_bf <- df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora
    b_bf <- df_$X..Lithophyllum
    c_bf <- df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis
    d_bf <- df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses
    e_bf <- df_$X..algues.vertes
    f_bf <- df_$X..Roche.Nue

    a_bf <- c(a_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(a_bf)))
    b_bf <- c(b_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(b_bf)))
    c_bf <- c(c_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(c_bf)))
    d_bf <- c(d_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(d_bf)))
    e_bf <- c(e_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(e_bf)))
    f_bf <- c(f_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(f_bf)))

    vrfs_bf <- ((df_$X..algues.brunes + df_$X..algues.rouges + df_$X..Cladophora)
      +  df_$X..Lithophyllum
      + (df_$Nb.Littorina.obtusata + df_$Nb.Gibbula.cineraria + df_$Nb.Gibbula.pennanti + df_$Nb.Gibbula.umbilicalis)
      + (df_$X..Eponges + df_$X..Ascidies.Coloniales + df_$X..Ascidies.Solitaires + df_$X..Bryozoaires.Dresses)
    ) -
      (df_$X..algues.vertes
          +  df_$X..Roche.Nue
      ) # different from Pauline, check with her
    vrfs_bf
    vrfs_bf <- c(vrfs_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(vrfs_bf)))

    # (G - (H + I)) Bloc fixé & Roche en place

    g_bf <- df_$Nb.spirorbis.total / 1000
    h_bf <- df_$X..Balanes.Vivantes / 100
    i_bf <- df_$Nb.Spirobranchus.lamarckii.total / 100

    g_bf <- c(g_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(g_bf)))
    h_bf <- c(h_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(h_bf)))
    i_bf <- c(i_bf, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(i_bf)))

    `(G - (H + I))BF` <- (df_$Nb.spirorbis.total / 1000
      - (df_$X..Balanes.Vivantes / 100 + df_$Nb.Spirobranchus.lamarckii.total / 100)
    )
    `(G - (H + I))BF`
    `(G - (H + I))BF` <- c(`(G - (H + I))BF`, rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(`(G - (H + I))BF`)))


    # vrfs_bf.moy

    (mean(vrfs_bf + `(G - (H + I))BF`, na.rm = TRUE) -> vrfs_bf.moy)


    # (G - (H + I)S.BM) Bloc mobile face supérieure

    df_ <- qecb_i %>% dplyr::filter(Type.Bloc == "Bloc mobile" & Face == "face supérieure")
    df_ <- dplyr::arrange(df_, Numéro.Bloc.échantillon)

    g_bms <- df_$Nb.spirorbis.total / 1000
    h_bms <- df_$X..Balanes.Vivantes / 100
    i_bms <- df_$Nb.Spirobranchus.lamarckii.total / 100

    `(G - (H + I))S.BM` <- (df_$Nb.spirorbis.total / 1000
      - (df_$X..Balanes.Vivantes / 100 + df_$Nb.Spirobranchus.lamarckii.total / 100)
    )
    `(G - (H + I))S.BM`


    # vrfs_bm

    (vrfs_bm <- vfs_bm + `(G - (H + I))S.BM`)


    # vrfs_bm_moy

    (vrfs_bm_moy <- mean(vrfs_bm#[val.vrfs_bm_moy.i]
          , na.rm = TRUE))


    # ||vrfs_bm_moy/vrfs_bf.moy||

    (`||vrfs_bm_moy/vrfs_bf.moy||` <- abs(mean(vrfs_bm#[val.vrfs_bm_moy.i]
              , na.rm = TRUE)/vrfs_bf.moy))


    # qebm_2

    (qebm_2 <- qebm_1 * `||vrfs_bm_moy/vrfs_bf.moy||`)


    ## QECB

    (QECB <- mean(qebm_2#[val.qecb_i]
          , na.rm = TRUE))

  }

  qecb_val_qu_list[[i]] <- data.frame(id_qecb = rep(unique(qecb_i$id_qecb), length(qebm_2)),
    Site = rep(unique(qecb_i$Site), length(qebm_2)),
    Site_bis = rep(unique(qecb_i$Site_bis), length(qebm_2)),
    site_year_month_day = rep(unique(qecb_i$site_year_month_day), length(qebm_2)),
    Boulder.nb_bms = sort(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"])[, 1]),
    Boulder.nb_bmi = sort(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["Numéro.Bloc.échantillon"])[, 1]),
    Boulder.nb_bf = c(sort(unique(dplyr::filter(qecb_i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Numéro.Bloc.échantillon"])[, 1]), rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["Numéro.Bloc.échantillon"])[, 1]) - length(unique(dplyr::filter(qecb_i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["Numéro.Bloc.échantillon"])[, 1]))),
    quadrat.bmS = sort(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face supérieure")["quadrat_bis"][, 1]),
    quadrat.bmI = sort(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile" & Face == "face inférieure")["quadrat_bis"][, 1]),
    quadrat.bf = c(sort(dplyr::filter(qecb_i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["quadrat_bis"][, 1]), rep(NA, length(unique(dplyr::filter(qecb_i, Type.Bloc == "Bloc mobile")["Numéro.Bloc.échantillon"])[, 1]) - length(dplyr::filter(qecb_i, Type.Bloc %in% c("Bloc fixé", "Roche en place"))["quadrat_bis"][, 1]))),
    a_bms,
    b_bms,
    c_bms,
    d_bms,
    e_bms,
    f_bms,
    a_bmi,
    b_bmi,
    c_bmi,
    d_bmi,
    e_bmi,
    f_bmi,
    g_bmsi,
    h_bmsi,
    i_bmsi,
    a_bf,
    b_bf,
    c_bf,
    d_bf,
    e_bf,
    f_bf,
    g_bf,
    h_bf,
    i_bf,
    g_bms,
    h_bms,
    i_bms,
    vfs_bm,
    vfi_bm,
    vfsi_bm,
    qebm_1,
    vrfs_bf,
    `(G - (H + I))S.BM`,
    `(G - (H + I))BF`,
    vrfs_bm,
    qebm_2)

  qecb_val_list[[i]] <- data.frame(id_qecb = unique(qecb_i$id_qecb),
    Site = unique(qecb_i$Site),
    Site_bis = unique(qecb_i$Site_bis),
    site_year_month_day = unique(qecb_i$site_year_month_day),
    vrfs_bm_moy,
    vrfs_bf.moy,
    `||vrfs_bm_moy/vrfs_bf.moy||`,
    QECB)

  rm(qecb_i)
  rm(df_bm_fs, df_bm_fi)
  rm("(G - (H + I))BF", "(G - (H + I))S.BM", "||vrfs_bm_moy/vrfs_bf.moy||", "a_bf", "a_bmi", "a_bms", "b_bf", "b_bmi", "b_bms", "bloc_nb", "c_bf", "c_bmi", "c_bms", "d_bf", "d_bmi", "d_bms", "e_bf", "e_bmi", "e_bms", "f_bf", "f_bmi", "f_bms", "g_bf", "g_bms", "g_bmsi", "gin_", "h_bf", "h_bms", "h_bmsi", "hin_", "i", "i_bf", "i_bms", "i_bmsi", "j_", "k", "l_bmsi", "lin_", "nb.", "num_bloc", "qebm_1", "qebm_2", "QECB", "vfi_bm", "vfs_bm", "vfsi_bm", "vfsin_", "vrfs_bf", "vrfs_bf.moy", "vrfs_bm", "vrfs_bm_moy", "terri_")

}

qecb_val_qu_ <- do.call("rbind", qecb_val_qu_list)
qecb_val_qu_ <- dplyr::arrange(qecb_val_qu_, site_year_month_day, Boulder.nb_bms)
Date <- as.Date(stringr::str_sub(qecb_val_qu_$site_year_month_day,-10,-1), format = "%Y.%m.%d", origin = "1970-01-01")
qecb_val_qu_ <- tibble::add_column(qecb_val_qu_, Date, .after = "Site_bis")
rm(Date)

qebm_2_list <- vector("list", length(unique(qecb_val_qu_$site_year_month_day)))

for (i in c(1:length(unique(qecb_val_qu_$site_year_month_day)))) {

qebm_2_i <- qecb_val_qu_ %>% dplyr::filter(site_year_month_day == unique(qecb_val_qu_$site_year_month_day)[[i]])

  qebm_2_bis <- qebm_2_i$qebm_1 *
  abs((mean(((qebm_2_i$a_bms
      + qebm_2_i$b_bms
      + qebm_2_i$c_bms
      + qebm_2_i$d_bms)
    -
      (qebm_2_i$e_bms
      + qebm_2_i$f_bms)
    )
    +
    (qebm_2_i$g_bms
     - (qebm_2_i$h_bms
        + qebm_2_i$i_bms)
    )
    , na.rm = TRUE)
  )
  /
  (mean(
    (
    (qebm_2_i$a_bf
     + qebm_2_i$b_bf
     + qebm_2_i$c_bf
     + qebm_2_i$d_bf) 
    -
    (qebm_2_i$e_bf
     + qebm_2_i$f_bf)
    )
    +
    (qebm_2_i$g_bf 
     -
    (qebm_2_i$h_bf
     + qebm_2_i$i_bf)
    )
    , na.rm = TRUE)
  )
  )

qebm_2_list[[i]] <- data.frame(site_year_month_day = unique(qebm_2_i$site_year_month_day), qebm_2_bis)

rm(i, qebm_2_i, qebm_2_bis)

}

qecb_val_qu_[, ncol(qecb_val_qu_) + 1] <- do.call("rbind", qebm_2_list)[2]

qecb_val_ <- do.call("rbind", qecb_val_list)
qecb_val_ <- dplyr::arrange(qecb_val_, site_year_month_day)
Date <- as.Date(stringr::str_sub(qecb_val_$site_year_month_day, -10, -1), format = "%Y.%m.%d", origin = "1970-01-01")
qecb_val_ <- tibble::add_column(qecb_val_, Date, .after = "Site_bis")
rm(Date)

rm(list = ls()[!ls() %in% c("fiche", "qecb", "qecbnew", "qecb_val_qu_")])

qecb_val_qu_ <- tidyr::separate(qecb_val_qu_, Date, c("Annee", "Mois", "Jour"), remove = FALSE)
qecb_val_qu_$Annee <- as.integer(qecb_val_qu_$Annee)
qecb_val_qu_$Mois <- as.integer(qecb_val_qu_$Mois)
qecb_val_qu_$Jour <- as.integer(qecb_val_qu_$Jour)

dplyr::filter(qecb_val_qu_, qebm_2 %in% c("Inf", "NaN"))

qecb_val_qu_nan <- qecb_val_qu_
qecb_val_qu_nan$qebm_2 <- ifelse(qecb_val_qu_nan$qebm_2 %in% c("-Inf", "NaN"), NA, qecb_val_qu_nan$qebm_2)

qecb_val_qu_stat_ <- qecb_val_qu_nan %>% dplyr::group_by(id_qecb, Site, Site_bis, Annee, Mois, Jour) %>% dplyr::summarize(qecb.moy = mean(qebm_2, na.rm = TRUE), qecb.et = sd(qebm_2, na.rm = TRUE), qecb.med = median(qebm_2, na.rm = TRUE), qecb.min = min(qebm_2, na.rm = TRUE), qecb.max = max(qebm_2, na.rm = TRUE), nb. = dplyr::n(), nb.notNa = sum(!is.na(qebm_2)))

Date <- as.Date(paste0(qecb_val_qu_stat_$Annee, "-", qecb_val_qu_stat_$Mois, "-", qecb_val_qu_stat_$Jour), origin = "1970-01-01")
qecb_val_qu_stat_ <- tibble::add_column(qecb_val_qu_stat_, Date, .after = "Site_bis")
rm(Date)

qecb_val_qu_stat_ <- as.data.frame(qecb_val_qu_stat_)
indic <- qecb_val_qu_stat_
saveRDS(qecb_val_qu_stat_, "qecb_val_qu_stat.RDS")


survey_list <- vector("list", length(unique(qecb_val_qu_$site_year_month_day)))

for (i in c(1:length(unique(qecb_val_qu_$site_year_month_day)))) {

  qecb_i <- qecb_val_qu_  %>% dplyr::filter(site_year_month_day == unique(qecb_val_qu_$site_year_month_day)[[i]])

  survey_list[[i]] <- data.frame(
    site_year_month_day = rep(unique(qecb_i$site_year_month_day), nrow(qecb_i)),
    survey_nb = rep(i, nrow(qecb_i))
  )

}

survey <- do.call("rbind", survey_list)

qecb_val_qu_ <- tibble::add_column(qecb_val_qu_, survey_nb = survey$survey_nb, .after = "site_year_month_day")
indic_full <- qecb_val_qu_
rm(i, survey_list, survey, qecb_i)

survey_nb <- c(1:nrow(qecb_val_qu_))
qecb_val_ <- tibble::add_column(qecb_val_qu_, survey_nb, .after = "site_year_month_day")

rm(survey_nb, qecb_val_qu_nan)

saveRDS(qecb_val_, "qecb_val.RDS")
saveRDS(qecb_val_qu_, "qecb_val_qu.RDS")


## Plots qecb

qecb_val_qu_nan <- qecb_val_qu_
qecb_val_qu_stat_nan <- qecb_val_qu_stat_

`%notin%` <- Negate(`%in%`)

qecb_val_qu_nan$qebm_2 <- ifelse(qecb_val_qu_nan$qebm_2 %in% c("-Inf", "NaN"), NA, qecb_val_qu_nan$qebm_2)

qecb_val_qu_stat_nan[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")] <- ifelse(qecb_val_qu_stat_nan[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")] %in% c("-Inf", "NaN"), NA, qecb_val_qu_stat_nan[, c("qecb.moy", "qecb.et", "qecb.med", "qecb.min", "qecb.max")])



for (i in c(1:length(unique(qecb_val_qu_stat_nan$Site)))) {

  df1 <- dplyr::filter(qecb_val_qu_stat_nan, Site == unique(qecb_val_qu_stat_nan$Site)[i])


  xmin_ <- as.Date(ifelse(min(df1$Annee) >= 2014, "2014-01-01", paste0(min(df$Annee), "-01-01")), origin = "1970-01-01")
  xmax_ <- as.Date(ifelse(max(df1$Annee) <= 2017, "2018-01-01", #paste0(max(qecb_val_eg$Annee)+1,
                          "2023-01-01")
                   #)
                   , origin = "1970-01-01")

  png(paste0("old_qecb_", unique(qecb_val_qu_stat_nan$Site), ".png"))
  plot(qecb_val_qu_stat_nan$Date, qecb_val_qu_stat_nan$qecb.med, xlim = c(xmin_, xmax_), ylim = c(-360, 360), pch = 19, cex = 1, main = unique(df1$Site_bis), xlab = "Année",
       ylab = "QECB", col = "grey")
  points(df1$Date, df1$qecb.med, pch = 19, cex = 1.5)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.max, code = 3, angle = 90, length = 0.00)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.min, code = 3, angle = 90, length = 0.00)

  abline(h = c(-216, -72, 72, 216), lty = "dashed")
  text(xmax_, -288, "1")
  text(xmax_, -146, "2")
  text(xmax_, 0, "3")
  text(xmax_, 146, "4")
  text(xmax_, 288, "5")

}

# New quality scale based on quartiles

dt_ <- dplyr::filter(qecb_val_qu_nan, qebm_2 >= -360)
dt_ <- dplyr::filter(dt_, qebm_2 <= 360)
dt_bis <- dplyr::filter(dt_, qebm_2 >= quantile(dt_$qebm_2, c(0.05), na.rm = TRUE))
dt_bis <- dplyr::filter(dt_bis, qebm_2 <= quantile(dt_bis$qebm_2, c(0.95), na.rm = TRUE))

one <- round(mean(unlist(dplyr::filter(dt_bis, qebm_2 <= quantile(dt_bis$qebm_2, 0.25, na.rm = TRUE))["qebm_2"])), digits = 0)
two <- round(mean(unlist(dplyr::filter(dt_bis, qebm_2 > quantile(dt_bis$qebm_2, 0.25, na.rm = TRUE) & qebm_2 <= quantile(dt_bis$qebm_2, 0.5, na.rm = TRUE))["qebm_2"])), digits = 0)
three <- round(mean(unlist(dplyr::filter(dt_bis, qebm_2 > quantile(dt_bis$qebm_2, 0.5, na.rm = TRUE) & qebm_2 <= quantile(dt_bis$qebm_2, 0.75, na.rm = TRUE))["qebm_2"])), digits = 0)
four <- round(mean(unlist(dplyr::filter(dt_bis, qebm_2 > quantile(dt_bis$qebm_2, 0.75, na.rm = TRUE))["qebm_2"])), digits = 0)


# I have unactivated the model line drawing because aberant for some sites with bad qecb values
for (i in c(1:length(unique(qecb_val_qu_stat_nan$Site)))) {

  df1 <- dplyr::filter(qecb_val_qu_stat_nan, Site == unique(qecb_val_qu_stat_nan$Site)[i])

  xmin_ <- as.Date(ifelse(min(df1$Annee) >= 2014, "2014-01-01", paste0(min(df$Annee), "-01-01")), origin = "1970-01-01")
  xmax_ <- as.Date(ifelse(max(df1$Annee) <= 2017, "2018-01-01", #paste0(max(qecb_val_eg$Annee)+1,
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")

  ymin_ <- ifelse(min(df1$qecb.med, na.rm = TRUE) < -70, -360, -70)
  ymax_ <- ifelse(max(df1$qecb.med, na.rm = TRUE) > 200, 360, 200)

  png(paste0("new_qecb_", unique(qecb_val_qu_stat_nan$Site), ".png"))
  plot(qecb_val_qu_stat_nan$Date, qecb_val_qu_stat_nan$qecb.med, xlim = c(xmin_, xmax_), ylim = c(ymin_, ymax_), pch = 19, main = "", xlab = "", ylab = "", type = "n", axes = FALSE)

  rect(as.Date("2013-01-01", origin = "1970-01-01"), -400, as.Date("2023-01-01", origin = "1970-01-01"), one, col = "red", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), one, as.Date("2023-01-01", origin = "1970-01-01"), two, col = "orange", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), two, as.Date("2023-01-01", origin = "1970-01-01"), three, col = "yellow", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), three, as.Date("2023-01-01", origin = "1970-01-01"), four, col = "olivedrab", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), four, as.Date("2023-01-01", origin = "1970-01-01"), 400, col = "blue", border = NA)

  par(new = TRUE)
  plot(qecb_val_qu_stat_nan$Date, qecb_val_qu_stat_nan$qecb.med, xlim = c(xmin_, xmax_), ylim = c(ymin_, ymax_), pch = 19, cex = 1, main = unique(df1$Site_bis), xlab = "Année",
       ylab = "QECB", col = "grey")
  points(df1$Date, df1$qecb.med, pch = 19, cex = 1.5)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.max, code = 3, angle = 90, length = 0.00)
  arrows(df1$Date, df1$qecb.med, df1$Date, df1$qecb.min, code = 3, angle = 90, length = 0.00)

}
rm(df1, dt_, dt_bis, four, i, one, three, two, xmax_, xmin_, ymax_, ymin_)
args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    report <- args[5]
    loop_file <- source(args[6])
}

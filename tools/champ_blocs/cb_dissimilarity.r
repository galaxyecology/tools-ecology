# author: "Jonathan Richir"
# date: "01 October 2022"


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
library(dplyr, conflicts = "warn")
#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

#####Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    fiche_val <- args[1]
    input_data <- args[2]
    choice <- args[3]
    choice_date <- as.numeric(args[4])
}

#############################################################
#                                                           #
#               Loading and cleaning data                   #
#                                                           #
#############################################################
# load qecb data

qecb <- read.csv2(input_data, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

# import csv files ficheterrain

fiche <- read.csv2(fiche_val, header = TRUE, fileEncoding = "Latin1") # fileEncoding = "Latin1",  cfr é in variable names

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

qecb <- tibble::add_column(qecb, Site = qecb$zone.habitat, .after = "ID.Fiche")

for (x in seq_along(qecb$Site)) {
  if (grepl(pattern = "Locmariaquer", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "GDMO_Locmariaquer"
 } else if (grepl(pattern = "Beg Lann", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "GDMO_BegLann"
 } else if (grepl(pattern = "Plateau du Four", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FOUR_PlateauFour"
 } else if (grepl(pattern = "Grouin", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "EGMP_GroinCou"
 } else if (grepl(pattern = "Ensembert", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "EGMP_PasEmsembert"
 } else if (grepl(pattern = "Brée-les-Bains", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "EGMP_BreeBains"
 } else if (grepl(pattern = "Antiochat", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "EGMP_PerreAntiochat"
 } else if (grepl(pattern = "Chassiron", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "EGMP_Chassiron"
 } else if (grepl(pattern = "zone p", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "BASQ_FlotsBleusZP"
 } else if (grepl(pattern = "zone f", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "BASQ_FlotsBleusZF"
 } else if (grepl(pattern = "Saint-Michel", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "GONB_IlotStMichel"
 } else if (grepl(pattern = "Quéménès", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_Quemenes"
 } else if (grepl(pattern = "Goulenez", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_SeinGoulenez"
 } else if (grepl(pattern = "Kilaourou", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_SeinKilaourou"
 } else if (grepl(pattern = "Verdelet", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "ARMO_Verdelet"
 } else if (grepl(pattern = "Piégu", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "ARMO_Piegu"
 } else if (grepl(pattern = "Bilfot", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "ARMO_Bilfot"
 } else if (grepl(pattern = "Plate", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "ARMO_IlePlate"
 } else if (grepl(pattern = "Perharidy", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "PDMO_Perharidy"
 } else if (grepl(pattern = "Keraliou", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "BRES_Keraliou"
 } else if (grepl(pattern = "Mousterlin", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_Mousterlin"
 } else if (grepl(pattern = "Nicolas", qecb$Site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_StNicolasGlenan"
 }
  if (grepl(pattern = "Roz", qecb$site[x]) == TRUE) {
    qecb$Site[x] <- "FINS_AnseRoz"
}
}

# Name for report/plot

qecb <- tibble::add_column(qecb, Site_bis = qecb$Site, .after = "Site")

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
qecb$Site_bis <- ifelse(qecb$Site == "FINS_AnseRoz", "Pointe de l'Anse du Roz", qecb$Site_bis)

## change some variables to factor

# change 'X..' variables that are indeed % to numeric; https://stackoverflow.com/questions/59410939/apply-function-to-all-variables-with-string-in-name
ix <- grep("^X..", names(qecb))
qecb[ix] <- lapply(qecb[ix], as.numeric)
rm(ix)


## save the final, complete qecb df_

qecb <- qecb[, c(72:107, 1:71)]

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
     "loop nb" = c(1:seq_along(unique(qecbnew$site_year_month_day))))

# dplyr::filter for df that makes problem, then eventually correct in the dataframe for wrong coding; brackets (xx) for nb because will change when qecb df_ enlarged.
# these listed boulder field survey error when highlighted when running the loop, that ran into an error ; it was a step by step procedure with solving one listed observation after another when issues appeared. Surely not the best way to proceed, maybe better just to skip these surveys (site + date), but in the present case I wanted to keep most of the observations, therefore I corrected them manually whenever needed.

# list nb (28) - EGMP_BreeBains.2016.04.06
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_La Bree_20160406_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-LaBreeB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_La Bree_20160406_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-LaBreeB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)

# list nb 33 - EGMP_PerreAntiochat.2016.04.07
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_PerAnt_20160407_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-PerAntB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_PerAnt_20160407_VImport.xlsx" & qecbnew$Référence.bloc == "avr16-PerAntB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)

# list nb 37 - EGMP_Chassiron.2016.03.09
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Chassiron_20160309&10_VImport.xlsx" & qecbnew$Référence.bloc == "mars16-ChassB9sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Chassiron_20160309&10_VImport.xlsx" & qecbnew$Référence.bloc == "mars16-ChasB10sup", "face supérieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)

# list nb 76 - ARMO_Verdelet.2015.03.23
qecbnew$Face <- as.character(qecbnew$Face)
qecbnew$Face <- ifelse(qecbnew$ID.Fiche == "BDD_IVR&QECB_Verdelet_20150323_VImport.xlsx" & qecbnew$Référence.bloc == "mar15-VerB10inf", "face inférieure", qecbnew$Face)
qecbnew$Face <- as.factor(qecbnew$Face)

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


qecbnew_spirobranchus <- (dplyr::filter(qecbnew, Nb.Spirobranchus.lamarckii.total %in% c(NA, "NaN", "Inf", "-Inf")))
qecbnew_spirobranchus[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")] <- sapply(qecbnew_spirobranchus[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], as.character)
(spirobranchus_data <- subset(qecbnew_spirobranchus, !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.1B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.2B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.3B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.4B) || !is.na(qecbnew_spirobranchus$Nb.Spirobranchus.lamarckii.5B))[, c("site_year_month_day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")])

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
spirobranchus <- subset(qecbnew, !is.na(qecbnew$Nb.Spirobranchus.lamarckii.1B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.2B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.3B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.4B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.5B) & !is.na(qecbnew$Nb.Spirobranchus.lamarckii.total))[, c("site_year_month_day", "Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total")]

for (i in c(1:nrow(spirobranchus))) {
  spirobranchus$mean.x.100[[i]] <- sum(spirobranchus[i, c(2:6)], na.rm = TRUE) / sum(!is.na(spirobranchus[i, c(2:6)])) * 100
}

spirobranchus$mean.x.100 <- unlist(spirobranchus$mean.x.100)
spirobranchus$Nb.Spirobranchus.lamarckii.total <- as.numeric(spirobranchus$Nb.Spirobranchus.lamarckii.total)
for (i in c(1:nrow(spirobranchus))) {
  spirobranchus$diff[[i]] <- spirobranchus[i, "Nb.Spirobranchus.lamarckii.total"] - spirobranchus[i, "mean.x.100"]
}

spirobranchus$diff <- abs(as.integer(spirobranchus$diff))
spirobranchus <- dplyr::arrange(spirobranchus, desc(diff), mean.x.100)
spirobranchus <- dplyr::arrange(dplyr::filter(spirobranchus, diff != 0 & mean.x.100 != 0), desc(diff))

# check it all in the qecbnew df

for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.100[[i]] <-
    sum(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")], na.rm = TRUE) / sum(!is.na(qecbnew[i, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B")])) * 100
} # sum of only NAs/0 = NaN; so replace NaN by Na
qecbnew$mean.x.100 <- as.character(qecbnew$mean.x.100)


for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.100[[i]] <- ifelse(qecbnew$mean.x.100[[i]] == "NaN", NA, qecbnew$mean.x.100[[i]])
}
qecbnew$mean.x.100 <- as.integer(qecbnew$mean.x.100)

qecbnew$Nb.Spirobranchus.lamarckii.total <- as.integer(qecbnew$Nb.Spirobranchus.lamarckii.total)
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


for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] <- ifelse(qecbnew$Nb.Spirobranchus.lamarckii.total[[i]] == "NaN", NA, qecbnew$Nb.Spirobranchus.lamarckii.total[[i]])
}

qecbnew$Nb.Spirobranchus.lamarckii.total <- as.integer(qecbnew$Nb.Spirobranchus.lamarckii.total)

qecbnew$Nb.Spirobranchus.lamarckii.total.diff <- abs(qecbnew$Nb.Spirobranchus.lamarckii.total - qecbnew$Nb.Spirobranchus.lamarckii.total.ini)
spirobranchus_diff <- qecbnew[, c("Nb.Spirobranchus.lamarckii.1B", "Nb.Spirobranchus.lamarckii.2B", "Nb.Spirobranchus.lamarckii.3B", "Nb.Spirobranchus.lamarckii.4B", "Nb.Spirobranchus.lamarckii.5B", "Nb.Spirobranchus.lamarckii.total", "Nb.Spirobranchus.lamarckii.total.ini", "mean.x.100", "Nb.Spirobranchus.lamarckii.total.diff")]
spirobranchus_diff <- dplyr::arrange(spirobranchus_diff, desc(Nb.Spirobranchus.lamarckii.total.diff), mean.x.100)
spirobranchus_diff <- dplyr::arrange(dplyr::filter(spirobranchus_diff, Nb.Spirobranchus.lamarckii.total.diff != 0 & mean.x.100 != 0), desc(Nb.Spirobranchus.lamarckii.total.diff))

qecbnew <- subset(qecbnew, select = -c(Nb.Spirobranchus.lamarckii.total.ini, mean.x.100, Nb.Spirobranchus.lamarckii.total.diff))

rm(qecbnew_spirobranchus, spirobranchus, spirobranchus_data, spirobranchus_diff)

# do the same for spirorbis

qecbnew$Nb.spirorbis.total.ini <- qecbnew$Nb.spirorbis.total
qecbnew$Nb.spirorbis.total <- as.character(qecbnew$Nb.spirorbis.total)

qecbnew_spirorbis <- (dplyr::filter(qecbnew, Nb.spirorbis.total %in% c(NA, "NaN", "Inf", "-Inf")))
qecbnew_spirorbis[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")] <- sapply(qecbnew_spirorbis[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], as.character)
(spirobranchus_data <- subset(qecbnew_spirorbis, !is.na(qecbnew_spirorbis$Nb.spirorbis.1A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.2A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.3A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.4A) || !is.na(qecbnew_spirorbis$Nb.spirorbis.5A))[, c("site_year_month_day", "Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total")])

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
    qecbnew$mean.x.200[[i]] <- sum(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")], na.rm = TRUE) / sum(!is.na(qecbnew[i, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A")])) * 200
} # sum of only NAs/0 = NaN; so replace NaN by Na
qecbnew$mean.x.200 <- as.character(qecbnew$mean.x.200)

for (i in c(1:nrow(qecbnew))) {
  qecbnew$mean.x.200[[i]] <- ifelse(qecbnew$mean.x.200[[i]] == "NaN", NA, qecbnew$mean.x.200[[i]])
}

qecbnew$mean.x.200 <- as.integer(qecbnew$mean.x.200)

qecbnew$Nb.spirorbis.total <- as.integer(qecbnew$Nb.spirorbis.total)
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

for (i in c(1:nrow(qecbnew))) {
  qecbnew$Nb.spirorbis.total[[i]] <- ifelse(qecbnew$Nb.spirorbis.total[[i]] == "NaN", NA, qecbnew$Nb.spirorbis.total[[i]])
}

qecbnew$Nb.spirorbis.total <- as.integer(qecbnew$Nb.spirorbis.total)

qecbnew$Nb.spirorbis.total.diff <- abs(qecbnew$Nb.spirorbis.total - qecbnew$Nb.spirorbis.total.ini)
spirorbis_diff <- qecbnew[, c("Nb.spirorbis.1A", "Nb.spirorbis.2A", "Nb.spirorbis.3A", "Nb.spirorbis.4A", "Nb.spirorbis.5A", "Nb.spirorbis.total", "Nb.spirorbis.total.ini", "mean.x.200", "Nb.spirorbis.total.diff")]
spirorbis_diff <- dplyr::arrange(spirorbis_diff, desc(Nb.spirorbis.total.diff), mean.x.200)
spirorbis_diff <- dplyr::arrange(dplyr::filter(spirorbis_diff, Nb.spirorbis.total.diff != 0 & mean.x.200 != 0), desc(Nb.spirorbis.total.diff))

qecbnew <- subset(qecbnew, select = -c(Nb.spirorbis.total.ini, mean.x.200, Nb.spirorbis.total.diff))

rm(qecbnew_spirorbis, spirorbis, spirobranchus_data, spirorbis_diff, i)


# dplyr::filter for abnormal data, based on histogram distribution of data

dplyr::filter(qecbnew, X..algues.brunes > 100)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "X..algues.brunes")]
qecbnew$X..algues.brunes <- ifelse(qecbnew$X..algues.brunes > 100, 100, qecbnew$X..algues.brunes)
dplyr::filter(qecbnew, X..algues.rouges > 100)[, c("Site", "date_fiche", "Type.Bloc", "Numéro.Bloc.échantillon", "Face", "X..algues.rouges")]
qecbnew$X..algues.rouges <- ifelse(qecbnew$X..algues.rouges > 100, 100, qecbnew$X..algues.rouges)


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

qecb <- qecbnew


`%notin%` <- Negate(`%in%`)

## reorder and/or create new variables

# variable site_year_month_day moved for clarity purpose, not needed necessarily
qecb <- tibble::add_column(qecb, qecb$site_year_month_day, .after = "Site_bis")
qecb <- qecb[, -which(names(qecb) %in% c("site_year_month_day"))]
qecb <- dplyr::rename(qecb, site_year_month_day = `qecb$site_year_month_day`)

# new variable period (nothing to see with the existing périod variable)
period <- rep(NA, nrow(qecb))
qecb <- tibble::add_column(qecb, period, .after = "Day")
qecb$period <- ifelse(as.numeric(qecb$Month) < 7, "p1", "p2")
qecb$period <- as.factor(qecb$period)
rm(period)

qecb <- dplyr::arrange(qecb, region, site_year_month_day, Type.Bloc, Numéro.Bloc.échantillon, Face)

# NB: les infos surface d'accolement sont dupliquées de la face inf vers la face sup de blocs mobiles (même si peu de sens d'avoir cette info pour les face sup ...)
# NB: les data "Abondance ressources ciblées par pêcheurs à pied" présentes uniquement pour les blocs mobiles sont dupliquées entre face inf et face sup.

## SCRIPT I - NAs <- 0

# already performed for part in the CB_qecb script ; but here I also consider mobile organisms, logical observation (or not) according to boulders, faces etc ... so more complete. Could be some kind of script fusion to only keep Na to 0 correction in one script, i.e. moving this script to the CB_qecb script ...

bretagne_bm <- dplyr::filter(qecb, region == "Bretagne" & Type.Bloc == "Bloc mobile")
bretagne_bf <- dplyr::filter(qecb, region == "Bretagne" & Type.Bloc %in% c("Bloc fixé", "Roche en place"))
egmp_basq_bm <- dplyr::filter(qecb, region == "EGMP.BASQ" & Type.Bloc == "Bloc mobile")
egmp_basq_bf <- dplyr::filter(qecb, region == "EGMP.BASQ" & Type.Bloc %in% c("Bloc fixé", "Roche en place"))

# replace NAs by "0" for variables used in qecb determination

bretagne_bm[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    #.."Nb.Crassostrea.gigas",
    #.."Nb.Ostrea.edulis",
    #.."X..Mytilus.sp.",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    "Nb.Cancer.pagurus..Tourteau.",
    "Nb.Necora.puber..Etrille.",
    "Nb.Carcinus.maenas..Crabe.vert.",
    "Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    "Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    "Nb.Haliotis.tuberculata..Ormeau.",
    #"Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    "Nb.Littorina.littorea..Bigorneau.",
    "Nb.Xantho.pilipes..Xanthe.poilu.",
    "Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ] <- lapply(bretagne_bm[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    #.."Nb.Crassostrea.gigas",
    #.."Nb.Ostrea.edulis",
    #.."X..Mytilus.sp.",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    "Nb.Cancer.pagurus..Tourteau.",
    "Nb.Necora.puber..Etrille.",
    "Nb.Carcinus.maenas..Crabe.vert.",
    "Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    "Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    "Nb.Haliotis.tuberculata..Ormeau.",
    #"Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    "Nb.Littorina.littorea..Bigorneau.",
    "Nb.Xantho.pilipes..Xanthe.poilu.",
    "Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ], function(x) replace(x, is.na(x), 0))


  # bretagne_bf
  bretagne_bf[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    #.."Nb.Crassostrea.gigas",
    #.."Nb.Ostrea.edulis",
    #.."X..Mytilus.sp.",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    #."Nb.Cancer.pagurus..Tourteau.",
    #.."Nb.Necora.puber..Etrille.",
    #."Nb.Carcinus.maenas..Crabe.vert.",
    #."Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    #."Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    #."Nb.Haliotis.tuberculata..Ormeau.",
    #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    #."Nb.Littorina.littorea..Bigorneau.",
    #."Nb.Xantho.pilipes..Xanthe.poilu.",
    #."Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ] <- lapply(bretagne_bf[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
    "Nb.spirorbis.total",
    #.."Nb.Crassostrea.gigas",
    #.."Nb.Ostrea.edulis",
    #.."X..Mytilus.sp.",
    #.."X..Hermelles",
    #.."X..Hydraires",
    "X..Eponges",
    "X..Ascidies.Coloniales",
    "X..Ascidies.Solitaires",
    "X..Bryozoaires.Dresses",
    "X..Balanes.Vivantes",
    #"Commentaires.Avant",
    "X..Surface.Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    #."Nb.Cancer.pagurus..Tourteau.",
    #.."Nb.Necora.puber..Etrille.",
    #."Nb.Carcinus.maenas..Crabe.vert.",
    #."Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    #."Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    #."Nb.Haliotis.tuberculata..Ormeau.",
    #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    #."Nb.Littorina.littorea..Bigorneau.",
    #."Nb.Xantho.pilipes..Xanthe.poilu.",
    #."Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ], function(x) replace(x, is.na(x), 0))


  # egmp_basq_bm
  egmp_basq_bm[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
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
    #"Commentaires.Avant",
    "X..Surface.Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
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
  )
  ] <- lapply(egmp_basq_bm[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
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
    #"Commentaires.Avant",
    "X..Surface.Accolement", # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
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
  )
  ], function(x) replace(x, is.na(x), 0))


  # egmp_basq_bf
  egmp_basq_bf[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
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
    #"Commentaires.Avant",
    "X..Surface.Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    #."Nb.Cancer.pagurus..Tourteau.",
    #.."Nb.Necora.puber..Etrille.",
    #."Nb.Carcinus.maenas..Crabe.vert.",
    #."Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    #."Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    #."Nb.Haliotis.tuberculata..Ormeau.",
    #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    #."Nb.Littorina.littorea..Bigorneau.",
    #."Nb.Xantho.pilipes..Xanthe.poilu.",
    #."Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ] <- lapply(egmp_basq_bf[, c(
    "X..algues.brunes",
    "Strate.algues.brunes",
    "X..algues.rouges",
    "Strate.algues.rouges",
    "X..algues.vertes",
    "Strate.algues.vertes",
    "X..Cladophora",
    "X..Lithophyllum",
    "X..Recouvrement.Sediment", # is NA, then replace by 0 as well because no sense to have a NA value for "% recouvrement sédiment" as well.
    #"Type.Sediment",
    "X..Roche.Nue", # is NA, then replace by 0 as well because no sense to have a NA value for "% roche nue" as well.
    "Nb.Littorina.obtusata",
    "Nb.Gibbula.cineraria",
    "Nb.Gibbula.pennanti",
    "Nb.Gibbula.umbilicalis",
    "Nb.Phallusia.mamillata",
    "Nb.Tethya.aurantium",
    #"Nb.Spirobranchus.lamarckii.1B",
    #"Nb.Spirobranchus.lamarckii.2B",
    #"Nb.Spirobranchus.lamarckii.3B",
    #"Nb.Spirobranchus.lamarckii.4B",
    #"Nb.Spirobranchus.lamarckii.5B",
    "Nb.Spirobranchus.lamarckii.total",
    #"Nb.spirorbis.1A",
    #"Nb.spirorbis.2A",
    #"Nb.spirorbis.3A",
    #"Nb.spirorbis.4A",
    #"Nb.spirorbis.5A",
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
    #"Commentaires.Avant",
    "X..Surface.Accolement"#, # is NA, then replace by 0 as well because no sense to have a NA value for "% surface accolement" as well.
    #"Type.sustrat.observé",
    #"Commentaires",
    #."Nb.Cancer.pagurus..Tourteau.",
    #.."Nb.Necora.puber..Etrille.",
    #."Nb.Carcinus.maenas..Crabe.vert.",
    #."Nb.Nucella.lapilus..Pourpre.",
    #.."Nb.Eriphia.verrucosa..Crabe.verruqueux.",
    #.."Nb.Octopus.vulgaris..Poulpe.",
    #."Nb.Galathea..Galathées.",
    #.."Nb.Paracentrotus.lividus..Oursin.",
    #."Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
    #."Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
    #."Nb.Haliotis.tuberculata..Ormeau.",
    #."Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.",
    #."Nb.Littorina.littorea..Bigorneau.",
    #."Nb.Xantho.pilipes..Xanthe.poilu.",
    #."Nb.Mimachlamys.varia..Pétoncle.noir."
  )
  ], function(x) replace(x, is.na(x), 0))


# merge dfs.
qecbnato0 <- dplyr::bind_rows(bretagne_bm, bretagne_bf)
qecbnato0 <- dplyr::bind_rows(qecbnato0, egmp_basq_bm)
qecbnato0 <- dplyr::bind_rows(qecbnato0, egmp_basq_bf)

qecbnato0 <- dplyr::arrange(qecbnato0, region, site_year_month_day, Type.Bloc, Numéro.Bloc.échantillon, Face)

rm(bretagne_bm, bretagne_bf, egmp_basq_bm, egmp_basq_bf)


## analyse matricielle

# NB some variables were dplyr::renamed or created, cfr I originally merged qecb and ivr data in below script to do some correlation analysis. This is not the case anymore, so no more merging anymore.

qecbnato0 <- tibble::add_column(qecbnato0, region.site_year_month_day = paste0(qecbnato0$region, qecbnato0$site_year_month_day), .before = "region")


numero_quadrat <- stringr::str_sub(qecbnato0$quadrat_bis, start = -1)
qecbnato0 <- tibble::add_column(qecbnato0, numero_quadrat, .after = "quadrat_bis")
rm(numero_quadrat)
qecbnato0$numero_quadrat <- as.integer(qecbnato0$numero_quadrat)


qecbnato0$Year <- as.integer(qecbnato0$Year)
qecbnato0$Month <- as.integer(qecbnato0$Month)
qecbnato0$Day <- as.integer(qecbnato0$Day)

############################################################
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Anna still hasn't corrected for boulder nb in FINS_Quemenes.2020.10.16 data encoding ! removed from the df_
qecbnato0 <- qecbnato0 %>% dplyr::filter(site_year_month_day != "FINS_Quemenes.2020.10.16")
############################################################

# what to do with spirorbes & Nb.Spirobranchus.lamarckii.total? log10 transformation

qecbnato0 <- tibble::add_column(qecbnato0, log10.Nb.spirorbis.total = log10(qecbnato0$Nb.spirorbis.total + 1), .after = "Nb.spirorbis.total")
qecbnato0 <- tibble::add_column(qecbnato0, log10.Nb.Spirobranchus.lamarckii.total = log10(qecbnato0$Nb.Spirobranchus.lamarckii.total + 1), .after = "Nb.Spirobranchus.lamarckii.total")

saveRDS(qecbnato0, "qecbnato0.RDS")


###############################################################
#                                                             #
#   Start dissimilarity calculation with some data handling   #
#                                                             #
###############################################################

# first, create vector (4) for qecb and fishing by region (same as above)

bret_egmp_basq_qecb <- c(
  "X..algues.brunes",
  "X..algues.rouges",
  "X..algues.vertes",
  "X..Cladophora",
  "X..Lithophyllum",
  "Nb.Littorina.obtusata",
  "Nb.Gibbula.cineraria",
  "Nb.Gibbula.pennanti",
  "Nb.Gibbula.umbilicalis",
  "Nb.Phallusia.mamillata",
  "Nb.Tethya.aurantium",
  "Nb.Spirobranchus.lamarckii.total",
  "Nb.spirorbis.total",
  "X..Eponges",
  "X..Ascidies.Coloniales",
  "X..Ascidies.Solitaires",
  "X..Bryozoaires.Dresses",
  "X..Balanes.Vivantes"
  #, "X..Recouvrement.Sediment"
  #, "X..Roche.Nue"
  #, "X..Surface.Accolement"
  )

egmp_basq_qecb <- c("Nb.Crassostrea.gigas", "Nb.Ostrea.edulis", "X..Mytilus.sp.", "X..Hermelles", "X..Hydraires")

bret_egmp_basq_fishing <- c("Nb.Cancer.pagurus..Tourteau.",
                            "Nb.Necora.puber..Etrille.",
                            "Nb.Carcinus.maenas..Crabe.vert.",
                            "Nb.Nucella.lapilus..Pourpre.",
                            "Nb.Galathea..Galathées.",
                            "Nb.Lophozozymus.incisus..ancien.Xantho.incisus.",
                            "Nb.Palaemon.sp..Crevette.bouquet.ou.crevette.rose.",
                            "Nb.Haliotis.tuberculata..Ormeau.",
                            "Nb.Littorina.littorea..Bigorneau.",
                            "Nb.Xantho.pilipes..Xanthe.poilu.",
                            "Nb.Mimachlamys.varia..Pétoncle.noir.")

egmp_basq_fishing <- c("Nb.Eriphia.verrucosa..Crabe.verruqueux.", "Nb.Octopus.vulgaris..Poulpe.", "Nb.Paracentrotus.lividus..Oursin.", "Nb.Stramonita.haemastoma..Pourpre.bouche.de.sang.")

# here I can choose to either replace spirorbis and/or spirobranchus by their log10 transformation in bret_egmp_basq_qecb vector
bret_egmp_basq_qecb <- replace(bret_egmp_basq_qecb, bret_egmp_basq_qecb == "Nb.spirorbis.total", "log10.Nb.spirorbis.total")
saveRDS(bret_egmp_basq_qecb, "bret_egmp_basq_qecb.RDS")


#############################################################
#                                                           #
#                  Compute dissimilarity                    #
#                                                           #
#############################################################
### determination of coefficient of dissimilarity face sup mobile bloc vs fixed bloc

# loop in a fct

matri_fct_bmf <- function(data, conca) {

  matri_df <- data

  for (x in c(1:length(unique(matri_df$site_year_month_day)))) {

    qecbnato0_x <- matri_df %>% dplyr::filter(site_year_month_day == unique(matri_df$site_year_month_day)[[x]])

    rownames(qecbnato0_x) <- paste0(qecbnato0_x$Type.Bloc, "_", qecbnato0_x$Face,  "_", qecbnato0_x$Numéro.Bloc.échantillon, "_", qecbnato0_x$quadrat_bis)


  mtxdis <- vegan::vegdist(
    sqrt(qecbnato0_x[, conca]), #Transform your species abundance data_ Typically, raw abundances are transformed prior to analysis. Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats.stackexchange.com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)

    na.rm = TRUE,
    method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis. If your data contains samples that are all-zero you will run into the double zero problem. This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  )


 #https://rdrr.io/github/phytomosaic/ecole/man/bray0.html
 # mtxdis <- ecole::bray0(
 #   sqrt
  #  (qecbnato0_x[,conca]), na.rm = TRUE)

  expand.grid(mtxdis)

  mtxdisdf_ <- as.data.frame(as.matrix(mtxdis))

  a_ <- NA
  b_ <- NA
  c_ <- NA
  d_ <- NA
  e_ <- NA
  f_ <- NA
  g_ <- NA
  h_ <- NA
  i_ <- NA
  j_ <- NA
  k_ <- NA
  l_ <- NA
  m_ <- NA
  n_ <- NA

  for (z in c(1:nrow(mtxdisdf_))) {

    a_[[z]] <- (paste0(rownames(mtxdisdf_[z + 1, ]), " & ", ifelse(nrow(mtxdisdf_) >= 1, colnames(mtxdisdf_[1]), NA)))
    b_[[z]] <- (paste0(rownames(mtxdisdf_[z + 2, ]), " & ", ifelse(nrow(mtxdisdf_) >= 2, colnames(mtxdisdf_[2]), NA)))
    c_[[z]] <- (paste0(rownames(mtxdisdf_[z + 3, ]), " & ", ifelse(nrow(mtxdisdf_) >= 3, colnames(mtxdisdf_[3]), NA)))
    d_[[z]] <- (paste0(rownames(mtxdisdf_[z + 4, ]), " & ", ifelse(nrow(mtxdisdf_) >= 4, colnames(mtxdisdf_[4]), NA)))
    e_[[z]] <- (paste0(rownames(mtxdisdf_[z + 5, ]), " & ", ifelse(nrow(mtxdisdf_) >= 5, colnames(mtxdisdf_[5]), NA)))
    f_[[z]] <- (paste0(rownames(mtxdisdf_[z + 6, ]), " & ", ifelse(nrow(mtxdisdf_) >= 6, colnames(mtxdisdf_[6]), NA)))
    g_[[z]] <- (paste0(rownames(mtxdisdf_[z + 7, ]), " & ", ifelse(nrow(mtxdisdf_) >= 7, colnames(mtxdisdf_[7]), NA)))
    h_[[z]] <- (paste0(rownames(mtxdisdf_[z + 8, ]), " & ", ifelse(nrow(mtxdisdf_) >= 8, colnames(mtxdisdf_[8]), NA)))
    i_[[z]] <- (paste0(rownames(mtxdisdf_[z + 9, ]), " & ", ifelse(nrow(mtxdisdf_) >= 9, colnames(mtxdisdf_[9]), NA)))
    j_[[z]] <- (paste0(rownames(mtxdisdf_[z + 10, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 10, colnames(mtxdisdf_[10]), NA)))
    k_[[z]] <- (paste0(rownames(mtxdisdf_[z + 11, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 11, colnames(mtxdisdf_[11]), NA)))
    l_[[z]] <- (paste0(rownames(mtxdisdf_[z + 12, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 12, colnames(mtxdisdf_[12]), NA)))
    m_[[z]] <- (paste0(rownames(mtxdisdf_[z + 13, ]), " & ", ifelse(nrow(mtxdisdf_) >= 13, colnames(mtxdisdf_[13]), NA)))
    n_[[z]] <- (paste0(rownames(mtxdisdf_[z + 14, ]), " & ", ifelse(nrow(mtxdisdf_) >= 14, colnames(mtxdisdf_[14]), NA)))

  }

  rm(z)

  y <- length(a_) - (ifelse(nrow(mtxdisdf_) >= 1, 1, nrow(mtxdisdf_)))
  a_ <- a_[1:y]
  y <- length(b_) - (ifelse(nrow(mtxdisdf_) >= 2, 2, nrow(mtxdisdf_)))
  b_ <- b_[1:y]
  y <- length(c_) - (ifelse(nrow(mtxdisdf_) >= 3, 3, nrow(mtxdisdf_)))
  c_ <- c_[1:y]
  y <- length(d_) - (ifelse(nrow(mtxdisdf_) >= 4, 4, nrow(mtxdisdf_)))
  d_ <- d_[1:y]
  y <- length(e_) - (ifelse(nrow(mtxdisdf_) >= 5, 5, nrow(mtxdisdf_)))
  e_ <- e_[1:y]
  y <- length(f_) - (ifelse(nrow(mtxdisdf_) >= 6, 6, nrow(mtxdisdf_)))
  f_ <- f_[1:y]
  y <- length(g_) - (ifelse(nrow(mtxdisdf_) >= 7, 7, nrow(mtxdisdf_)))
  g_ <- g_[1:y]
  y <- length(h_) - (ifelse(nrow(mtxdisdf_) >= 8, 8, nrow(mtxdisdf_)))
  h_ <- h_[1:y]
  y <- length(i_) - (ifelse(nrow(mtxdisdf_) >= 9, 9, nrow(mtxdisdf_)))
  i_ <- i_[1:y]
  y <- length(j_) - (ifelse(nrow(mtxdisdf_) >= 10, 10, nrow(mtxdisdf_)))
  j_ <- j_[1:y]
  y <- length(k_) - (ifelse(nrow(mtxdisdf_) >= 11, 11, nrow(mtxdisdf_)))
  k_ <- k_[1:y]
  y <- length(l_) - (ifelse(nrow(mtxdisdf_) >= 12, 12, nrow(mtxdisdf_)))
  l_ <- l_[1:y]
  y <- length(m_) - (ifelse(nrow(mtxdisdf_) >= 13, 13, nrow(mtxdisdf_)))
  m_ <- m_[1:y]
  y <- length(n_) - (ifelse(nrow(mtxdisdf_) >= 14, 14, nrow(mtxdisdf_)))
  n_ <- n_[1:y]

  rm(y)

  name_ <- c(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_)
  df_ <- data.frame(expand.grid(mtxdis), name_[1:nrow(expand.grid(mtxdis))])
  names(df_) <- c("dist.", "name_")

  rm(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_)
  rm(name_)

  q_ <- strsplit(df_$name_, " & ")
  mat_  <- matrix(unlist(q_), ncol = 2, byrow = TRUE)
  q_df_   <- as.data.frame(matrix(unlist(q_), ncol = 2, byrow = TRUE))
  df_ <- dplyr::bind_cols(df_, q_df_)

  rm(q_, mat_, q_df_)

  split_ <- strsplit(df_$V1, "_")
  v1_split_ <- as.data.frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))
  split_ <- strsplit(df_$V2, "_")
  v2_split_ <- as.data.frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))

  df_ <- dplyr::bind_cols(df_, v1_split_)
  df_ <- dplyr::bind_cols(df_, v2_split_)
  df_red_ <- subset(df_, V4...8 == V4...12 & V1...5 != V1...9)
  site_year_month_day <- rep(unique(qecbnato0_x$site_year_month_day), nrow(df_red_))

  df_red_ <- tibble::add_column(df_red_, site_year_month_day, .before = "dist.")

  rm(split_, v1_split_, v2_split_)
  rm(mtxdis, mtxdisdf_, df_, site_year_month_day)

  matri_list[[x]] <- df_red_
  matri_list <<- matri_list

  rm(df_red_, qecbnato0_x, x)

  }

  matri_df <- do.call("rbind", matri_list)

  names(matri_df) <- c("site_year_month_day", "dist.", "name_", "name_left", "name_right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")

  matri_df <<- matri_df

  hist(matri_df$dist.)

}

data_ <- dplyr::filter(qecbnato0, Face == "face supérieure")
data_$Type.Bloc <- ifelse(as.character(data_$Type.Bloc) == "Roche en place", "Bloc fixé", as.character(data_$Type.Bloc))
matri_list <- vector("list", length(unique(data_$site_year_month_day)))

matri_fct_bmf(data = data_, conca = c(bret_egmp_basq_qecb, egmp_basq_qecb))
hist(matri_df$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BF -")))

matri_full_bm_bf_fs <- matri_df


rm(data_, matri_df, matri_list)


### determination of coefficient of dissimilarity between blocs mobiles face sup vs face inf.

# loop in a fct

matri_fct_bmm <- function(data, conca) {

  matri_df <- data

  for (x in c(1:length(unique(matri_df$site_year_month_day)))) {

    qecbnato0_x <- matri_df %>% dplyr::filter(site_year_month_day == unique(matri_df$site_year_month_day)[[x]])

    rownames(qecbnato0_x) <- paste0(qecbnato0_x$Type.Bloc, "_", qecbnato0_x$Face,  "_", qecbnato0_x$Numéro.Bloc.échantillon, "_", qecbnato0_x$quadrat_bis)


  mtxdis <- vegan::vegdist(
  sqrt(qecbnato0_x[, c(bret_egmp_basq_qecb)]), #Transform your species abundance data_ Typically, raw abundances are transformed prior to analysis. Usually you will use square root, fourth-root, log(X+1), or presence-absence (square root being least extreme, P/A being most). I would start with square root. (https://stats.stackexchange.com/questions/234495/double-zeroes-problem-with-euclidean-distance-and-abundance-data-is-the-proble)
  na.rm = TRUE,
  method = "bray" #Construct species abundance dissimilarity matrices with Bray-Curtis. If your data contains samples that are all-zero you will run into the double zero problem. This can be overcome by using a zero-adjusted Bray-Curtis coefficient, which is sometimes referred to as a 'dummy variable' which damps down the similarity fluctuations between samples that are both zero (undefined). => see below for zero-adjusted Bray-Curtis coefficient ; #another possibility, sqrt + 1 ??
  )


  #mtxdis <- ecole::bray0(
   # sqrt(qecbnato0_x[, conca]), na.rm = TRUE)


  expand.grid(mtxdis)

  mtxdisdf_ <- as.data.frame(as.matrix(mtxdis))

  a_ <- NA
  b_ <- NA
  c_ <- NA
  d_ <- NA
  e_ <- NA
  f_ <- NA
  g_ <- NA
  h_ <- NA
  i_ <- NA
  j_ <- NA
  k_ <- NA
  l_ <- NA
  m_ <- NA
  n_ <- NA
  o_ <- NA
  p_ <- NA
  q_ <- NA
  r_ <- NA
  s_ <- NA

  for (z in c(1:nrow(mtxdisdf_))) {

    a_[[z]] <- (paste0(rownames(mtxdisdf_[z + 1, ]), " & ", ifelse(nrow(mtxdisdf_) >= 1, colnames(mtxdisdf_[1]), NA)))
    b_[[z]] <- (paste0(rownames(mtxdisdf_[z + 2, ]), " & ", ifelse(nrow(mtxdisdf_) >= 2, colnames(mtxdisdf_[2]), NA)))
    c_[[z]] <- (paste0(rownames(mtxdisdf_[z + 3, ]), " & ", ifelse(nrow(mtxdisdf_) >= 3, colnames(mtxdisdf_[3]), NA)))
    d_[[z]] <- (paste0(rownames(mtxdisdf_[z + 4, ]), " & ", ifelse(nrow(mtxdisdf_) >= 4, colnames(mtxdisdf_[4]), NA)))
    e_[[z]] <- (paste0(rownames(mtxdisdf_[z + 5, ]), " & ", ifelse(nrow(mtxdisdf_) >= 5, colnames(mtxdisdf_[5]), NA)))
    f_[[z]] <- (paste0(rownames(mtxdisdf_[z + 6, ]), " & ", ifelse(nrow(mtxdisdf_) >= 6, colnames(mtxdisdf_[6]), NA)))
    g_[[z]] <- (paste0(rownames(mtxdisdf_[z + 7, ]), " & ", ifelse(nrow(mtxdisdf_) >= 7, colnames(mtxdisdf_[7]), NA)))
    h_[[z]] <- (paste0(rownames(mtxdisdf_[z + 8, ]), " & ", ifelse(nrow(mtxdisdf_) >= 8, colnames(mtxdisdf_[8]), NA)))
    i_[[z]] <- (paste0(rownames(mtxdisdf_[z + 9, ]), " & ", ifelse(nrow(mtxdisdf_) >= 9, colnames(mtxdisdf_[9]), NA)))
    j_[[z]] <- (paste0(rownames(mtxdisdf_[z + 10, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 10, colnames(mtxdisdf_[10]), NA)))
    k_[[z]] <- (paste0(rownames(mtxdisdf_[z + 11, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 11, colnames(mtxdisdf_[11]), NA)))
    l_[[z]] <- (paste0(rownames(mtxdisdf_[z + 12, ]), " & ",  ifelse(nrow(mtxdisdf_) >= 12, colnames(mtxdisdf_[12]), NA)))
    m_[[z]] <- (paste0(rownames(mtxdisdf_[z + 13, ]), " & ", ifelse(nrow(mtxdisdf_) >= 13, colnames(mtxdisdf_[13]), NA)))
    n_[[z]] <- (paste0(rownames(mtxdisdf_[z + 14, ]), " & ", ifelse(nrow(mtxdisdf_) >= 14, colnames(mtxdisdf_[14]), NA)))
    o_[[z]] <- (paste0(rownames(mtxdisdf_[z + 15, ]), " & ", ifelse(nrow(mtxdisdf_) >= 15, colnames(mtxdisdf_[15]), NA)))
    p_[[z]] <- (paste0(rownames(mtxdisdf_[z + 16, ]), " & ", ifelse(nrow(mtxdisdf_) >= 16, colnames(mtxdisdf_[16]), NA)))
    q_[[z]] <- (paste0(rownames(mtxdisdf_[z + 17, ]), " & ", ifelse(nrow(mtxdisdf_) >= 17, colnames(mtxdisdf_[17]), NA)))
    r_[[z]] <- (paste0(rownames(mtxdisdf_[z + 18, ]), " & ", ifelse(nrow(mtxdisdf_) >= 18, colnames(mtxdisdf_[18]), NA)))
    s_[[z]] <- (paste0(rownames(mtxdisdf_[z + 19, ]), " & ", ifelse(nrow(mtxdisdf_) >= 19, colnames(mtxdisdf_[19]), NA)))

  }

  rm(z)

  y <- length(a_) - (ifelse(nrow(mtxdisdf_) >= 1, 1, nrow(mtxdisdf_)))
  a_ <- a_[1:y]
  y <- length(b_) - (ifelse(nrow(mtxdisdf_) >= 2, 2, nrow(mtxdisdf_)))
  b_ <- b_[1:y]
  y <- length(c_) - (ifelse(nrow(mtxdisdf_) >= 3, 3, nrow(mtxdisdf_)))
  c_ <- c_[1:y]
  y <- length(d_) - (ifelse(nrow(mtxdisdf_) >= 4, 4, nrow(mtxdisdf_)))
  d_ <- d_[1:y]
  y <- length(e_) - (ifelse(nrow(mtxdisdf_) >= 5, 5, nrow(mtxdisdf_)))
  e_ <- e_[1:y]
  y <- length(f_) - (ifelse(nrow(mtxdisdf_) >= 6, 6, nrow(mtxdisdf_)))
  f_ <- f_[1:y]
  y <- length(g_) - (ifelse(nrow(mtxdisdf_) >= 7, 7, nrow(mtxdisdf_)))
  g_ <- g_[1:y]
  y <- length(h_) - (ifelse(nrow(mtxdisdf_) >= 8, 8, nrow(mtxdisdf_)))
  h_ <- h_[1:y]
  y <- length(i_) - (ifelse(nrow(mtxdisdf_) >= 9, 9, nrow(mtxdisdf_)))
  i_ <- i_[1:y]
  y <- length(j_) - (ifelse(nrow(mtxdisdf_) >= 10, 10, nrow(mtxdisdf_)))
  j_ <- j_[1:y]
  y <- length(k_) - (ifelse(nrow(mtxdisdf_) >= 11, 11, nrow(mtxdisdf_)))
  k_ <- k_[1:y]
  y <- length(l_) - (ifelse(nrow(mtxdisdf_) >= 12, 12, nrow(mtxdisdf_)))
  l_ <- l_[1:y]
  y <- length(m_) - (ifelse(nrow(mtxdisdf_) >= 13, 13, nrow(mtxdisdf_)))
  m_ <- m_[1:y]
  y <- length(n_) - (ifelse(nrow(mtxdisdf_) >= 14, 14, nrow(mtxdisdf_)))
  n_ <- n_[1:y]
  y <- length(o_) - (ifelse(nrow(mtxdisdf_) >= 15, 15, nrow(mtxdisdf_)))
  o_ <- o_[1:y]
  y <- length(p_) - (ifelse(nrow(mtxdisdf_) >= 16, 16, nrow(mtxdisdf_)))
  p_ <- p_[1:y]
  y <- length(q_) - (ifelse(nrow(mtxdisdf_) >= 17, 17, nrow(mtxdisdf_)))
  q_ <- q_[1:y]
  y <- length(r_) - (ifelse(nrow(mtxdisdf_) >= 18, 18, nrow(mtxdisdf_)))
  r_ <- r_[1:y]
  y <- length(s_) - (ifelse(nrow(mtxdisdf_) >= 19, 19, nrow(mtxdisdf_)))
  s_ <- s_[1:y]

  rm(y)

  name_ <- c(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_, o_, p_, q_, r_, s_)
  df_ <- data.frame(expand.grid(mtxdis), name_[1:seq_len(nrow(expand.grid(mtxdis)))])
  names(df_) <- c("dist.", "name_")

  rm(a_, b_, c_, d_, e_, f_, g_, h_, i_, j_, k_, l_, m_, n_, o_, p_, q_, r_, s_)
  rm(name_)

  q_ <- strsplit(df_$name_, " & ")
  mat_  <- matrix(unlist(q_), ncol = 2, byrow = TRUE)
  q_df_   <- as.data.frame(matrix(unlist(q_), ncol = 2, byrow = TRUE))
  df_ <- dplyr::bind_cols(df_, q_df_)

  rm(q_, mat_, q_df_)

  split_ <- strsplit(df_$V1, "_")
  v1_split_ <- as.data.frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))
  split_ <- strsplit(df_$V2, "_")
  v2_split_ <- as.data.frame(matrix(unlist(split_), ncol = 4, byrow = TRUE))

  df_ <- dplyr::bind_cols(df_, v1_split_)
  df_ <- dplyr::bind_cols(df_, v2_split_)
  df_red_ <- subset(df_, V4...8 == V4...12 & V3...7 == V3...11)
  site_year_month_day <- rep(unique(qecbnato0_x$site_year_month_day), nrow(df_red_))

  df_red_ <- tibble::add_column(df_red_, site_year_month_day, .before = "dist.")

  rm(split_, v1_split_, v2_split_)
  rm(mtxdis, mtxdisdf_, df_, site_year_month_day)

  matri_list[[x]] <- df_red_
  matri_list <<- matri_list

  rm(df_red_, qecbnato0_x, x)

  }

  matri_df <- do.call("rbind", matri_list)

  names(matri_df) <- c("site_year_month_day", "dist.", "name_", "name_left", "name_right", "Type.Bloc.left", "Face.left", "Numéro.Bloc.échantillon.left", "Quadrat.left", "Type.Bloc.right", "Face.right", "Numéro.Bloc.échantillon.right", "Quadrat.right")

  matri_df <<- matri_df

  hist(matri_df$dist.)

}

data_ <- dplyr::filter(qecbnato0, Type.Bloc == "Bloc mobile")
matri_list <- vector("list", length(unique(data_$site_year_month_day)))

matri_fct_bmm(data = data_, conca = c(bret_egmp_basq_qecb, egmp_basq_qecb))
hist(matri_df$dist., main = c(paste("Histo. of Bray (0-adjusted) dist. dissimilarity measures"), paste(" (sqrt transfo) - BMfs vs BMfi -")))

matri_full_bm_bf_fi <- matri_df

rm(data_, matri_df, matri_list)


#############################################################
#                                                           #
#                     Plot dissimilarity                    #
#                                                           #
#############################################################
## plot

# activate line

matri_full_bm_bf_fs <- tidyr::separate(matri_full_bm_bf_fs, "site_year_month_day", into = c("departement", "Site", "Year", "Month", "Day"), remove = FALSE)
matri_full_bm_bf_fs$Site <- paste0(matri_full_bm_bf_fs$departement, "_", matri_full_bm_bf_fs$Site)
matri_full_bm_bf_fs <- subset(matri_full_bm_bf_fs, select = - c(departement))

matri_full_bm_bf_fs <- tibble::add_column(matri_full_bm_bf_fs, Date = as.Date(paste0(matri_full_bm_bf_fs$Year, "-", matri_full_bm_bf_fs$Month, "-", matri_full_bm_bf_fs$Day), origin = "1970-01-01"), .after = "Site")
matri_full_bm_bf_fs$Site <- as.factor(matri_full_bm_bf_fs$Site)

matri_full_bm_bf_fi <- tidyr::separate(matri_full_bm_bf_fi, "site_year_month_day", into = c("departement", "Site", "Year", "Month", "Day"), remove = FALSE)
matri_full_bm_bf_fi$Site <- paste0(matri_full_bm_bf_fi$departement, "_", matri_full_bm_bf_fi$Site)
matri_full_bm_bf_fi <- subset(matri_full_bm_bf_fi, select = - c(departement))
matri_full_bm_bf_fi <- tibble::add_column(matri_full_bm_bf_fi, Date = as.Date(paste0(matri_full_bm_bf_fi$Year, "-", matri_full_bm_bf_fi$Month, "-", matri_full_bm_bf_fi$Day), origin = "1970-01-01"), .after = "Site")
matri_full_bm_bf_fi$Site <- as.factor(matri_full_bm_bf_fi$Site)

# if error message "Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state"


bf_fs_plot <- ggplot2::ggplot(matri_full_bm_bf_fs, ggplot2::aes(x = Site, y = dist.)) +
  ggplot2::geom_boxplot() +
  #geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("distance diss. BM.BF_FS") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot2::ggsave("distance_diss_BF_FS.png", bf_fs_plot, height = 4.5, width = 4)

fs_fi_plot <- ggplot2::ggplot(matri_full_bm_bf_fi, ggplot2::aes(x = Site, y = dist.)) +
  ggplot2::geom_boxplot() +
  #geom_jitter(shape = 16, position=position_jitter(0.2)) +
  ggplot2::xlab("") +
  ggplot2::ylab("distance diss. BM_FS.FI") +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

ggplot2::ggsave("distance_diss_FS_FI.png", fs_fi_plot, height = 4.5, width = 4)

# issue with type de bloc, numéro de bloc and quadrat for df_ BM.BF_FS, cfr df_ left vs right variables doesn't give the right combination (variables with left vs right label in names come from the dissimilarity coefficient functions).
matri_full_bm_bf_fs$Quadrat <- NA
for (i in c(1:seq_len(nrow(matri_full_bm_bf_fs)))) {
  ifelse(matri_full_bm_bf_fs$Type.Bloc.left[i] == "Bloc mobile", matri_full_bm_bf_fs$Quadrat[i] <- matri_full_bm_bf_fs$Quadrat.left[i], matri_full_bm_bf_fs$Quadrat[i] <- matri_full_bm_bf_fs$Quadrat.right[i])
}
matri_full_bm_bf_fs$Numéro.Bloc <- NA
for (i in c(1:seq_len(nrow(matri_full_bm_bf_fs)))) {
  ifelse(matri_full_bm_bf_fs$Type.Bloc.left[i] == "Bloc mobile", matri_full_bm_bf_fs$Numéro.Bloc[i] <- matri_full_bm_bf_fs$Numéro.Bloc.échantillon.left[i], matri_full_bm_bf_fs$Numéro.Bloc[i] <- matri_full_bm_bf_fs$Numéro.Bloc.échantillon.right[i])
}

matri_full_bm_bf_fs <- tibble::add_column(matri_full_bm_bf_fs, site_year_month_day.q_BMnb = paste0(matri_full_bm_bf_fs$site_year_month_day, "_",  matri_full_bm_bf_fs$Quadrat, "_", matri_full_bm_bf_fs$Numéro.Bloc), .after = "site_year_month_day")
matri_full_bm_bf_fi <- tibble::add_column(matri_full_bm_bf_fi, site_year_month_day.q_BMnb = paste0(matri_full_bm_bf_fi$site_year_month_day, "_",  matri_full_bm_bf_fi$Quadrat.left, "_", matri_full_bm_bf_fi$Numéro.Bloc.échantillon.left), .after = "site_year_month_day")

colnames(matri_full_bm_bf_fs) <- paste("BM.BF_FS", colnames(matri_full_bm_bf_fs), sep = "_")
matri_full_bm_bf_fs <- dplyr::rename(matri_full_bm_bf_fs, site_year_month_day.q_BMnb = BM.BF_FS_site_year_month_day.q_BMnb)
colnames(matri_full_bm_bf_fi) <- paste("BM_FS.FI", colnames(matri_full_bm_bf_fi), sep = "_")
matri_full_bm_bf_fi <- dplyr::rename(matri_full_bm_bf_fi, site_year_month_day.q_BMnb = BM_FS.FI_site_year_month_day.q_BMnb)

matri_full <- dplyr::full_join(matri_full_bm_bf_fs[, c("site_year_month_day.q_BMnb", "BM.BF_FS_dist.")], matri_full_bm_bf_fi[, c("site_year_month_day.q_BMnb", "BM_FS.FI_dist.")])

matri_full <- tidyr::separate(matri_full, "site_year_month_day.q_BMnb", into = c("departement", "Site", "Year", "Month", "Day", "Quadrat", "Bloc Mobile Number"), remove = FALSE)
matri_full$Site <- paste0(matri_full$departement, "_", matri_full$Site)
matri_full <- subset(matri_full, select = - c(departement))
matri_full <- tibble::add_column(matri_full, Date = as.Date(paste0(matri_full$Year, "-", matri_full$Month, "-", matri_full$Day), origin = "1970-01-01"), .after = "Site")

# Name for report/plot

matri_full <- tibble::add_column(matri_full, Site_bis = matri_full$Site, .after = "Site")

matri_full$Site_bis <- ifelse(matri_full$Site == "GDMO_Locmariaquer", "Locmariaquer", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "GDMO_BegLann", "Beg Lann", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FOUR_PlateauFour", "Plateau du Four", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_GroinCou", "Groin du Cou", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_PasEmsembert", "Le Pas d'Emsembert", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_BreeBains", "La Brée-les-Bains", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_PerreAntiochat", "Le Perré d'Antiochat", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "EGMP_Chassiron", "Chassiron", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "BASQ_FlotsBleusZP", "Les Flots Bleus / zone pêcheurs", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "BASQ_FlotsBleusZF", "Les Flots Bleus / zone familles", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "GONB_IlotStMichel", "Îlot Saint-Michel", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_Quemenes", "Quéménès", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_SeinGoulenez", "Île de Sein - Goulenez", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_SeinKilaourou", "Île de Sein - Kilaourou", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_Verdelet", "Îlot du Verdelet", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_Piegu", "Piégu", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_Bilfot", "Pointe de Bilfot", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "ARMO_IlePlate", "Île Plate", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "PDMO_Perharidy", "Perharidy", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "BRES_Keraliou", "Keraliou", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_Mousterlin", "Pointe de Mousterlin", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_StNicolasGlenan", "Saint-Nicolas des Glénan", matri_full$Site_bis)
matri_full$Site_bis <- ifelse(matri_full$Site == "FINS_AnseRoz", "Pointe de l'Anse du Roz", matri_full$Site_bis)

saveRDS(matri_full, "matri_full.RDS")

#############################################################
#                                                           #
#            Plot the dissimilarity per site                #
#                                                           #
#############################################################
## plot dissimilarity coefficient

matri_full$Year <- as.integer(matri_full$Year)
matri_full$Month <- as.integer(matri_full$Month)
matri_full$Day <- as.integer(matri_full$Day)


## BM_FS.FI_dist => mobile boulder upper vs lower faces

# Stats

bm_fs_fi_dist_stat <- matri_full %>% dplyr::group_by(Site, Site_bis, Date, Year, Month, Day) %>% dplyr::summarize(BM_FS.FI_dist.moy = mean(BM_FS.FI_dist., na.rm = TRUE), BM_FS.FI_dist.et = sd(BM_FS.FI_dist., na.rm = TRUE), BM_FS.FI_dist.med = median(BM_FS.FI_dist., na.rm = TRUE), BM_FS.FI_dist.min = min(BM_FS.FI_dist., na.rm = TRUE), BM_FS.FI_dist.max = max(BM_FS.FI_dist., na.rm = TRUE), nb. = dplyr::n(), nb.notNa = sum(!is.na(BM_FS.FI_dist.)))

bm_fs_fi_dist_stat <- dplyr::ungroup(bm_fs_fi_dist_stat)

# Quality scale based on quartiles

if (choice == "N") {
  one <- round(mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. <= quantile(matri_full$BM_FS.FI_dist., 0.25, na.rm = TRUE))["BM_FS.FI_dist."])), digits = 3)
  two <- round(mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. > quantile(matri_full$BM_FS.FI_dist., 0.25, na.rm = TRUE) & BM_FS.FI_dist. <= quantile(matri_full$BM_FS.FI_dist., 0.5, na.rm = TRUE))["BM_FS.FI_dist."])), digits = 3)
  three <- round(mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. > quantile(matri_full$BM_FS.FI_dist., 0.5, na.rm = TRUE) & BM_FS.FI_dist. <= quantile(matri_full$BM_FS.FI_dist., 0.75, na.rm = TRUE))["BM_FS.FI_dist."])), digits = 3)
  four <- round(mean(unlist(dplyr::filter(matri_full, BM_FS.FI_dist. > quantile(matri_full$BM_FS.FI_dist., 0.75, na.rm = TRUE))["BM_FS.FI_dist."])), digits = 3)
}else {
  one <- 0.47
  two <- 0.7
  three <- 0.83
  four <- 0.98
}

val_xmax <- as.Date(paste0(as.character(choice_date + 1), "-01-01"), origin = "1970-01-01")

for (i in c(1:length(unique(bm_fs_fi_dist_stat$Site)))) {

  df1 <- dplyr::filter(bm_fs_fi_dist_stat, bm_fs_fi_dist_stat$Site == unique(bm_fs_fi_dist_stat$Site)[i])

  bm_fs_fi_plot <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = bm_fs_fi_dist_stat$Date, y = bm_fs_fi_dist_stat$BM_FS.FI_dist.med), col = "grey") +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = - 0.1, ymax = one, fill = "blue"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = one, ymax = two, fill = "green"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = two, ymax = three, fill = "yellow"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = three, ymax = four, fill = "orange"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = four, ymax = 1.1, fill = "red"), alpha = 0.3) +
  ggplot2::scale_fill_manual(values = c("#FF0000", "#F59404", "#18E125", "#1A1AE8", "#FAFA15")) +
  ggplot2::geom_pointrange(ggplot2::aes(x = df1$Date, y = df1$BM_FS.FI_dist.med, ymin = df1$BM_FS.FI_dist.min, ymax =  df1$BM_FS.FI_dist.max), col = "black") +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Coef dissim BM FS-FI") +
  ggplot2::ggtitle(unique(df1$Site_bis)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none")

ggplot2::ggsave(paste0("fs_fi_", df1$Site, ".png"), device = "png", bm_fs_fi_plot, height = 3, width = 3.5)

}

rm(df1, four, i, one, three, two, xmax_, xmin_, ymax_, ymin_)


## BM.BF_FS_dist => mobile boulder vs fixed boulder upper faces

# Stats

bm_bf_fs_dist_stat <- matri_full %>% dplyr::group_by(Site, Site_bis, Date, Year, Month, Day) %>% dplyr::summarize(BM.BF_FS_dist.moy = mean(BM.BF_FS_dist., na.rm = TRUE), BM.BF_FS_dist.et = sd(BM.BF_FS_dist., na.rm = TRUE), BM.BF_FS_dist.med = median(BM.BF_FS_dist., na.rm = TRUE), BM.BF_FS_dist.min = min(BM.BF_FS_dist., na.rm = TRUE), BM.BF_FS_dist.max = max(BM.BF_FS_dist., na.rm = TRUE), nb. = dplyr::n(), nb.notNa = sum(!is.na(BM.BF_FS_dist.)))

bm_bf_fs_dist_stat <- dplyr::ungroup(bm_bf_fs_dist_stat)

# Quality scale based on quartiles

if (choice == "N") {
  one <- round(mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. <= quantile(matri_full$BM.BF_FS_dist., 0.25, na.rm = TRUE))["BM.BF_FS_dist."])), digits = 3)
  two <- round(mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. > quantile(matri_full$BM.BF_FS_dist., 0.25, na.rm = TRUE) & BM.BF_FS_dist. <= quantile(matri_full$BM.BF_FS_dist., 0.5, na.rm = TRUE))["BM.BF_FS_dist."])), digits = 3)
  three <- round(mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. > quantile(matri_full$BM.BF_FS_dist., 0.5, na.rm = TRUE) & BM.BF_FS_dist. <= quantile(matri_full$BM.BF_FS_dist., 0.75, na.rm = TRUE))["BM.BF_FS_dist."])), digits = 3)
  four <- round(mean(unlist(dplyr::filter(matri_full, BM.BF_FS_dist. > quantile(matri_full$BM.BF_FS_dist., 0.75, na.rm = TRUE))["BM.BF_FS_dist."])), digits = 3)

}else {
  one <- 0.19
  two <- 0.32
  three <- 0.455
  four <- 0.735
}
# Plot

for (i in c(1:length(unique(bm_bf_fs_dist_stat$Site)))) {

  df1 <- dplyr::filter(bm_bf_fs_dist_stat, bm_bf_fs_dist_stat$Site == unique(bm_bf_fs_dist_stat$Site)[i])

  bm_bf_fs_plot <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = bm_bf_fs_dist_stat$Date, y = bm_bf_fs_dist_stat$BM.BF_FS_dist.med), col = "grey") +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = - 0.1, ymax = one, fill = "red"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = one, ymax = two, fill = "orange"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = two, ymax = three, fill = "yellow"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = three, ymax = four, fill = "green"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = as.Date("2013-01-01", origin = "1970-01-01"), xmax = val_xmax, ymin = four, ymax = 1.1, fill = "blue"), alpha = 0.3) +
  ggplot2::scale_fill_manual(values = c("#FF0000", "#F59404", "#18E125", "#1A1AE8", "#FAFA15")) +
  ggplot2::geom_pointrange(ggplot2::aes(x = df1$Date, y = df1$BM.BF_FS_dist.med, ymin = df1$BM.BF_FS_dist.min, ymax =  df1$BM.BF_FS_dist.max), col = "black") +
  ggplot2::xlab("Date") +
  ggplot2::ylab("Coef dissim BM-BF FS") +
  ggplot2::ggtitle(unique(df1$Site_bis)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none")

ggplot2::ggsave(paste0("bm_bf_", df1$Site, ".png"), device = "png", bm_bf_fs_plot, height = 3, width = 3.5)

}

rm(df1, four, i, one, three, two, xmax_, xmin_, ymax_, ymin_)

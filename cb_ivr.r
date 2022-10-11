# author: "Jonathan Richir"
# date: "19 April 2021"                     )

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

## load ivr data

ivr <- read.csv2(input_data, header = FALSE, fileEncoding = "Latin1")
names_ <- as.vector(unlist(ivr[1, ]))
names_ <- gsub(" ", ".", names_)
colnames(ivr) <- names_
ivr <- ivr[-1, ]
ivr <- ivr[, -17]

ivr_next <- read.csv2(input_data2, header = FALSE, fileEncoding = "Latin1")
names_ <- as.vector(unlist(ivr_next[1, ]))
names_ <- gsub(" ", ".", names_)
colnames(ivr_next) <- names_
rm(names_)
ivr_next <- ivr_next[-1, ]
ivr_next <- ivr_next[, -17]

# bind ivr dfs.

ivr <- dplyr::bind_rows(ivr, ivr_next)

# NB inversion between id and ID.Fiche variable names
ivr <- dplyr::rename(ivr, XX = ID.Fiche)
ivr <- dplyr::rename(ivr, ID.Fiche = id)
ivr <- dplyr::rename(ivr, id = XX)

rm(ivr_next)


## import excel files "Fiche terrain"

fiche <- read.csv2(fiche_val, fileEncoding = "Latin1") # fileEncoding = "Latin1" cfr é in variable names

fiche_next <- read.csv2(fiche_term, fileEncoding = "Latin1") # fileEncoding = "Latin1" cfr é in variable names

fiche <- dplyr::bind_rows(fiche, fiche_next)
date_fiche <- as.Date(stringr::str_sub(fiche$date.sortie, end = 10), origin = "1970-01-01")
fiche <- tibble::add_column(fiche, date_fiche, .after = "date.sortie")
rm(date_fiche)

rm(fiche_next)


## ivr vs fiche terrain

ivr$id <- as.numeric(ivr[, c("id")])

fiche_red <- dplyr::filter(fiche, fiche$ID.Fiche %in% unique(ivr[, c("id")]))

id_count <- ivr %>% dplyr::group_by(id) %>% dplyr::count()
id_count <- dplyr::rename(id_count, "ID.Fiche" = "id")
id_count <- dplyr::ungroup(id_count)
id_count <- as.data.frame(id_count)

fiche_red <- dplyr::left_join(fiche_red, id_count)

# rep fiche terrain information
fiche_expanded <- fiche_red[rep(row.names(fiche_red), fiche_red$n), 1:ncol(fiche_red)]
fiche_expanded <- dplyr::rename(fiche_expanded, "id" = "ID.Fiche")

## merge ivr data and ficheterrain information

ivr <- dplyr::bind_cols(ivr, fiche_expanded)
ivr <- dplyr::rename(ivr, "id.ivr" = "id...1")
ivr <- dplyr::rename(ivr, "id.fiche" = "id...17")

rm(fiche_expanded, fiche_red, id_count)

ivr <- ivr %>% tidyr::separate(date_fiche, c("Year", "Month", "Day"), sep = "-", remove = FALSE)


## I create two new variables for Site names, one for data analysis and one for data reporting. Only works for actual ivr df with 22 sites !

# Name for data analysis

ivr <- tibble::add_column(ivr, Site = NA, .after = "ID.Fiche")
unique(ivr$Site)

ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[1], "GDMO_Locmariaquer", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[2], "GDMO_BegLann", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[3], "FOUR_PlateauFour", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[4], "EGMP_GroinCou", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[5], "EGMP_PasEmsembert", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[6], "EGMP_BreeBains", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[7], "EGMP_PerreAntiochat", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[8], "EGMP_Chassiron", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[9], "BASQ_FlotsBleusZP", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[10], "BASQ_FlotsBleusZF", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[11], "GONB_IlotStMichel", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[12], "FINS_Quemenes", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[13], "FINS_SeinGoulenez", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[14], "FINS_SeinKilaourou", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[15], "ARMO_Verdelet", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[16], "ARMO_Piegu", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[17], "ARMO_Bilfot", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[18], "ARMO_IlePlate", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[19], "PDMO_Perharidy", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[20], "BRES_Keraliou", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[21], "FINS_Mousterlin", ivr$Site)
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[22], "FINS_StNicolasGlenan", ivr$Site)

unique(ivr$Site)

# Anne Boulet forgot to specify zone.habitat in 2020. I asked her to correct it in ESTAMP
ivr$Site <- ifelse(ivr$zone.habitat == unique(ivr$zone.habitat)[23], "GDMO_Locmariaquer", ivr$Site)
unique(ivr$Site)
unique(ivr[, c("Site", "zone.habitat")])

# Name for report/plot

ivr <- tibble::add_column(ivr, Site_bis = NA, .after = "Site")

ivr$Site_bis <- ifelse(ivr$Site == "GDMO_Locmariaquer", "Locmariaquer", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "GDMO_BegLann", "Beg Lann", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "FOUR_PlateauFour", "Plateau du Four", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "EGMP_GroinCou", "Groin du Cou", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "EGMP_PasEmsembert", "Le Pas d'Emsembert", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "EGMP_BreeBains", "La Brée-les-Bains", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "EGMP_PerreAntiochat", "Le Perré d'Antiochat", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "EGMP_Chassiron", "Chassiron", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "BASQ_FlotsBleusZP", "Les Flots Bleus / zone pêcheurs", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "BASQ_FlotsBleusZF", "Les Flots Bleus / zone familles", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "GONB_IlotStMichel", "Îlot Saint-Michel", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "FINS_Quemenes", "Quéménès", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "FINS_SeinGoulenez", "Île de Sein - Goulenez", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "FINS_SeinKilaourou", "Île de Sein - Kilaourou", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "ARMO_Verdelet", "Îlot du Verdelet", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "ARMO_Piegu", "Piégu", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "ARMO_Bilfot", "Pointe de Bilfot", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "ARMO_IlePlate", "Île Plate", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "PDMO_Perharidy", "Perharidy", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "BRES_Keraliou", "Keraliou", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "FINS_Mousterlin", "Pointe de Mousterlin", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "FINS_StNicolasGlenan", "Saint-Nicolas des Glénan", ivr$Site_bis)

unique(ivr[, c("Site", "Site_bis")])


## change some variable format to integer

ivr$Nb.Blocs.Non.Retournes <- as.integer(ivr$Nb.Blocs.Non.Retournes)
ivr$Nb.Blocs.Retournes <- as.integer(ivr$Nb.Blocs.Retournes)

ivr$Year <- as.integer(ivr$Year)
ivr$Month <- as.integer(ivr$Month)
ivr$Day <- as.integer(ivr$Day)
ivr$Numero.Quadrat <- as.integer(ivr$Numero.Quadrat)


## save the final, commplete ivr df.

ivr <- ivr[, c(19:54, 1:18)]

saveRDS(ivr, "ivr.RDS")


## percentage of unturned vs overturned boulders and IVR previous 0-5 discrete scale values calculation

# create two new variables first

site_year_month_day <- paste0(ivr$Site, ".", gsub("-", ".", as.character(ivr$date_fiche)))
ivr <- tibble::add_column(ivr, site_year_month_day, .after = "Site_bis")
rm(site_year_month_day)

site_year_month_day_qdnb <- paste0(ivr$Site, ".", gsub("-", ".", as.character(ivr$Date)), ".", ivr$Numero.Quadrat)
ivr <- tibble::add_column(ivr, site_year_month_day_qdnb, .after = "site_year_month_day")
rm(site_year_month_day_qdnb)

ivr <- dplyr::arrange(ivr, Site, Year, Month, Numero.Quadrat)

# remove data with NA value for Nb.Blocs.Retournes & Nb.Blocs.Non.Retournes
ivr_naomit <- ivr %>% dplyr::filter(!is.na(ivr$Nb.Blocs.Retournes))
ivr_naomit <- as.data.frame(ivr_naomit)
colnames(ivr_naomit) <- colnames(ivr)
ivr_naomit <- ivr_naomit %>% dplyr::filter(!is.na(ivr_naomit$Nb.Blocs.Non.Retournes))
ivr_naomit <- as.data.frame(ivr_naomit)

# also remove data with Nb.Blocs.Retournes = 0 & Nb.Blocs.Non.Retournes = 0, cfr equivalent to quadrat with no boulders ... makes no sense to consider quadrat without boulder for ivr determination.
ivr_rm <- dplyr::filter(ivr_naomit, ivr_naomit$Nb.Blocs.Retournes == 0 && ivr_naomit$Nb.Blocs.Non.Retournes == 0)
ivr_naomit <- ivr_naomit %>% dplyr::anti_join(ivr_rm)
rm(ivr_rm)

ivr_val_qu_ <- ivr_naomit

for (i in 1:nrow(ivr_naomit)) {
  (bm <- sum(ivr_naomit$Nb.Blocs.Non.Retournes[i], ivr_naomit$Nb.Blocs.Retournes[i]))
  (ivr_val_qu_$blocs.retournes.fr.[i] <- (ivr_naomit$Nb.Blocs.Retournes[i] / bm) * 100)
  (ivr_val_qu_$blocs.non.retournes.fr.[i]  <- (ivr_naomit$Nb.Blocs.Non.Retournes[i] / bm) * 100)
}

rm(bm, i)

ivr_val_qu_$blocs.non.retournes.fr. <- ifelse(is.nan(ivr_val_qu_$blocs.non.retournes.fr.), NA, ivr_val_qu_$blocs.non.retournes.fr.)
ivr_val_qu_$blocs.retournes.fr. <- ifelse(is.nan(ivr_val_qu_$blocs.retournes.fr.), NA, ivr_val_qu_$blocs.retournes.fr.)

# ivr for loop by quadrat.

for (i in 1:nrow(ivr_val_qu_)) {
  if (ivr_val_qu_$Nb.Blocs.Non.Retournes[i] == 0 && ivr_val_qu_$Nb.Blocs.Retournes[i] == 0) {
    ivr_ <- NA
  }else {
  if (ivr_val_qu_$blocs.retournes.fr.[i] < 5) {
    ivr_ <- 0
  } else if (ivr_val_qu_$blocs.retournes.fr.[i] >= 5 && ivr_val_qu_$blocs.retournes.fr.[i] < 25) {
    ivr_ <- 1
  } else if (ivr_val_qu_$blocs.retournes.fr.[i] >= 25 && ivr_val_qu_$blocs.retournes.fr.[i] < 45) {
    ivr_ <- 2
  } else if (ivr_val_qu_$blocs.retournes.fr.[i] >= 45 && ivr_val_qu_$blocs.retournes.fr.[i] < 65) {
    ivr_ <- 3
  } else if (ivr_val_qu_$blocs.retournes.fr.[i] >= 65 && ivr_val_qu_$blocs.retournes.fr.[i] < 85) {
    ivr_ <- 4
  } else {
    ivr_ <- 5
  }

  ivr_val_qu_$valeur.ivr_quadrat[i] <- ivr_
  }
}

rm(i, ivr_)

# reorder variables for logical purpose
ivr_val_qu_ <- ivr_val_qu_[, c(1:56, 58, 57, 59)]
indic_full <- ivr_val_qu_
saveRDS(ivr_val_qu_, "ivr_val_qu.RDS")

rm(ivr_naomit)


## Calculate ivr statistics now

ivr_val_qu_stat_ <- ivr_val_qu_ %>% dplyr::group_by(id.ivr, Site, Site_bis, Year, Month, Day) %>% dplyr::summarize(ivr_moy = mean(valeur.ivr_quadrat), ivr_et = sd(valeur.ivr_quadrat), ivr_med = median(valeur.ivr_quadrat), ivr_min = min(valeur.ivr_quadrat), ivr_max = max(valeur.ivr_quadrat), fr.r.moy = mean(blocs.retournes.fr.), fr.r.et = sd(blocs.retournes.fr.), fr.r.med = median(blocs.retournes.fr.), fr.r.min = min(blocs.retournes.fr.), fr.r.max = max(blocs.retournes.fr.), fr.nr.moy = mean(blocs.non.retournes.fr.), fr.nr.et = sd(blocs.non.retournes.fr.), fr.nr.med = median(blocs.non.retournes.fr.), fr.nr.min = min(blocs.non.retournes.fr.), fr.nr.max = max(blocs.non.retournes.fr.), nb. = dplyr::n())

Date <- as.Date(paste0(ivr_val_qu_stat_$Year, "-", ivr_val_qu_stat_$Month, "-", ivr_val_qu_stat_$Day), origin = "1970-01-01")
ivr_val_qu_stat_ <- tibble::add_column(ivr_val_qu_stat_, Date, .after = "Site_bis")
rm(Date)

ivr_val_qu_stat_ <- as.data.frame(ivr_val_qu_stat_)
indic <- ivr_val_qu_stat_
saveRDS(ivr_val_qu_stat_, "ivr_val_qu_stat.RDS")


## plot ivr (NB: Year, Month, Day variable names are replace by Annee, Mois, Jour, cfr previous label use in the script)

ivr_val_qu_stat_ <- dplyr::rename(ivr_val_qu_stat_, Annee = Year)
ivr_val_qu_stat_ <- dplyr::rename(ivr_val_qu_stat_, Mois = Month)
ivr_val_qu_stat_ <- dplyr::rename(ivr_val_qu_stat_, Jour = Day)
write.table(ivr_val_qu_stat_, "Valeurs_stat.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")
# old IVR scale with discrete 0 to 5 environmental status levels, plus other site data

for (i in c(1:length(unique(ivr_val_qu_stat_$Site)))) {
  ivr_val_eg <- dplyr::filter(ivr_val_qu_stat_, ivr_val_qu_stat_$Site == unique(ivr_val_qu_stat_$Site)[i])

  xmin_ <- as.Date(ifelse(min(ivr_val_eg$Annee) >= 2014, "2014-01-01", paste0(min(ivr_val_eg$Annee), "-01-01")), origin = "1970-01-01")
  xmax_ <- as.Date(ifelse(max(ivr_val_eg$Annee) <= 2017, "2018-01-01", #paste0(max(ivr_val_eg$Annee)
                          "2022-01-01")
                   #)
                   , origin = "1970-01-01")

  png(paste0("old_ivr_", unique(ivr_val_eg$Site), ".png"))
  plot(ivr_val_qu_stat_$Date, ivr_val_qu_stat_$ivr_med, xlim = c(xmin_, xmax_), ylim = c(-0.5, 5.5), pch = 19, main = unique(ivr_val_eg$Site), xlab = "Date", ylab = "IVR (médiane, min. et max de 5 quadrats)", col = "grey")

  points(ivr_val_eg$Date, ivr_val_eg$ivr_med, pch = 19, cex = 1.5)
  arrows(ivr_val_eg$Date, ivr_val_eg$ivr_med, ivr_val_eg$Date, ivr_val_eg$ivr_max, code = 3, angle = 90, length = 0.00)
  arrows(ivr_val_eg$Date, ivr_val_eg$ivr_med, ivr_val_eg$Date, ivr_val_eg$ivr_min, code = 3, angle = 90, length = 0.00)
}

rm(ivr_val_eg)

# new IVR scale with continuous 0 to 5 environmental status levels based on % of overturned boulders /20, plus other site data

par(mar = c(5, 4, 2, 2) + 0.1)

par(mfrow = c(1, 1))

for (i in c(1:length(unique(ivr_val_qu_stat_$Site)))) {
  ivr_val_eg <- dplyr::filter(ivr_val_qu_stat_, ivr_val_qu_stat_$Site == unique(ivr_val_qu_stat_$Site)[i])

  xmin_ <- as.Date(ifelse(min(ivr_val_eg$Annee) >= 2014, "2014-01-01", paste0(min(ivr_val_eg$Annee), "-01-01")), origin = "1970-01-01")
  xmax_ <- as.Date(ifelse(max(ivr_val_eg$Annee) <= 2017, "2018-01-01", "2022-01-01"), origin = "1970-01-01")

  png(paste0("new_ivr_", unique(ivr_val_eg$Site), ".png"))
  plot(ivr_val_eg$Date, ivr_val_eg$fr.r.moy / 20, xlim = c(xmin_, xmax_), ylim = c(0, 5), pch = 19, main = unique(ivr_val_eg$Site), xlab = "", ylab = "", type = "n", axes = FALSE)

  rect(as.Date("2013-01-01", origin = "1970-01-01"), 85 / 20, as.Date("2023-01-01", origin = "1970-01-01"), 5.5, col = "red", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), 65 / 20, as.Date("2023-01-01", origin = "1970-01-01"), 85 / 20, col = "orange1", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), 45 / 20, as.Date("2023-01-01", origin = "1970-01-01"), 65 / 20, col = "gold1", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), 25 / 20, as.Date("2023-01-01", origin = "1970-01-01"), 45 / 20, col = "yellow", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), 5 / 20, as.Date("2023-01-01", origin = "1970-01-01"), 25 / 20, col = "olivedrab", border = NA)
  rect(as.Date("2013-01-01", origin = "1970-01-01"), -0.5, as.Date("2023-01-01", origin = "1970-01-01"), 5 / 20, col = "blue", border = NA)

  par(new = TRUE)
  plot(ivr_val_qu_stat_$Date, ivr_val_qu_stat_$fr.r.moy / 20, xlim = c(xmin_, xmax_), ylim = c(0, 5), pch = 19, main = unique(ivr_val_eg$Site), xlab = "Date", ylab = "IVR", col = "grey")

  points(ivr_val_eg$Date, ivr_val_eg$fr.r.moy / 20, pch = 19, cex = 1.5)

  arrows(ivr_val_eg$Date, ivr_val_eg$fr.r.moy / 20 + ivr_val_eg$fr.r.et / 20, ivr_val_eg$Date, ivr_val_eg$fr.r.moy / 20 - ivr_val_eg$fr.r.et / 20, code = 3, angle = 90, length = 0.00)

  rm(ivr_val_eg, i, xmax_, xmin_)
}

report <- args[5]
loop_file <- source(args[6])

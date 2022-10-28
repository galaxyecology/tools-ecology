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

### Import data

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    fiche_val <- args[1]
    input_data <- args[2]

}

#############################################################
#                                                           #
#                Load and clean the data                    #
#                                                           #
#############################################################
### load ivr data

ivr <- read.csv2(input_data, header = FALSE, fileEncoding = "Latin1")
names_ <- as.vector(unlist(ivr[1, ]))
names_ <- gsub(" ", ".", names_)
colnames(ivr) <- names_
ivr <- ivr[-1, ]
ivr <- ivr[, -17]

# NB inversion between id and ID.Fiche variable names
ivr <- dplyr::rename(ivr, XX = ID.Fiche)
ivr <- dplyr::rename(ivr, ID.Fiche = id)
ivr <- dplyr::rename(ivr, id = XX)


### load excel files "Fiche terrain" the metadata

fiche <- read.csv2(fiche_val, fileEncoding = "Latin1") # fileEncoding = "Latin1" cfr é in variable names

date_fiche <- as.Date(stringr::str_sub(fiche$date.sortie, end = 10), origin = "1970-01-01")
fiche <- tibble::add_column(fiche, date_fiche, .after = "date.sortie")
rm(date_fiche)

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
ivr <- tibble::add_column(ivr, Site = ivr$zone.habitat, .after = "ID.Fiche")
ivr$Site <- gsub(pattern = " \\(champ de blocs\\)", replacement = "", ivr$Site)
ivr$Site <- gsub(pattern = " \\(champ blocs\\)", replacement = "", ivr$Site)

for (x in seq_along(ivr$Site)) {
  if (grepl(pattern = "Locmariaquer", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "GDMO_Locmariaquer"
 } else if (grepl(pattern = "Beg Lann", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "GDMO_BegLann"
 } else if (grepl(pattern = "Plateau du Four", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "FOUR_PlateauFour"
 } else if (grepl(pattern = "Grouin", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "EGMP_GroinCou"
 } else if (grepl(pattern = "Ensembert", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "EGMP_PasEmsembert"
 } else if (grepl(pattern = "Brée-les-Bains", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "EGMP_BreeBains"
 } else if (grepl(pattern = "Antiochat", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "EGMP_PerreAntiochat"
 } else if (grepl(pattern = "Chassiron", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "EGMP_Chassiron"
 } else if (grepl(pattern = "zone p", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "BASQ_FlotsBleusZP"
 } else if (grepl(pattern = "zone f", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "BASQ_FlotsBleusZF"
 } else if (grepl(pattern = "Saint-Michel", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "GONB_IlotStMichel"
 } else if (grepl(pattern = "Quéménès", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "FINS_Quemenes"
 } else if (grepl(pattern = "Goulenez", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "FINS_SeinGoulenez"
 } else if (grepl(pattern = "Kilaourou", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "FINS_SeinKilaourou"
 } else if (grepl(pattern = "Verdelet", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "ARMO_Verdelet"
 } else if (grepl(pattern = "Piégu", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "ARMO_Piegu"
 } else if (grepl(pattern = "Bilfot", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "ARMO_Bilfot"
 } else if (grepl(pattern = "Plate", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "ARMO_IlePlate"
 } else if (grepl(pattern = "Perharidy", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "PDMO_Perharidy"
 } else if (grepl(pattern = "Keraliou", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "BRES_Keraliou"
 } else if (grepl(pattern = "Mousterlin", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "FINS_Mousterlin"
 } else if (grepl(pattern = "Nicolas", ivr$Site[x]) == TRUE) {
    ivr$Site[x] <- "FINS_StNicolasGlenan"
 }
if (grepl(pattern = "Roz", ivr$site[x]) == TRUE) {
    ivr$Site[x] <- "FINS_AnseRoz"
}
}

# Name for report/plot

ivr <- tibble::add_column(ivr, Site_bis = ivr$Site, .after = "Site")

ivr$Site_bis <- ifelse(ivr$Site == "GDMO_Locmariaquer", "Locmariaquer", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "GDMO_BegLann", "Beg Lann", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "FOUR_PlateauFour", "Plateau du Four", ivr$Site_bis)
ivr$Site_bis <- ifelse(ivr$Site == "EGMP_GroinCou", "Grouin du Cou", ivr$Site_bis)
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
ivr$Site_bis <- ifelse(ivr$Site == "FINS_AnseRoz", "Pointe de l'Anse du Roz", ivr$Site_bis)

## change some variable format to integer
ivr$Nb.Blocs.Non.Retournes <- as.integer(ivr$Nb.Blocs.Non.Retournes)
ivr$Nb.Blocs.Retournes <- as.integer(ivr$Nb.Blocs.Retournes)

ivr$Year <- as.integer(ivr$Year)
ivr$Month <- as.integer(ivr$Month)
ivr$Day <- as.integer(ivr$Day)
ivr$Numero.Quadrat <- as.integer(ivr$Numero.Quadrat)


## save the final, commplete ivr df.

ivr <- ivr[, c(19:54, 1:18)]


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



#############################################################
#                                                           #
#                    Calcul of the IVR                      #
#                                                           #
#############################################################

### Percentage of turned boulder
for (i in 1:nrow(ivr_naomit)) {
  (bm <- sum(ivr_naomit$Nb.Blocs.Non.Retournes[i], ivr_naomit$Nb.Blocs.Retournes[i]))
  (ivr_val_qu_$blocs.retournes.fr.[i] <- (ivr_naomit$Nb.Blocs.Retournes[i] / bm) * 100)
  (ivr_val_qu_$blocs.non.retournes.fr.[i]  <- (ivr_naomit$Nb.Blocs.Non.Retournes[i] / bm) * 100)
}

rm(bm, i)


ivr_val_qu_$blocs.non.retournes.fr. <- ifelse(is.nan(ivr_val_qu_$blocs.non.retournes.fr.), NA, ivr_val_qu_$blocs.non.retournes.fr.)
ivr_val_qu_$blocs.retournes.fr. <- ifelse(is.nan(ivr_val_qu_$blocs.retournes.fr.), NA, ivr_val_qu_$blocs.retournes.fr.)


# ivr for loop by quadrat.
for (i in 1:seq_len(nrow(ivr_val_qu_))) {
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

rm(ivr_naomit)


## Calculate ivr statistics now
ivr_val_qu_stat_ <- ivr_val_qu_ %>% dplyr::group_by(id.ivr, Site, Site_bis, Year, Month, Day) %>% dplyr::summarize(ivr_moy = mean(valeur.ivr_quadrat), ivr_et = sd(valeur.ivr_quadrat), ivr_med = median(valeur.ivr_quadrat), ivr_min = min(valeur.ivr_quadrat), ivr_max = max(valeur.ivr_quadrat), fr.r.moy = mean(blocs.retournes.fr.), fr.r.et = sd(blocs.retournes.fr.), fr.r.med = median(blocs.retournes.fr.), fr.r.min = min(blocs.retournes.fr.), fr.r.max = max(blocs.retournes.fr.), fr.nr.moy = mean(blocs.non.retournes.fr.), fr.nr.et = sd(blocs.non.retournes.fr.), fr.nr.med = median(blocs.non.retournes.fr.), fr.nr.min = min(blocs.non.retournes.fr.), fr.nr.max = max(blocs.non.retournes.fr.), nb. = dplyr::n())

Date <- as.Date(paste0(ivr_val_qu_stat_$Year, "-", ivr_val_qu_stat_$Month, "-", ivr_val_qu_stat_$Day), origin = "1970-01-01")
ivr_val_qu_stat_ <- tibble::add_column(ivr_val_qu_stat_, Date, .after = "Site_bis")
rm(Date)

ivr_val_qu_stat_ <- as.data.frame(ivr_val_qu_stat_)
indic <- ivr_val_qu_stat_
saveRDS(ivr_val_qu_stat_, "ivr_val_qu_stat.RDS")


#############################################################
#                                                           #
#                  Plot the IVR per site                    #
#                                                           #
#############################################################

## plot ivr (NB: Year, Month, Day variable names are replace by Annee, Mois, Jour, cfr previous label use in the script)
ivr_val_qu_stat_ <- dplyr::rename(ivr_val_qu_stat_, Annee = Year)
ivr_val_qu_stat_ <- dplyr::rename(ivr_val_qu_stat_, Mois = Month)
ivr_val_qu_stat_ <- dplyr::rename(ivr_val_qu_stat_, Jour = Day)


# new IVR scale with continuous 0 to 5 environmental status levels based on % of overturned boulders /20, plus other site data

for (i in c(1:length(unique(ivr_val_qu_stat_$Site)))) {

  ivr_val_eg <- dplyr::filter(ivr_val_qu_stat_, ivr_val_qu_stat_$Site == unique(ivr_val_qu_stat_$Site)[i])

  ivr_plot <- ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = ivr_val_qu_stat_$Date, y = ivr_val_qu_stat_$fr.r.moy / 20), col = "grey") +
  ggplot2::geom_rect(ggplot2::aes(xmin = min(ivr_val_qu_stat_$Date), xmax = max(ivr_val_qu_stat_$Date), ymin = - 0.5, ymax = 5 / 20, fill = "#FF0000"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = min(ivr_val_qu_stat_$Date), xmax = max(ivr_val_qu_stat_$Date), ymin = 5 / 20, ymax = 25 / 20, fill = "#F59404"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = min(ivr_val_qu_stat_$Date), xmax = max(ivr_val_qu_stat_$Date), ymin = 25 / 20, ymax = 45 / 20, fill = "#FAFA15"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = min(ivr_val_qu_stat_$Date), xmax = max(ivr_val_qu_stat_$Date), ymin = 45 / 20, ymax = 65 / 20, fill = "#18E125"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = min(ivr_val_qu_stat_$Date), xmax = max(ivr_val_qu_stat_$Date), ymin = 65 / 20, ymax = 85 / 20, fill = "#04F5F5"), alpha = 0.3) +
  ggplot2::geom_rect(ggplot2::aes(xmin = min(ivr_val_qu_stat_$Date), xmax = max(ivr_val_qu_stat_$Date), ymin = 85 / 20, ymax = 5.5, fill = "#1A1AE8"), alpha = 0.3) +
  ggplot2::scale_fill_manual(values = c("#F59404", "#FAFA15", "#FF0000", "#04F5F5", "#18E125", "#1A1AE8")) +
  ggplot2::geom_pointrange(ggplot2::aes(x = ivr_val_eg$Date, y = ivr_val_eg$fr.r.moy / 20, ymin = ivr_val_eg$fr.r.moy / 20 - ivr_val_eg$fr.r.et / 20, ymax = ivr_val_eg$fr.r.moy / 20 + ivr_val_eg$fr.r.et / 20), col = "black") +
  ggplot2::xlab("Date") +
  ggplot2::ylab("IVR") +
  ggplot2::ggtitle(unique(ivr_val_eg$Site_bis)) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), legend.position = "none")

ggplot2::ggsave(paste0("ivr_", unique(ivr_val_eg$Site), ".png"), ivr_plot, height = 3, width = 3.5)


}

report <- args[3]
loop_file <- source(args[4])

#Rscript

#####################################################################################################################
#####################################################################################################################
###################################### Create a plot from your community data #######################################
#####################################################################################################################
#####################################################################################################################

###################### Packages
suppressMessages(library(ggplot2))
suppressMessages(library(boot))

###################### Load arguments and declaring variables

args <- commandArgs(trailingOnly = TRUE)


if (length(args) < 2) {
    stop("At least 3 arguments must be supplied input dataset file with GLM results (.tabular)", call. = FALSE) #if no args -> error and exit1

} else {
    import_data <- args[1] ###### file name : glm results table
    data_tab <- args[2] ###### file name : Metrics table
    unitobs_tab <- args[3] ###### file name : Unitobs table
    source(args[4]) ###### Import functions

}

#Import data
glmtable <- read.table(import_data, sep = "\t", dec = ".", header = TRUE, encoding = "UTF-8") #
datatable <- read.table(data_tab, sep = "\t", dec = ".", header = TRUE, encoding = "UTF-8") #
unitobs <- read.table(unitobs_tab, sep = "\t", dec = ".", header = TRUE, encoding = "UTF-8") #

#Check files

vars_data1 <- c("analysis", "Interest.var", "distribution")
err_msg_data1 <- "The input GLM results dataset doesn't have the right format. It needs to have at least the following 3 fields :\n- analysis\n- Interest.var\n- distribution\n"
check_file(glmtable, err_msg_data1, vars_data1, 4)

if (length(grep("[0-2][0|9][0-9][0-9].[Estimate|Pvalue]", colnames(glmtable))) == 0) {
    stop("The input GLM results dataset doesn't have the right format or informations. This tool is to represent temporal trends, if your GLM doesn't take the year variable as a fixed effect this tool is not proper to make any representation of it. It needs to have at least estimates and p-value for every year from your time series GLM as columns with name formated as : yyyy Estimate (example : 2020 Estimate) and  yyyy Pvalue (example : 2020 Pvalue).")
}

if (length(grep("[0-2][0|9][0-9][0-9].IC_[up|inf]", colnames(glmtable))) == 0) {
    assess_ic <- FALSE
}else{
    assess_ic <- TRUE
}

metric <- as.character(glmtable[1, "Interest.var"])

vars_data2 <- c("observation.unit", "location", metric)
err_msg_data2 <- "The input metrics dataset doesn't have the right format. It needs to have at least the following 3 fields :\n- observation.unit\n- location\n- the name of the interest metric\n"
check_file(datatable, err_msg_data2, vars_data2, 4)

vars_data3 <- c("observation.unit", "year")
err_msg_data3 <- "The input unitobs dataset doesn't have the right format. It needs to have at least the following 2 fields :\n- observation.unit\n- year\n"
check_file(unitobs, err_msg_data3, vars_data3, 2)
if (length(grep("[0-2][0|9][0-9][0-9]", unitobs$year)) == 0) {
    stop("The year column in the input unitobs dataset doesn't have the right format. Years must be fully written as : yyyy (example : 2020).")
}

if (all(is.na(match(datatable[, "observation.unit"], unitobs[, "observation.unit"])))) {
    stop("Observation units doesn't match in the inputs metrics dataset and unitobs dataset")
}

####################################################################################################################
######################### Creating plot from time series GLM data ## Function : ggplot_glm #########################
####################################################################################################################
ggplot_glm <- function(glmtable, datatable, unitobs, metric = metric, sp, description = TRUE,
                       trend_on_graph = TRUE, assess_ic = TRUE) {
    ## Purpose: Creating plot from time series GLM data
    ## ----------------------------------------------------------------------
    ## Arguments: glmtable : GLM(s) results table
    ##            datatable : Metrics table
    ##            unitobs : Unitobs table
    ##            metric : Interest variable in GLM(s)
    ##            sp : name of processed GLM
    ##            description : Two graphs ?
    ##            trend_on_graph : Write global trend of the time series on graph ?
    ##            assess_ic : Assess confidence intervals ?
    ## ----------------------------------------------------------------------
    ## Author: Coline ROYAUX 13 october 2020

    s_signif <- 0.05 ## threshold when pvalue is considered significant
    distrib <- as.character(glmtable[1, "distribution"]) ## extract GLM distribution

    col <- c("observation.unit", "location", metric) ## names of needed columns in metrics table to construct the 2nd panel of the graph

    if (colnames(glmtable)[length(glmtable)] == "separation") { ## if GLM is a community analysis
        cut <- as.character(glmtable[1, "separation"])
        if (cut != "None") { ## if there is plural GLM analysis performed
            if (! is.element(as.character(cut), colnames(unitobs))) {
                stop("The input unitobs dataset doesn't have the right format. If plural GLM analysis have been performed it needs to have the field used as separation factor.")
            }
            datatable <- cbind(datatable[, col], unitobs[match(datatable[, "observation.unit"], unitobs[, "observation.unit"]), c("year", cut)]) ## extracting 'year' and analysis separation factor columns from unitobs table to merge with metrics table /// Matching lines with 'observation.unit' column
            colnames(datatable) <- c(col, "year", cut)
        }else{
            datatable <- cbind(datatable[, col], unitobs[match(datatable[, "observation.unit"], unitobs[, "observation.unit"]), "year"]) ## extracting 'year' column from unitobs table to merge with metrics table /// Matching lines with 'observation.unit' column
            colnames(datatable) <- c(col, "year")
        }

    }else{ ## GLM is a population analysis
        cut <- "species.code"
        if (! is.element("species.code", colnames(datatable))) {
            stop("The input metrics dataset or the GLM results dataset doesn't have the right format. If the GLM is a population analysis, the field species.code needs to be informed in the metrics dataset. If the GLM is a community analysis the field separation (None if only one analysis and name of the separation factor if several analysis) needs to be informed in the last column of the GLM results dataset.")
        }
        col <- c(col, cut)
        datatable <- cbind(datatable[, col], unitobs[match(datatable[, "observation.unit"], unitobs[, "observation.unit"]), "year"]) ## extracting 'year' column from unitobs table to merge with metrics table /// Matching lines with 'observation.unit' column
        colnames(datatable) <- c(col, "year")
    }

    ##vpan vector of names of the two panels in the ggplot

    switch(as.character(metric),
           "number" = vpan <- c("Abundance variation", "Raw abundance"),
           "presence_absence" = vpan <- c("Presence-absence variation", "% presence in location"),
           vpan <- c(paste(metric, " variation"), paste("Mean ", metric)))

    ##Cut table for 1 analysis
    glmtab <- glmtable[glmtable[, "analysis"] == sp, ]

    glmtab <- glmtab[, grep("FALSE", is.na(glmtab[1, ]))] ## Supress columns with NA only

    ## specification of temporal variable necessary for the analyses
    an <- as.numeric(unlist(strsplit(gsub("X", "", paste(colnames(glmtab)[grep("[0-2][0|9][0-9][0-9].Estimate", colnames(glmtab))], collapse = " ")), split = ".Estimate"))) ## Extract list of years studied in the GLM

    year <- sort(c(min(an) - 1, an)) ## Add the first level, "reference" in the GLM
    nbans <- length(year)
    pasdetemps <- nbans - 1

    ##### Table 1

    coefan <- unlist(lapply(an, FUN = function(x) {
                                          if (length(glmtab[glmtab[, 1] == sp, grep(paste0("X", x, ".Estimate"), colnames(glmtab))]) > 0) {
                                              glmtab[glmtab[, 1] == sp, grep(paste0("X", x, ".Estimate"), colnames(glmtab))]
                                          }else{
                                              NA
                                          }
                                                 }))  ## extract estimates for each years to contruct graph 1

    ic_inf <- unlist(lapply(an, FUN = function(x) {
                                          if (length(glmtab[glmtab[, 1] == sp, grep(paste0("X", x, ".IC_inf"), colnames(glmtab))]) > 0) {
                                              glmtab[glmtab[, 1] == sp, grep(paste0("X", x, ".IC_inf"), colnames(glmtab))]
                                          }else{
                                               NA
                                          }
                                                  }))

    ic_up <- unlist(lapply(an, FUN = function(x) {
                                         if (length(glmtab[glmtab[, 1] == sp, grep(paste0("X", x, ".IC_up"), colnames(glmtab))]) > 0) {
                                             glmtab[glmtab[, 1] == sp, grep(paste0("X", x, ".IC_up"), colnames(glmtab))]
                                         }else{
                                             NA
                                         }
                                                }))

    switch(distrib,  ## Applying the reciprocal of the link function to coefficients and confidence intervals depending on distribution law
           "poisson" = {
                            coefyear <- c(1, exp(as.numeric(coefan))) ## link function : log
                            if (assess_ic) {
                                ic_inf_sim <- c(1, exp(as.numeric(ic_inf)))
                                ic_sup_sim <- c(1, exp(as.numeric(ic_up)))
                            } else {
                                ic_inf_sim <- NA
                                ic_sup_sim <- NA
                      }},
           "quasipoisson" = {
                                 coefyear <- c(1, exp(as.numeric(coefan))) ## link function : log
                                 if (assess_ic) {
                                     ic_inf_sim <- c(1, exp(as.numeric(ic_inf)))
                                     ic_sup_sim <- c(1, exp(as.numeric(ic_up)))
                                 } else {
                                     ic_inf_sim <- NA
                                     ic_sup_sim <- NA
                           }},
           "inverse.gaussian" = {
                                     coefyear <- c(1, as.numeric(coefan)^( - 1 / 2)) ## link function : x^ - 2
                                     if (assess_ic) {
                                         ic_inf_sim <- c(1, as.numeric(ic_inf)^( - 1 / 2))
                                         ic_sup_sim <- c(1, as.numeric(ic_up)^( - 1 / 2))
                                     } else {
                                         ic_inf_sim <- NA
                                         ic_sup_sim <- NA
                               }},
           "binomial" = {
                             coefyear <- c(1, inv.logit(as.numeric(coefan))) ## link function : logit
                             if (assess_ic) {
                                 ic_inf_sim <- c(1, inv.logit(as.numeric(ic_inf)))
                                 ic_sup_sim <- c(1, inv.logit(as.numeric(ic_up)))
                             } else {
                                 ic_inf_sim <- NA
                                 ic_sup_sim <- NA
                       }},
           "quasibinomial" = {
                                  coefyear <- c(1, inv.logit(as.numeric(coefan))) ## link function : logit
                                  if (assess_ic) {
                                      ic_inf_sim <- c(1, inv.logit(as.numeric(ic_inf)))
                                      ic_sup_sim <- c(1, inv.logit(as.numeric(ic_up)))
                                  } else {
                                      ic_inf_sim <- NA
                                      ic_sup_sim <- NA
                            }},
           "Gamma" = {
                          coefyear <- c(1, as.numeric(coefan)^( - 1)) ## link function : -x^ - 1
                          if (assess_ic) {
                              ic_inf_sim <- c(1, as.numeric(ic_inf)^( - 1))
                              ic_sup_sim <- c(1, as.numeric(ic_up)^( - 1))
                          } else {
                              ic_inf_sim <- NA
                              ic_sup_sim <- NA
                    }},
           {
                coefyear <- c(1, as.numeric(coefan))
                if (assess_ic) {
                    ic_inf_sim <- c(1, as.numeric(ic_inf))
                    ic_sup_sim <- c(1, as.numeric(ic_up))
                } else {
                    ic_inf_sim <- NA
                    ic_sup_sim <- NA
            }})

    pval <- c(1, unlist(lapply(an, FUN = function(x) {
                                             if (length(glmtab[glmtab[, 1] == sp, grep(paste0("X", x, ".Pvalue"), colnames(glmtab))]) > 0) {
                                                 glmtab[glmtab[, 1] == sp, grep(paste0("X", x, ".Pvalue"), colnames(glmtab))]
                                              }else{
                                                  NA
                                              }
                                                    }))) ## extract p value for each year


    tab1 <- data.frame(year, val = coefyear,  ## table for the graphical output 1
                       LL = unlist(ic_inf_sim), UL = unlist(ic_sup_sim),
                       catPoint = ifelse(pval<s_signif, "significatif", NA), pval,
                       courbe = vpan[1],
                       panel = vpan[1])
    ## cleaning of wrong or biaised measures of the confidence interval
    if (assess_ic) {
        tab1$UL <-  ifelse(tab1$UL == Inf, NA, tab1$UL)
        tab1$UL <-  ifelse(tab1$UL > 1.000000e+20, NA, tab1$UL)
        tab1$UL[1] <- 1
        tab1$val <-  ifelse(tab1$val > 1.000000e+20, 1.000000e+20, tab1$val)
    }

    coefancontinu <- as.numeric(as.character(glmtab[glmtab[, 1] == sp, grep("year.Estimate", colnames(glmtab))])) ##  tendency of population evolution on the studied period = regression coefficient of the variable year as a continuous variable in the GLM
    switch(distrib, ## Applying the reciprocal of the link function to coefficients and confidence intervals depending on distribution law
           "poisson" = {
                           trend <- round(exp(as.numeric(coefancontinu)), 3) ## link function : log
                            pourcentage <- round((exp(as.numeric(coefancontinu) * as.numeric(pasdetemps)) - 1) * 100, 2)
                       },
           "quasipoisson" = {
                                 trend <- round(exp(as.numeric(coefancontinu)), 3) ## link function : log
                                 pourcentage <- round((exp(as.numeric(coefancontinu) * as.numeric(pasdetemps)) - 1) * 100, 2)
                            },
           "inverse.gaussian" = {
                                     trend <- round(as.numeric(coefancontinu)^( - 1 / 2), 3) ## link function : x^ - 2
                                     pourcentage <- round((((as.numeric(coefancontinu) * as.numeric(pasdetemps))^( - 1 / 2)) - 1) * 100, 2)
                                },
           "binomial" = {
                             trend <- round(inv.logit(as.numeric(coefancontinu)), 3) ## link function : logit
                             pourcentage <- round((inv.logit(as.numeric(coefancontinu) * as.numeric(pasdetemps)) - 1) * 100, 2)
                        },
           "quasibinomial" = {
                                  trend <- round(inv.logit(as.numeric(coefancontinu)), 3) ## link function : logit
                                  pourcentage <- round((inv.logit(as.numeric(coefancontinu) * as.numeric(pasdetemps)) - 1) * 100, 2)
                             },
           "Gamma" = {
                          trend <- round(as.numeric(coefancontinu)^( - 1), 3) ## link function : -x^ - 1
                          pourcentage <- round((((as.numeric(coefancontinu) * as.numeric(pasdetemps))^( - 1)) - 1) * 100, 2)
                     },
           {
                trend <- round(as.numeric(coefancontinu), 3)
                pourcentage <-  round((((as.numeric(coefancontinu) * as.numeric(pasdetemps))) - 1) * 100, 2)
           })

    pval <- as.numeric(as.character(glmtab[glmtab[, 1] == sp, grep("year.Pvalue", colnames(glmtab))])) ## Extract p value

    ## table for global trend on the whole time series
    tab1t <- NULL
    if (length(pval) > 0) {
        tab1t <- data.frame(Est = trend, pourcent = pourcentage, signif = pval<s_signif, pval)
    }

    ##### Table 2

    if (sp == "global") { ## prepare metrics table to extract variables used in graph 2
        datatablecut <- datatable[grep("FALSE", is.na(datatable[, as.character(metric)])), ]

    }else{

        datatablecut <- datatable[datatable[, as.character(cut)] == sp, ]
        datatablecut <- datatablecut[grep("FALSE", is.na(datatablecut[, as.character(metric)])), ]
    }

    switch(as.character(metric), ## Different value represented graph 2 depending on the nature of the metric
           "number" = {
                           valplot <- lapply(sort(year), FUN = function(x) {
                                                                   sum(na.omit(as.numeric(subset(datatablecut, year == x)[, as.character(metric)])))
                                                                           })
                      }, ## sum if abundance
           "presence_absence" = {
                                     nb_loc <- lapply(sort(year), FUN = function(x) {
                                                                            length(unique(subset(datatablecut, year == x)[, "location"]))
                                                                                    }) ## number of plots per year
                                     nb_loc_presence <- lapply(sort(year), FUN = function(x) {
                                                                                     length(unique(subset(datatablecut[datatablecut[, metric] > 0, ], year == x)[, "location"]))
                                                                                             }) ##  number of plots where the species were observed
                                     valplot <- (na.omit(as.numeric(nb_loc_presence)) / na.omit(as.numeric(nb_loc))) * 100
                                }, ## % of presence in observered plots if presence / absence
           {
                valplot <- lapply(sort(year), FUN = function(x) {
                                                        mean(na.omit(as.numeric(subset(datatablecut, year == x)[, as.character(metric)])))
                                                                })
           } ## mean if any other metric
          )

    tab2 <- data.frame(year, val = round(as.numeric(valplot), 2), LL = NA, UL = NA, catPoint = NA, pval = NA,
                       courbe = vpan[2], panel = vpan[2])

    ## Creating ggplots

    dgg <- tab1

    figname<- paste(sp, ".png", sep = "")

    ## coord for horizontal lines in graphs
    hline.data1 <- data.frame(z = c(1), panel = c(vpan[1]), couleur = "var estimates", type = "var estimates")
    hline.data3 <- data.frame(z = 0, panel = vpan[2], couleur = "seuil", type = "seuil")
    hline.data <- rbind(hline.data1, hline.data3)
    titre <- paste(sp)

    ## text for the population evolution trend

    pasdetemps <- max(dgg$year) - min(dgg$year) + 1
    if (! is.null(tab1t)) {
        if (assess_ic) {
            txt_pente1 <- paste("Global trend : ", tab1t$Est,
                               ifelse(tab1t$signif, " *", ""),
                               ifelse(tab1t$signif, paste("\n", ifelse(tab1t$pourcent>0, "+ ", "- "),
                                                         abs(tab1t$pourcent), " % in ", pasdetemps, " years", sep = ""), ""), sep = "")
        }else{
            txt_pente1 <- ifelse(tab1t$signif, paste("\n", ifelse(tab1t$pourcent>0, "+ ", "- "),
                                                   abs(tab1t$pourcent), " % in ", pasdetemps, " years", sep = ""), "")
        }
    }else{
        trend_on_graph <- FALSE
    }

    ## table of the text for the population evolution trend
    tab_text_pent <- data.frame(y = c(max(c(dgg$val, dgg$UL), na.rm = TRUE) * .9),
                              x = median(dgg$year),
                              txt = ifelse(trend_on_graph, c(txt_pente1), ""),
                              courbe = c(vpan[1]), panel = c(vpan[1]))

    dgg <- rbind(tab1, tab2)

    ## colors for plots
    vec_col_point <- c("#ffffff", "#eeb40f", "#ee0f59")
    names(vec_col_point) <- c("significatif", "infSeuil", "0")
    vec_col_courbe <- c("#3c47e0", "#5b754d", "#55bb1d", "#973ce0")
    names(vec_col_courbe) <- c(vpan[1], "loc", "presence", vpan[2])
    vec_col_hline <- c("#ffffff", "#e76060")
    names(vec_col_hline) <- c("var estimates", "seuil")

    col <- c(vec_col_point, vec_col_courbe, vec_col_hline)
    names(col) <- c(names(vec_col_point), names(vec_col_courbe), names(vec_col_hline))

    if (description) { ## if 2 panels
        p <- ggplot(data = dgg, mapping = aes(x = year, y = val))
        ## Titles and scales
        p <- p + facet_grid(panel ~ ., scale = "free") +
        theme(legend.position = "none",
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  +
        ylab("") + xlab("year")+ ggtitle(titre) +
        scale_colour_manual(values = col, name = "",
                                  breaks = names(col))+
        scale_x_continuous(breaks = min(dgg$year):max(dgg$year))
        p <- p + geom_hline(data =hline.data, mapping = aes(yintercept = z, colour = couleur, linetype = type ),
                        alpha = 1, size = 1.2)
        if (assess_ic) { ############# ONLY FOR THE CONFIDENCE INTERVAL
            p <- p + geom_ribbon(mapping = aes(ymin = LL, ymax = UL), fill = col[vpan[1]], alpha = .2)
            p <- p + geom_pointrange(mapping= aes(y = val, ymin = LL, ymax = UL), fill = col[vpan[1]], alpha = .2)
        }

        p <- p + geom_line(mapping = aes(colour = courbe), size = 1.5)
        p <- p + geom_point(mapping = aes(colour = courbe), size = 3)
        p <- p + geom_point(mapping = aes(colour = catPoint, alpha = ifelse(!is.na(catPoint), 1, 0)), size = 2)
        p <- p + geom_text(data = tab_text_pent, mapping = aes(x, y, label = txt), parse = FALSE, color = col[vpan[1]], fontface = 2, size = 4)
        ggsave(figname, p, width = 16, height = 15, units = "cm")

    } else {

        p <- ggplot(data = subset(dgg, panel == "var estimates"), mapping = aes(x = year, y = val))

        ## Titles and scales

        p <- p + facet_grid(panel ~ ., scale = "free") +
                 theme(legend.position = "none",
                       panel.grid.minor = element_blank(),
                       panel.grid.major.y = element_blank(),
                       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))  +
                 ylab("") + xlab("year")+ ggtitle(titre) +
                 scale_colour_manual(values = col, name = "",
                                     breaks = names(col))+
                 scale_x_continuous(breaks = min(dgg$year):max(dgg$year))
        p <- p + geom_hline(data =subset(hline.data, panel == vpan[1]), mapping = aes(yintercept = z, colour = couleur, linetype = type ),
                            alpha = 1, size = 1.2)

        if (assess_ic) { ############# ONLY FOR THE CONFIDENCE INTERVAL
            p <- p + geom_ribbon(mapping = aes(ymin = LL, ymax = UL), fill = col[vpan[1]], alpha = .2)
            p <- p + geom_pointrange(mapping= aes(y = val, ymin = LL, ymax = UL), fill = col[vpan[1]], alpha = .2)
	}

        p <- p + geom_line(mapping = aes(colour = courbe), size = 1.5)
        p <- p + geom_point(mapping = aes(colour = courbe), size = 3)
        p <- p + geom_point(mapping = aes(colour = catPoint, alpha = ifelse(!is.na(catPoint), 1, 0)), size = 2)
        p <-  p + geom_text(data = tab_text_pent, mapping = aes(x, y, label = txt), parse = FALSE, color = col[vpan[1]], fontface = 2, size = 4)
        ggsave(figname, p, width = 15, height = 9, units = "cm")
    }
    #return(p)
}
############################################################################################################ fin fonction graphique / end of function for graphical output

################# Analysis
#plots <- list()

for (sp in glmtable[, 1]) {

    if (!all(is.na(glmtable[glmtable[, 1] == sp, 4:(length(glmtable) - 1)]))) { ##ignore lines with only NA
        ggplot_glm(glmtable = glmtable, datatable = datatable, unitobs = unitobs, metric = metric, sp = sp, description = TRUE, trend_on_graph = TRUE, assess_ic = assess_ic)
    }
}

sink("stdout.txt", split = TRUE)

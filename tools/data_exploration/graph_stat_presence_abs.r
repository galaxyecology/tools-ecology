#Rscript

################################
##    Median and dispersion   ##
################################

#####Packages : Cowplot
#               ggplot2

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    stop("This tool needs at least one argument")
}else{
    table <- args[1]
    hr <- args[2]
    var <- as.numeric(args[3])
    spe <- as.numeric(args[4])
    loc <- as.numeric(args[5])
    time <-as.numeric(args[6])
}

if (hr == "false") {
  hr <- FALSE
}else{
  hr <- TRUE
}

#####Import data
data <- read.table(table, sep = "\t", dec = ".", header = hr, fill = TRUE, encoding = "UTF-8")
data <- na.omit(data)
colvar <- colnames(data)[var]
colspe <- colnames(data)[spe]
colloc <- colnames(data)[loc]
coltime <- colnames(data)[time]

data <- data[grep("^$", data[, spe], invert = TRUE), ]
time <- as.integer(substring(data[, time], first = 1, last = 4))

#####Your analysis

####Median and data dispersion####

#Median
graph_median <- function(data, var) {
  graph_median <- ggplot2::ggplot(data, ggplot2::aes_string(y = var)) +
  ggplot2::geom_boxplot(color = "darkblue") +
  ggplot2::theme(legend.position = "none") + ggplot2::ggtitle("Median")

return(graph_median)

}

#Dispersion
dispersion <- function(data, var, var2) {
  graph_dispersion <- ggplot2::ggplot(data) +
  ggplot2::geom_point(ggplot2::aes_string(x = var2, y = var, color = var2)) +
  ggplot2::scale_fill_brewer(palette = "Set3") +
  ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold")) + ggplot2::ggtitle("Dispersion")

return(graph_dispersion)

}

#The 2 graph
med_disp <- function(med, disp) {
  graph <- cowplot::plot_grid(med, disp, ncol = 1, nrow = 2, vjust = -5,  scales = "free")

  ggplot2::ggsave("Med_Disp.png", graph, width = 12, height = 20, units = "cm")
}


#### Zero problem in data ####

#Put data in form
makeTableAnalyse <- function(data, var, spe, var2, var3) {
    tab <- reshape(data
                  , v.names = var     
                  , idvar = c(var2, var3)
                  , timevar = spe
                  , direction = "wide")
    tab[is.na(tab)] <- 0 ###### remplace les na par des 0 / replace NAs by 0 

    colnames(tab) <- sub(var, "", colnames(tab))### remplace le premier pattern "abond." par le second "" / replace the column names "abond." by ""
    return(tab)
}
data_num <- makeTableAnalyse(data, colvar, colspe, colloc, coltime)
nb_spe <- length(unique(data[, spe]))
nb_col <- ncol(data_num) - nb_spe + 1
data_num <- data_num[, nb_col:ncol(data_num)]

#Presence of zeros in the data
mat_corr <- function(data) {cor(data)}
p.mat <- function(data) {ggcorrplot::cor_pmat(data)} # compute a matrix of correlation p-values

graph_corr <- function(data_num) {
  graph <- ggcorrplot::ggcorrplot(mat_corr(data_num), method = "circle", p.mat = p.mat(data_num), #barring the no significant coefficient
  ggtheme = ggplot2::theme_gray, colors = c("#00AFBB", "#E7B800", "#FC4E07"))

  ggplot2::ggsave("0_pb.png", graph)
}

##Med and disp
med <- graph_median(data, var = colvar)
disp <- dispersion(data, var = colvar, var2 = colspe)
med_disp(med = med, disp = disp)

##O problem
graph_corr(data_num)

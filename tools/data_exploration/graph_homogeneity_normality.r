#Rscript

#######################################
##     Homogeneity and normality     ##
#######################################

#####Packages : car
#               ggplot2
#               ggpubr
#               Cowplot

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    stop("This tool needs at least one argument")
}else{
    table <- args[1]
    hr <- args[2]
    date <- as.numeric(args[3])
    spe <- as.numeric(args[4])
    var <- as.numeric(args[5])
}

if (hr == "false") {
  hr <- FALSE
}else{
  hr <- TRUE
}

#####Import data
data <- read.table(table, sep = "\t", dec = ".", header = hr, fill = TRUE, encoding = "UTF-8")
data <- na.omit(data)
coldate <- colnames(data)[date]
colspe <- colnames(data)[spe]
colvar <- colnames(data)[var]

#####Your analysis

####Homogeneity of the variance####

##Test of Levene##
testlevene <- function(data, col1, col2) {
        data[, col1] <- as.numeric(data[, col1])
        data[, col2] <- as.factor(data[, col2])
    tb_levene <- car::leveneTest(y = data[, col1], group = data[, col2])

    return(tb_levene)
    }
levene <- capture.output(testlevene(data = data, col1 = colvar, col2 = colspe))

cat("\nwrite table with levene test. \n--> \"", paste(levene, "\"\n", sep = ""), file = "levene.txt", sep = "", append = TRUE)

##Two boxplots to visualize it##

homog_var <- function(data, col1, col2, col3, mult) {
    data[, col1] <- as.factor(data[, col1])
    if (mult) {
            for (spe in unique(data[, col2])) {
             data_cut <- data[data[, col2] == spe, ]
             graph_2 <- ggplot2::ggplot(data_cut, ggplot2::aes_string(x = col1, y = col3, color = col1)) +
             ggplot2::geom_boxplot() +
             ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), panel.background = ggplot2::element_rect(fill = "#d9d4c5", colour = "#d9d4c5", linetype = "solid"),
              panel.grid.major = ggplot2::element_line(linetype = "solid", colour = "white"),
              panel.grid.minor = ggplot2::element_line(linetype = "solid", colour = "white"))

            ggplot2::ggsave(paste("Homogeneity_of_", spe, ".png"), graph_2, width = 16, height = 9, units = "cm")
            }
        }else{
        graph_1 <- ggplot2::ggplot(data, ggplot2::aes_string(x = col1, y = col3, color = col1)) +
          ggplot2::geom_boxplot() +
          ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

    #Put multiple panels
    graph_2 <- graph_1 + ggplot2::facet_grid(rows = ggplot2::vars(data[, col2]), scales = "free") +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#d9d4c5", colour = "#d9d4c5", linetype = "solid"),
          panel.grid.major = ggplot2::element_line(linetype = "solid", colour = "white"),
          panel.grid.minor = ggplot2::element_line(linetype = "solid", colour = "white"))

    ggplot2::ggsave("Homogeneity.png", graph_2, width = 16, height = 9, units = "cm")
        }
}

####Normality of the distribution####
# Shapiro Wilk test

shapiro <- capture.output(shapiro.test(data[, var]))

cat("\nwrite table with shapiro wilk test. \n--> \"", paste(shapiro, "\"\n", sep = ""), file = "shapiro.txt", sep = "", append = TRUE)

#Histogramm with distribution line
graph_hist <- function(data, var1) {
  graph_hist <- ggplot2::ggplot(data) +
  ggplot2::geom_histogram(ggplot2::aes_string(x = var1), binwidth = 2, color = "black", fill = "white") +
  ggplot2::geom_density(ggplot2::aes_string(var1), alpha = 0.12, fill = "red") +
  ggplot2::ggtitle("Distribution 1")

return(graph_hist)
}

#Add the mean dashed line
add_mean <- function(graph, var1) {
  graph_mean <- graph + ggplot2::geom_vline(xintercept = mean(data[, var1]),
              color = "midnightblue", linetype = "dashed", size = 1)

return(graph_mean)
}

#Adding a QQplot
graph_qqplot <- function(data, var1) {
  graph2 <- ggpubr::ggqqplot(data, var1, color = "midnightblue") + ggplot2::ggtitle("Distribution 2")

return(graph2)
}

#On suppose que les données sont distribuées normalement lorsque les points suivent approximativement la ligne de référence à 45 degrés.

graph_fin <- function(graph1, graph2) {
  graph <- cowplot::plot_grid(graph1, graph2, ncol = 2, nrow = 1)

  ggplot2::ggsave("Normal_distribution.png", graph, width = 10, height = 7, units = "cm")
}

mult <- ifelse(length(unique(data[, colspe])) == 2, FALSE, TRUE)
homog_var(data, col1 = coldate, col2 = colspe, col3 = colvar, mult = mult)

graph_hist1 <- graph_hist(data, var1 = colvar)
graph_mean <- add_mean(graph = graph_hist1, var1 = colvar)
graph_fin(graph1 = graph_mean, graph2 = graph_qqplot(data, var1 = colvar))

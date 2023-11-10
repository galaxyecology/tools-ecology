#Script pour bar plot simple 

#### loading required R libraries
#### chargement des packages R utilisés
library(ggplot2)

###### overall parameters and settings
###### paramètres globaux utilisés

args = commandArgs(trailingOnly=TRUE) 
if (length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    data <- args[1]
    title <- as.character(args[2])
    error_bar <- args[3]
    color <- as.character(args[4])
    ylab <- as.character(args[5])
}

histo_data = read.table(data, header= T)

if (error_bar == "true"){

   ggplot(histo_data, aes(x = variable_name, y = variable, fill = variable_name)) +
     geom_bar(stat = "identity", position = "dodge", fill = color) +
     geom_errorbar(aes(ymin = variable - standard_deviation, ymax = variable + standard_deviation), 
                   position = position_dodge(0.9), width = 0.25) +
     geom_text(aes(label = variable), vjust = -2, color = "black", size = 4) +
     ggtitle(title) +
     ylab(ylab) +
     theme_minimal()+
     theme(legend.position = "none",
           axis.title.x = element_blank())

   ggsave("bar_plot.pdf", device = pdf, width = 20, height = 20, units = "cm")

}else{

   ggplot(histo_data, aes(x = variable_name, y = variable, fill = variable_name)) +
     geom_bar(stat = "identity", position = "dodge", fill = color) +
     geom_text(aes(label = variable), vjust = -1, color = "black", size = 4) +
     ggtitle(title) +
     ylab(ylab) +
     theme_minimal()+
     theme(legend.position = "none",
           axis.title.x = element_blank())

   ggsave("bar_plot.pdf", device = pdf, width = 20, height = 20, units = "cm")}



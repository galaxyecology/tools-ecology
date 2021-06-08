#Rscript

#######################################
##    Homegeneity of the variance    ##
#######################################

#####Packages : car
#		ggplot2

#####Load arguments

args = commandArgs(trailingOnly = TRUE)

if(length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    Table <- args[1]
        HR <- args[2]
	date <- as.numeric(args[3])
	spe <- as.numeric(args[4])
	var <- as.numeric(args[5])
}

if (HR == "false"){HR <- FALSE} else {HR <- TRUE}

#####Import data
Data <- read.table(Table, sep = "\t", dec = ".", header = HR, fill = TRUE, encoding = "UTF-8")
Data <- na.omit(Data)
coldate <- colnames(Data)[date]
colspe <- colnames(Data)[spe]
colvar <- colnames(Data)[var]

#####Your analysis

##Test of Levene##
testlevene <- function(Data, col1, col2){

        Data[,col1] <- as.numeric(Data[,col1])
        Data[,col2] <- as.factor(Data[,col2])
  
	tb_levene <- car::leveneTest(y = Data[,col1], group = Data[,col2])
	return(tb_levene)
	}
levene <-capture.output(testlevene(Data = Data, col1 = colvar, col2 = colspe))

cat("\nwrite table with levene test. \n--> \"", paste(levene,"\"\n", sep=""), file = "levene.txt", sep = "", append = TRUE)


##Two boxplots to visualize it##

homog_var <- function(Data, col1, col2, col3, mult){
	Data[,col1] <- as.factor(Data[,col1])

	if(mult){
            for(spe in unique(Data[, col2])){ 
             Data.cut <- Data[Data[, col2] == spe, ]
             graph_2 <- ggplot2::ggplot(Data.cut, ggplot2::aes_string(x = col1, y = col3, color = col1)) + 
               ggplot2::geom_boxplot() +
               ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1), 
                   panel.background = ggplot2::element_rect(fill = "#d9d4c5", colour = "#d9d4c5", linetype = "solid"),
               panel.grid.major = ggplot2::element_line(linetype = 'solid', colour = "white"), 
               panel.grid.minor = ggplot2::element_line(linetype = 'solid', colour = "white")) 

            ggplot2::ggsave(paste("graph_homog_",spe,".png"), graph_2, width=16, height=9, units="cm")
            }
        }else{
        graph_1 <- ggplot2::ggplot(Data, ggplot2::aes_string(x = col1, y = col3, color = col1)) + 
          ggplot2::geom_boxplot() +
          ggplot2::theme(legend.position = "none", axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

	#Put multiple panels
	graph_2 <- graph_1 + ggplot2::facet_grid(rows = ggplot2::vars(Data[,col2]), scales = "free") + 
	  ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#d9d4c5", colour = "#d9d4c5", linetype = "solid"),
          panel.grid.major = ggplot2::element_line(linetype = 'solid', colour = "white"), 
          panel.grid.minor = ggplot2::element_line(linetype = 'solid', colour = "white")) 
	
	ggplot2::ggsave("graph_homog.png", graph_2, width=16, height=9, units="cm")
        }
}

mult <- ifelse(length(unique(Data[, colspe]))==2, FALSE, TRUE)

homog_var(Data, col1 = coldate, col2 = colspe, col3 = colvar, mult = mult)







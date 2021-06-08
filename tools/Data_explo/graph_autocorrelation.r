#Rscript

############################
##    Auto-correlation    ##
############################

#####Packages : ggplot2

#####Load arguments

args = commandArgs(trailingOnly = TRUE)

if(length(args)==0)
{
    stop("This tool needs at least one argument")
}else{
    Table <- args[1]
        HR <- args[2]
	var <- as.numeric(args[3])
	
}

if (HR == "false"){HR <- FALSE} else {HR <- TRUE}

#####Import data
Data <- read.table(Table, sep = "\t", dec = ".", header = HR, fill = TRUE, encoding = "UTF-8")
colvar <- colnames(Data)[var]


#####Your analysis

####Independence of the observations####

acf_tb <- function(Data, var){
obj <- acf(Data[,var])
  return(obj)
}

acf_df <- function(Data, var){

tb <- data.frame(acf = acf_tb(Data, var)$acf, lag = acf_tb(Data, var)$lag)

  return(tb) # Lag: intervalle temporel entre mesures, fréquence à laquelle on mesure l'auto corrélation.
                                           # ACF: indépendance temporelle 
}

autocorr <- function(var1, var2){

  graph <- ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var2$lag, y = var2$acf), stat = "identity", position = "identity", fill = "midnightblue") +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = qnorm((1 + 0.95)/2)/sqrt(var1$n.used)), 
             linetype = "dashed") + # calcul interval de confiance à 95% sans correction du bruit blanc.
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = -qnorm((1 + 0.95)/2)/sqrt(var1$n.used)), linetype = "dashed") +
  ggplot2::labs(title = "Auto correlation") + ggplot2::xlab("lag") + ggplot2::ylab("acf")
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  
  ggplot2::ggsave("autocorr.png", graph)
}

obj1 <- acf_tb(Data, var = colvar)
obj2 <- acf_df(Data, var = colvar)

autocorr(var1 = obj1, var2 = obj2 )

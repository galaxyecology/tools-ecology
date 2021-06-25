#rscript

################################################
##    Link between variables and themselves   ##
################################################

#####Packages : ggplot2
#               Cowplot
#               Car
#               faraway
#               dplyr
#               GGally
#               FactoMiner
#               factoextra
#               ggcorrplot

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    stop("This tool needs at least one argument")
}else{
    table <- args[1]
    hr <- args[2]
    colli <- as.logical(args[3])
    vif <- as.logical(args[4])
    pca <- as.logical(args[5])
    interr <- as.logical(args[6])
    auto <- as.logical(args[7])
    spe <- as.numeric(args[8])
    col <- as.numeric(strsplit(args[9], ",")[[1]])
    var <- as.numeric(args[10])
    var2 <- as.numeric(args[11])
    var4 <- as.numeric(args[12])
}

if (hr == "false") {
  hr <- FALSE}else {
  hr <- TRUE
}

#####Import data
data <- read.table(table, sep = "\t", dec = ".", header = hr, fill = TRUE, encoding = "UTF-8")
if (vif | pca) {
data_active <- data[col]
#Define the active individuals and the active variables for the PCA
}

if (colli | interr) {
colspe <- colnames(data)[spe]
}

if (colli) {
data_num <- data[col]
data_num$species <- data[, spe]
data_num <- data_num[grep("^$", data_num$spe, invert = TRUE), ]
}

if (interr | auto) {
colvar <- colnames(data)[var]
}

if (interr) {
colvar2 <- colnames(data)[var2]
colvar4 <- colnames(data)[var4]
}

#####Your analysis

####Independence of the observations####

acf_tb <- function(data, var) {
obj <- acf(data[, var])
  return(obj)
}

acf_df <- function(data, var) {

tb <- data.frame(acf = acf_tb(data, var)$acf, lag = acf_tb(data, var)$lag)

  return(tb) # Lag: intervalle temporel entre mesures, fréquence à laquelle on mesure l'auto corrélation.
# ACF: indépendance temporelle
}

autocorr <- function(var1, var2) {
  cat("\nACF\n", var2$acf, file = "acf.txt", fill = 1, append = TrUE)
  graph <- ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var2$lag, y = var2$acf), stat = "identity", position = "identity", fill = "midnightblue") +
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = qnorm((1 + 0.95) / 2) / sqrt(var1$n.used)),
             linetype = "dashed") + # calcul interval de confiance à 95% sans correction du bruit blanc.
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = -qnorm((1 + 0.95) / 2) / sqrt(var1$n.used)), linetype = "dashed") + ggplot2::labs(title = "Autocorrelation") + ggplot2::xlab("lag") + ggplot2::ylab("acf")
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  ggplot2::ggsave("autocorrelation.png", graph)
}

####Interractions####

graph <- function(data, var1, var2, var3) {
  graph <- ggplot2::ggplot(data, ggplot2::aes_string(x = var1, y = var2, group = var3, color = var3)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = lm, se = FALSE) +
  ggplot2::theme(plot.title = ggplot2::element_text(color = "black", size = 12, face = "bold"))

return(graph)
}

# Put multiple panels
interraction <- function(data, var1, var2, var3, var4) {
  cat("\nSpecies\n", spe, file = "Species.txt", fill = 1, append = TrUE)
  if (mult1) {
      for (spe in unique(data[, var3])) {
      data_cut <- data[data[, var3] == spe, ]
      mult_graph <- graph(data_cut, var1, var2, var3) + ggplot2::facet_grid(cols = ggplot2::vars(data_cut[, var4]), scales = "free") +
      cowplot::background_grid(major = "xy", minor = "none") +
      cowplot::panel_border() + ggplot2::ggtitle("Interractions")

      ggplot2::ggsave(paste("interraction_of_", spe, ".png"), mult_graph, width = 10, height = 7)
      }
    }else{
    mult_graph <- graph(data, var1, var2, var3) + ggplot2::facet_grid(rows = ggplot2::vars(data[, var3]), cols = ggplot2::vars(data[, var4]), scales = "free") +
    cowplot::background_grid(major = "xy", minor = "none") +
    cowplot::panel_border() + ggplot2::ggtitle("Interractions")

    ggplot2::ggsave("interraction.png", mult_graph)
  }
}

####Collinearity among covariates####
# Create the plots

coli <- function(data, var) {
  if (mult2) {
    cat("\nThere is not enough data on these species they appear too few times in the tabular-file\n", file = "Data.txt", fill = 1, append = TrUE)
    for (spe in unique(data$species)) {
      nb_spe <- sum(data$species == spe)
      if (nb_spe <= 2) {
      cat(spe, file = "Data.txt", fill = 1, append = TrUE)
      }else{
      data_cut <- data[data$species == spe, ]
      nb <- ncol(data_cut)
      data_num <- data_cut[, -nb]
      graph <- GGally::ggpairs(data_num, ggplot2::aes(color = data_cut$species),
      lower = list(continuous = "points"), axisLabels = "internal")

      ggplot2::ggsave(paste("collinarity_of_", spe, ".png"), graph, width = 20, height = 15)
      }
  }

  }else{
    nb <- ncol(data)
    data_cut <- data[, -nb]
    graph <- GGally::ggpairs(data_cut, ggplot2::aes(color = data[, var]),
    lower = list(continuous = "points"), axisLabels = "internal") +
  ggplot2::scale_colour_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  ggplot2::scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

  ggplot2::ggsave("collinarity.png", graph)
  }
}

####PCA method####

plot_pca <- function(data) {
  #Correlation circle
  graph_corr <- factoextra::fviz_pca_var(active_data(data), col.var = "cos2",
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = TrUE #Avoid text overlap
                           )
  ggplot2::ggsave("Pca_circle.png", graph_corr)
}

plot_qual <- function(data) {
  #PCA results for variables
  var <- factoextra::get_pca_var(active_data(data))

  #representation quality
  graph_quality <- ggcorrplot::ggcorrplot(var$cos2[!apply(var$cos2, 1, anyNA), ], method = "circle",
  ggtheme = ggplot2::theme_gray,
  colors = c("#00AFBB", "#E7B800", "#FC4E07"))

  ggplot2::ggsave("Pca_quality.png", graph_quality)
}

#### Variance inflation factor ####

myvif <- function(mod) {
  v <- vcov(mod)
  assign <- attributes(model.matrix(mod))$assign
  if (names(coefficients(mod)[1]) == "(Intercept)") {
    v <- v[-1, -1]
    assign <- assign[-1]
  } else warning("No intercept: vifs may not be sensible.")
  terms <- labels(terms(mod))
  n_terms <- length(terms)
  if (n_terms < 2) stop("The model contains fewer than 2 terms")
  if (length(assign) > dim(v)[1]) {
     diag(tmp_cor) <- 0
     if (any(tmp_cor == 1.0)) {
        return("Sample size is too small, 100% collinearity is present")
     } else {
        return("Sample size is too small")
     }
  }
  r <- cov2cor(v)
  detr <- det(r)
  result <- matrix(0, n_terms, 3)
  rownames(result) <- terms
  colnames(result) <- c("GVIF", "Df", "GVIF^(1/2Df)")
  for (term in 1:n_terms) {
    subs <- which(assign == term)
    result[term, 1] <- det(as.matrix(r[subs, subs])) * det(as.matrix(r[-subs, -subs])) / detr
    result[term, 2] <- length(subs)
  }
  if (all(result[, 2] == 1)) {
    result <- data.frame(GVIF = result[, 1])
  } else {
    result[, 3] <- result[, 1] ^ (1 / (2 * result[, 2]))
  }
  invisible(result)
}
corvif1 <- function(dataz) {
    dataz <- as.data.frame(dataz)
    #correlation part
    tmp_cor <- cor(dataz, use = "complete.obs")
    return(tmp_cor)
}
corvif2 <- function(dataz) {
    dataz <- as.data.frame(dataz)
    #vif part
    form    <- formula(paste("fooy ~ ", paste(strsplit(names(dataz), " "), collapse = " + ")))
    dataz   <- data.frame(fooy = 1, dataz)
    lm_mod  <- lm(form, dataz)

    return(myvif(lm_mod))
}

#Autocorrelation
if (auto) {
obj1 <- acf_tb(data, var = colvar)
obj2 <- acf_df(data, var = colvar)
autocorr(var1 = obj1, var2 = obj2)
}

if (interr) {
#Interractions
mult1 <- ifelse(length(unique(data[, colspe])) <= 6, FALSE, TRUE)
interraction(data, var1 = colvar, var2 = colvar2, var3 = colspe, var4 = colvar4)
}

#Collinearity
if (colli) {
mult2 <- ifelse(length(unique(data[, spe])) < 3, FALSE, TRUE)
coli(data = data_num, var = spe)
}

#PCA
if (pca) {
active_data <- function(data) {
  #Calcul of PCA for the active data
  res_pca <- FactoMiner::PCA(data, graph = FALSE)

return(res_pca)
}

#eigenvalue
eig_val <- capture.output(factoextra::get_eigenvalue(active_data(data_active)))

cat("\nwrite table with eigenvalue. \n--> \"", paste(eig_val, "\"\n", sep = ""), file = "valeurs.txt", sep = "", append = TrUE)

plot_pca(data_active)
plot_qual(data_active)
}

#VIF
if (vif) {
#Compute 2 tables#
tb_corr <- as.data.frame(corvif1(dataz = data_active))
tb_corr <- cbind(x = rownames(tb_corr), tb_corr)
tb_vif <- corvif2(dataz = data_active)

write.table(tb_corr, "corr.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")

if (all(is.na(tb_vif))) {
  tb_vif <- NULL
  cat("Vif couldn't be calculated, selected data isn't correlated")
}else{
write.table(tb_vif, "vif.tabular", row.names = FALSE, quote = FALSE, sep = "\t", dec = ".", fileEncoding = "UTF-8")
}
}

library(Rdimtools)
library(tools)

### Load the point clouds 
list <- list.files(pattern = "\\.txt$")    

#### Create a loop to process the fractal dimension for each point clouds
output <- NULL
for (i in 1:length(list)) {
  cat(paste0("--- ",Sys.time(),"processing_",list[i]," ---/n")) # to monitor the processing time for each table.
  Points=read.csv(list[i], sep=",")
  
  # computation of Dn
  outputD=est.boxcount(Points[,1:3], nlevel=100, cut = c(0.1,0.9)) # the result of the function est.boxcount
  D <- outputD$estdim  # compile the estimation of the fractal dimension
  
  Dt <- 1-(3-D)
  
  output <- rbind(output,c(Dt))
  
}

#### Save the data

row.names(output) = file_path_sans_ext(list) # rename the row with the names of the 3D CAD models
colnames(output)<-"Dt"
output <- round(output,3)
write.csv(output,"fractal_dimension.csv") 

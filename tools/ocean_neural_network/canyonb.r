#Rscript

#################################
##    Canyon B Neural Network  ##
#################################

#####Packages : 

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

# Input parameters for Canyon B

if (length(args) < 4) {
    stop("This tool needs at least 4 argument : longitude, latitude, species and number of records")
}else {
    date <- as.character(args[1])
    longitude <- as.numeric(args[2])
    latitude <- as.numeric(args[3])
    pressure <- as.numeric(args[4])
    temperature <- as.numeric(args[5])
    salinity <- as.numeric(args[6])
    doxy <- as.numeric(args[7])
    variables <- args[8]
    epressure <- as.numeric(args[9])
    etemperature <- as.numeric(args[10])
    esalinity <- as.numeric(args[11])
    edoxy <- as.numeric(args[12])
}

output <- canyonb::CANYONB(date, longitude, latitude, pressure, temperature, salinity, doxy, variables, epressure, etemperature, esalinity, edoxy)

write.table(output, file = "output.tabular", sep = "\t", dec = ".", na = " ", col.names = TRUE, row.names = FALSE, quote = FALSE)

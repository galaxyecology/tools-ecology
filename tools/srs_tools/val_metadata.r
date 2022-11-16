#Rscript

############################################
##  Validate ISO 19139 metadata documen   ##
############################################

#####Packages : ncdf4,
#               geometa,
#               httr
#               xml
#               xml2
library(geometa)

#####Load arguments

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
    stop("This tool needs at least 1 argument")
}else {
    input_data <- args[1]
}

##------------------------------------------##
##      Read ISO 19139 from a file or url   ##
##------------------------------------------##

# Test depuis catalogue Indores http://indores-tmp.in2p3.fr/geonetwork/srv/fre/catalog.search#/metadata/112ebeea-e79c-422c-8a43-a5a8323b446b
# <!--ISO 19139 XML compliance: NO-->
input_data <- xml2::read_xml(input_data)

dir.create("results")
file.create("results/meta.xml")

xml2::write_xml(input_data, file = "results/meta.xml")

md <- geometa::readISO19139("results/meta.xml")


# validate iso
cat("\nValidation of metadata according to ISO 19139\n", md$validate(), file = "Metadata_validation.txt", fill = 1, append = FALSE)

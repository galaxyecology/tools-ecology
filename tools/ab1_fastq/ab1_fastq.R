#Rscript

############################################
##      Convert ab1 files into fastq      ##
############################################

#####Packages
#library(edgeR)
library(CrispRVariants, quietly = TRUE)
library(sangerseqR, quietly = TRUE)

#####Load arguments

args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) 
{
    stop("This tool needs at least one argument")
}else{
    file <- args[1]
    filename <- args[2]
    tr <- as.logical(args[3])
    co <- as.numeric(args[4])
    min_seq <- as.integer(args[5])
    os <- as.numeric(args[6])
}

##### Conversion

if(grepl("^.+\\.[aA][bB][1i]$", filename)) {
    nfile <- sub("^(.+)\\.[aA][bB][1i]$", "\\1", filename)
} else {
    nfile <- filename
}

CrispRVariants::abifToFastq(nfile, file, "output.fastq", trim = tr, cutoff = co, min_seq_len = min_seq, offset = os)


#Rscript

################################################################################
##      Extract consensus sequence from aligned forward and reverse fasta     ##
################################################################################

#####Packages
library(bioseq, quietly = TRUE)

##Load arguments
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    stop("This tool needs at least one argument")
} else {
    fasta_f <- args[1]
    seq_type <- args[2]
    meth_choice <- args[3]
    gaps_tf <- as.logical(args[4])
    out_og <- as.logical(args[5])
}

## Read input file
seq_l <- bioseq::read_fasta(fasta_f, type = seq_type)

if(bioseq::seq_nseq(seq_l) < 2){
    stop("Only one sequence in the file, at least two aligned sequences are needed to compute a consensus")
}else{
    if(length(unique(bioseq::seq_nchar(seq_l))) > 1) {stop("Sequences have different lengths, please provide aligned sequences")}
}

##Consensus sequence
seq_con <- bioseq::seq_consensus(seq_l, method = meth_choice, gaps = gaps_tf)

if(bioseq::seq_nseq(seq_con) > 1){stop("Consensus hasn't worked for an unknown reason, double-check your input file and the parameters you chose")}

names(seq_con) <- paste0("consensus_", Reduce(PTXQC::LCS, names(seq_l)))
##Create output
if(out_og){
    bioseq::write_fasta(c(seq_con, seq_l), file = "output.fasta", line_length = Inf, block_length = Inf)
}else{
    bioseq::write_fasta(seq_con, file = "output.fasta", line_length = Inf, block_length = Inf)
}

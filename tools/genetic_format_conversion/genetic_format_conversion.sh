#!/bin/bash

# Print an error message to stderr and exit with code 1
die() {
    echo "ERROR: $*" >&2
    exit 1
}

input_file="$1"
input_name="$2"
input_format="$3"
output_format="$4"

if [[ "$input_format" == "vcf" ]] && [[ "$output_format" == "genepop" ]]; then
    ploidy="$5"
    pl_gl="$6"
    exclude_loci="$7"
    non_polymorphic="$8"
    indels="$9"
    regions="${10}"
    PHRED_SCALED="${11}"
    MISSING_GQ="${12}"
    MISSING_DEPTH="${13}"
    individuals="${14}"
    pop_file="${15}"
    indpop="${16}"
    
elif [[ "$input_format" == "tabular" ]] && [[ "$output_format" == "genepop" ]]; then
    sep_char="$5"
    digit_num="$6"
    
elif [[ "$input_format" == "genepop" ]] && [[ "$output_format" == "bayescan" ]]; then
    indpop="$5"
    [[ -f "$indpop" ]] || die "Input indpop was not found: $indpop"
    marker_type="$6"
    alleles_coded="$7"
fi

##### Validate inputs #####
# Check that input files exist on disk
[[ -f "$input_file" ]] || die "Input VCF was not found: $input_file"

##### Check output directory #####
readonly output_dir="convert_directory"
readonly pop_dir="pop_affiliation"

if [[ ! -d "${output_dir}" ]]; then
    die "ERROR: Failed to create output directory: ${output_dir}"
fi

if [[ ! -d "${pop_dir}" ]]; then
    die "ERROR: Failed to create output directory: ${pop_dir}"
fi

##### Build output filename #####
name_without_ext="$(basename -- "$input_name")"
name_without_ext="${name_without_ext%.vcf}"
name_without_ext="${name_without_ext%.txt}"
name_without_ext="${name_without_ext%.tabular}"

# In Galaxy, dataset names may contain a trailing label in parentheses,
# e.g. "Tool name (dataset 42)". Extract the content inside the last
# parentheses if present; otherwise use the full name.
regex='\(([^)]+)\)[[:space:]]*$'
if [[ "$name_without_ext" =~ $regex ]]; then
    base_name="${BASH_REMATCH[1]}"
else
    base_name="$name_without_ext"
fi

[[ -n "$base_name" ]] || die "Could not derive a valid output filename from: $input_name"

###############################################################################
#Function: spid_vcf2genepop
#Description: Creation of the spid file required to convert vcf on genepop
###############################################################################
spid_vcf2genepop(){
    local ploidy="$1"
    local pl_gl="$2"
    local exclude_loci="$3"
    local non_polymorphic="$4"
    local indels="$5"
    local regions="$6"
    local PHRED_SCALED="$7"
    local MISSING_GQ="$8"
    local MISSING_DEPTH="$9"
    local individuals="${10}"
    local indpop="${11}"
    local pop_file="${12}"

        cat > "spid_file.spid" << EOF
# spid-file generated: $(date '+%a %b %d %H:%M:%S %Z %Y')

# VCF Parser questions
PARSER_FORMAT=VCF

# Only output SNPs with a phred-scaled quality of at least:
VCF_PARSER_QUAL_QUESTION=${PHRED_SCALED}
# Include a file with population definitions:
VCF_PARSER_POP_FILE_QUESTION=${indpop}
# What is the ploidy of the data? (DIPLOID/HAPLOID)
VCF_PARSER_PLOIDY_QUESTION=${ploidy}
# Do you want to include a file with population definitions?
VCF_PARSER_POP_QUESTION=${pop_file}
# Output genotypes as missing if the phred-scale genotype quality is below:
VCF_PARSER_GTQUAL_QUESTION=${MISSING_GQ}
# Do you want to include INDELS as STANDARD genetic markers? (TRUE/FALSE)
VCF_PARSER_INDEL_QUESTION=${indels}
# Do you want to include non-polymorphic SNPs? (TRUE/FALSE)
VCF_PARSER_MONOMORPHIC_QUESTION=${non_polymorphic}
# Only output following individuals (ind1, ind2, ind4, ...):
VCF_PARSER_IND_QUESTION=${individuals}
# Only input following regions (refSeqName:start:end, multiple regions: whitespace separated):
VCF_PARSER_REGION_QUESTION=${regions}
# Output genotypes as missing if the read depth of a position for the sample is below:
VCF_PARSER_READ_QUESTION=${MISSING_DEPTH}
# Take most likely genotype if "PL" or "GL" is given in the genotype field? (TRUE/FALSE)
VCF_PARSER_PL_QUESTION=${pl_gl}
# Do you want to exclude loci with only missing data? (TRUE/FALSE)
VCF_PARSER_EXC_MISSING_LOCI_QUESTION=${exclude_loci}

# GENEPOP Writer questions
WRITER_FORMAT=GENEPOP

# Specify which data type should be included in the GENEPOP file  (GENEPOP can only analyze one data type per file): (MICROSAT/STANDARD/SNP/DNA)
GENEPOP_WRITER_DATA_TYPE_QUESTION=SNP
# If SNP data are encoded as nucleotides,  enter the integers that code for nucleotide A, T, C, G (comma separated, e.g 1,2,3,4):
GENEPOP_WRITER_SNP_CODE_QUESTION=
EOF
}

###############################################################################
#Function: spid_snp2bayes
#Description: Creation of the spid file required to convert ssr on genepop
###############################################################################
spid_genepop2bayes(){
    local marker_type="$1"
    local alleles_coded="$2"

        cat > "spid_file.spid" << EOF
# spid-file generated: $(date '+%a %b %d %H:%M:%S %Z %Y')
# GENEPOP Parser questions
PARSER_FORMAT=GENEPOP

# Select the type of the data: (MICROSAT/SNP/STANDARD)
GENEPOP_PARSER_DATA_TYPE_QUESTION=$marker_type
# How are Microsat alleles coded? (REPEATS/LENGTH)
GENEPOP_PARSER_MICROSAT_CODING_QUESTION=$alleles_coded

# GESTE / BayeScan Writer questions
WRITER_FORMAT=GESTE_BAYE_SCAN

# Specify which data type should be included in the GESTE / BayeScan file  (GESTE / BayeScan can only analyze one data type per file): (MICROSAT/SNP/STANDARD/DNA)
GESTE_BAYE_SCAN_WRITER_DATA_TYPE_QUESTION=$marker_type

EOF
}

##########################################################
#Fonction : ssr2genepop
# Description : 
##########################################################
ssr2genepop() {
    local input_file="$1"
    local input_name="$2"
    local output_file="$3"
    local sep_char="$4"
    local digit_num="$5"
    local width=$((digit_num*2))     

    case "$sep_char" in
    TAB)        sep_char=$'\t' ;;
    COMMA)      sep_char=',' ;;
    SEMICOLON)  sep_char=';' ;;
    WHITESPACE) sep_char=' ' ;;
    *)
        echo "ERROR: Unknown separator: '$sep_char'" >&2
        exit 1
        ;;
esac
    
    echo "=== SEP_CHAR ===" >&2
echo "sep_char: '${sep_char}'" >&2
printf "sep_char hex: " >&2
echo -n "${sep_char}" | xxd | head -1 >&2

echo "=== PREMIÈRE LIGNE DU FICHIER ===" >&2
head -1 "$input" | cat -A >&2
printf "hex: " >&2
head -1 "$input" | xxd | head -3 >&2        
    
    # Fonction pour parser un génotype
    parse_genotype() {
        local genotype="$1"
        
        # Management of missing data
        if [[ -z "$genotype" ]] || [[ "$genotype" =~ ^(0|NA|N/A|-|\.)$ ]]; then
            printf "%0*d" "$width" 0
            return
        fi
        
        # Allele separation
        local allele1 allele2
        #rajouter vérification de la présence de / sinon break code
        IFS="/" read -r allele1 allele2 <<< "$genotype" #allele are separated by '/'
        
        # Cleaning
        allele1=$(echo "$allele1" | tr -d '[:space:]')
        allele2=$(echo "$allele2" | tr -d '[:space:]')
        
        # Check that they are coded with numbers 
        if ! [[ "$allele1" =~ ^[0-9]+$ ]] || ! [[ "$allele2" =~ ^[0-9]+$ ]]; then
            printf "%0*d" "$width" 0 #Convert to missing data if they aren't numbers
            return
        fi

        printf "%0${digit_num}d%0${digit_num}d" "$allele1" "$allele2"
    }
    
    local line_num=0
    local -a loci_names
    declare -A populations
    declare -A pop_individuals
    declare -A pop_genotypes
    
    while IFS= read -r line; do
        line_num=$((line_num + 1))
        
        # Ignore empty lines
        [[ -z "$line" ]] && continue
        
        # First line = header
        if [[ $line_num -eq 1 ]]; then
            IFS="$sep_char" read -ra fields <<< "$line"
            
            # Check header (ind_col=0 pop_col=1)
            if [[ "${fields[0]}" != "Ind" ]] || [[ "${fields[1]}" != "Pop" ]]; then
                echo "Ind and Pop column not found"
                return 1
            fi
            
            # Loci after pop_col (2)
            for ((i=2; i<${#fields[@]}; i++)); do
                loci_names+=("${fields[$i]}")
            done
            
            continue
        fi
        
        # Data rows
        IFS="$sep_char" read -ra fields <<< "$line"
        
        #Check presence of almost 3 columns (Ind Pop and 1 genotype)
        if [[ ${#fields[@]} -lt 3 ]]; then
            continue
        fi
        
        # [0]=Ind, [1]=Pop, [2+]=Genotypes
        local ind_name="${fields[0]}"
        local pop_name="${fields[1]}"
        
        # Keep population
        populations[$pop_name]=1
        pop_individuals[$pop_name]+="$ind_name "
        
        # Convert genotype (col 2+)
        local genepop_line=""
        for ((i=2; i<${#fields[@]}; i++)); do
            local genotype="${fields[$i]}"
            local converted=$(parse_genotype "$genotype")
            genepop_line+="$converted "
        done
        
        pop_genotypes["${pop_name}__${ind_name}"]="$genepop_line"
        
    done < "$input_file"
    
    # Write genepop file
    {     
        echo "SSR data: ${input_name}"

        for locus in "${loci_names[@]}"; do
            echo "$locus"
        done
        
        for pop_name in $(printf '%s\n' "${!populations[@]}" | sort); do
            echo "Pop"
            
            for ind_name in ${pop_individuals[$pop_name]}; do
                local genotypes="${pop_genotypes[${pop_name}__${ind_name}]}"
                echo "$ind_name ,  $genotypes"
            done
        done
    } > "$output_file"
    
    return 0
}

#########################################
# Main execution
#########################################

    if [ "$input_format" = "vcf" ] && [ "$output_format" = "genepop" ]; then
        output_genfile="${output_dir}/${base_name}_genepop.txt"
        spid_vcf2genepop "$ploidy" "$pl_gl" "$exclude_loci" "$non_polymorphic" "$indels" "$regions" "$PHRED_SCALED" "$MISSING_GQ" "$MISSING_DEPTH" "$individuals" "$indpop" "$pop_file"
        PGDSpider2-cli -inputfile "$input_file" -inputformat VCF -outputfile "$output_genfile" -outputformat GENEPOP -spid spid_file.spid

    elif [ "$input_format" = "genepop" ] && [ "$output_format" = "bayescan" ]; then
        ##### Retrieve pop order #####
        pop_order_file="${pop_dir}/pop_affiliation.txt"
        in_pop=false
        first_ind=false

        while IFS= read -r line; do
            line="${line%$'\r'}"   # remove windows end lines

                if [[ "${line,,}" == "pop" ]]; then
                in_pop=true
                first_ind=true
                continue
            fi

            if [[ "$in_pop" == true && "$first_ind" == true && -n "$line" ]]; then
                pop_count=$((pop_count + 1))
                ind_id=$(echo "$line" | cut -d',' -f1 | xargs)
                actual_pop_name=$(awk -v id="$ind_id" '{ gsub(/\r/, ""); if ($1 == id) print $2 }' "$indpop")
                if [[ -z "$actual_pop_name" ]]; then
                    actual_pop_name="Unknown_Pop_${pop_count}"
                fi
                echo -e "${actual_pop_name}\t${pop_count}" >> "$pop_order_file"
                first_ind=false
            fi
        done < "$input_file"

        ##### Bayes conversion #####
        output_genfile="${output_dir}/${base_name}_bayes.txt"
        spid_genepop2bayes "$marker_type" "$alleles_coded"
        PGDSpider2-cli -inputfile "$input_file" -inputformat GENEPOP -outputfile "$output_genfile" -outputformat GESTE_BAYE_SCAN -spid spid_file.spid

    elif [ "$input_format" = "tabular" ] && [ "$output_format" = "genepop" ]; then
        output_genfile="${output_dir}/${base_name}_genepop.txt" 
        ssr2genepop "$input_file" "$base_name" "$output_genfile" "$sep_char" "$digit_num"

    else
        echo "Unrecognised combination of parameters"
        exit 1
    fi





#!/bin/bash

#Exit on error
set -e

##### Load arguments #####
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --input)            vcf_input="$2";           shift ;;
        --name)             vcf_name="$2";            shift ;;
        --order)            ORDER="$2";               shift ;;
        --max-missing-ind)  MAX_MISSING_IND="$2";     shift ;;
        --max-missing-loci) MAX_MISSING_LOCI="$2";    shift ;;
        --min-gq)           MIN_GQ="$2";              shift ;;
        --min-dp)           MIN_DP="$2";              shift ;;
        --min-mac)          MAC="$2";                 shift ;;
        --max-ho)           MAX_Ho="$2";              shift ;;
        *) echo "ERROR: Unknown argument: $1"; exit 1 ;;
    esac
    shift
done

##### Verify that the output directories exist #####
tmp_dir="vcf_filtered_tmp"
vcf_dir="vcf_filtered_directory"
summ_dir="summary"

if [[ ! -d "${vcf_dir}" ]]; then
    echo "ERROR: Failed to create output VCF directory" >&2
    exit 1
fi

if [[ ! -d "${summ_dir}" ]]; then
    echo "ERROR: Failed to create summary output directory" >&2
    exit 1
fi

if [[ ! -d "${tmp_dir}" ]]; then
    echo "ERROR: Failed to create temporary directory" >&2
    exit 1
fi

##### Check input file #####
if [[ -z "$vcf_input" ]]; then
    echo "ERROR: VCF file is not provided." >&2
    exit 1
fi

# Verify that input VCF contains at least one variant
if ! bcftools view -H "$vcf_input" | head -n 1 | grep -q .; then
    echo "ERROR: Input VCF contains no variant records."
    exit 1
fi

# Extract base name from vcf_name
regex='\(([^)]+)\)[[:space:]]*$'
if [[ "$vcf_name" =~ $regex ]]; then
    base_name="${BASH_REMATCH[1]}"
else
    base_name=$(basename "$vcf_name")
fi
base_name=${base_name%.*}

# Initialize summary
SUMMARY="${summ_dir}/summary.tabular"
echo -e "File\tStep\tFilter\tParameters\tN_individuals\tN_SNPs" > "$SUMMARY"

log_stats() {
    local step="$1"
    local filter="$2"
    local params="$3"
    local vcf="$4"
    local n_ind n_snps
    n_ind=$(bcftools query -l "$vcf" | wc -l)
    n_snps=$(bcftools view -H "$vcf" | wc -l)
    echo -e "${base_name}\t${step}\t${filter}\t${params}\t${n_ind}\t${n_snps}" >> "$SUMMARY"
}

# Temporary vcf to make connexion between the diferent filter
CURRENT_VCF="$vcf_input"
STEP=0
SUFFIX=""

# Log initial
log_stats 0 "input" "raw" "$CURRENT_VCF"

###################################################
#Function: vcf_filtering_IND_missing_data
#Description:Remove individuals with missing data
###################################################

vcf_filtering_IND_missing_data(){
    ##### Parameters #####
    local vcf="$1"
    local MAX_MISSING_IND="$2"
    local tag="_mdIND"

        echo ">>> [Step $STEP] Filter A: Individuals missing data (threshold: $MAX_MISSING_IND)"

        ##### Filtering on individuals with a high amount of missing data #####
        local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"
        local intermed_files="${tmp_dir}/${base_name}_IND_MISSING_DATA"
        local ind_miss="${tmp_dir}/${base_name}_ind_missing_SNPs.txt"  #list of individuals to be retained
        local imiss="${intermed_files}.imiss"

        vcftools --vcf  "$vcf" --missing-indv --out "$intermed_files"
        
        awk -v threshold="$MAX_MISSING_IND" 'NR > 1 && $5 < threshold { print $1 }' "$imiss" > "$ind_miss"

        bcftools view -S "$ind_miss" -O v -o "$output_file" "$vcf"

        ##### Verify that filtered VCF is not empty ######
        if [[ ! -f "$output_file" ]]; then
            echo "ERROR: Output VCF not created: $output_file" >&2
            exit 1
        fi

        if ! bcftools view -H "$output_file" | head -n 1 | grep -q .; then
            echo "ERROR: Filtered VCF contains no variants."
            exit 1
        fi  

        CURRENT_VCF="$output_file"
        SUFFIX="${SUFFIX}${tag}"

        #Complete summary file
        log_stats "$STEP" "A_ind_missing" "MAX_MISSING_IND=${MAX_MISSING_IND}" "$CURRENT_VCF" 
    
}

######################################################
#Function: vcf_filtering_SNP_missingdata_MAC
#Description: Remove SNPs with missing data
#######################################################

vcf_filtering_SNP_missingdata(){
    ##### Parameters #####
    local vcf="$1"
    local MAX_MISSING_LOCI="$2"
    local tag="_SNPmd"

        echo ">>> [Step $STEP] Filter B: Loci missing data (threshold: $MAX_MISSING_LOCI)"

        ##### Remove SNPs with a high amount of missing data and singletons #####
        local output_file_gz="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf.gz" #Final output file - gzip
        local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf" #Final output file

        bcftools filter -e "F_MISSING > ${MAX_MISSING_LOCI}" -O z -o "$output_file_gz" "$vcf"

        if [[ ! -f "$output_file_gz" ]]; then
            echo "ERROR: Output VCF not created: ${output_file_gz}" >&2
            exit 1
        fi

        gunzip "${output_file_gz}"

        ##### Verify that filtered VCF is not empty ######
        if [[ ! -f "$output_file" ]]; then
            echo "ERROR: Output VCF not created: $output_file" >&2
            exit 1
        fi

        if ! bcftools view -H "$output_file" | head -n 1 | grep -q .; then
            echo "ERROR: Filtered VCF contains no variants."
            exit 1
        fi  

        CURRENT_VCF="$output_file"
        SUFFIX="${SUFFIX}${tag}"
        log_stats "$STEP" "B_loci_missing" "MAX_MISSING_LOCI=${MAX_MISSING_LOCI}" "$CURRENT_VCF"

}


##############################################################
#Function: vcf_filtering_gen_qual
#Description: Filters variants based on genotype quality (GQ)
# and replaces low-quality genotypes with missing values.
##############################################################

vcf_filtering_gen_qual(){
    ##### Parameters #####
    local vcf="$1"
    local MIN_GQ="$2"
    local tag="_GQ"

        echo ">>> [Step $STEP] Filter C: Genotype quality (threshold: $MIN_GQ)"

        ###### Filtering variants based on genotype quality (GQ))######
        local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf.gz"
        local output_file_n="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"
        local final_vcf

        local gq_params="MIN_GQ=${MIN_GQ}"

        if grep -q "##INFO=<ID=GQ" "${vcf}"; then
            bcftools filter -S . -e "INFO/GQ<${MIN_GQ}" -O z -o "$output_file" "$vcf"
            gunzip "${output_file}"
            final_vcf="${output_file%.gz}"
  
        elif grep -q "##FORMAT=<ID=GQ" "${vcf}"; then
            bcftools filter -S . -e "FMT/GQ<${MIN_GQ}" -O z -o "$output_file" "$vcf"
            gunzip "${output_file}"
            final_vcf="${output_file%.gz}"
            
        else
            echo "No genotype quality (GQ) field found in $base_name"
            cp "$vcf" "$output_file_n" #Allows the pipeline to continue without filtering
            final_vcf="$output_file_n"
            gq_params="No GQ field found"
        fi

        ##### Verify that filtered VCF is not empty ######
        if [[ ! -f "$final_vcf" ]]; then
            echo "ERROR: Output VCF not created: $final_vcf" >&2
            exit 1
        fi

        if ! bcftools view -H "$final_vcf" | head -n 1 | grep -q .; then
            echo "ERROR: Filtered VCF contains no variants."
            exit 1
        fi

        CURRENT_VCF="$final_vcf"
        SUFFIX="${SUFFIX}${tag}"
        log_stats "$STEP" "C_GQ" "${gq_params}" "$CURRENT_VCF"
}

#######################################################################
#Function: vcf_filtering_depth
#Description: Applies a filter for reading depth (minimum and maximum)
#######################################################################

vcf_filtering_depth(){
    ##### Parameters #####
    local vcf="$1"
    local MIN_DP="$2"
    local tag="_DP"

    echo ">>> [Step $STEP] Filter D: Depth coverage (threshold: $MIN_DP)"

    ###### Filtering min and maximum read depth ######
    local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf.gz"
    local output_file_n="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"

    #Estimate maximum reading depth as twice the average reading depth

    local dp_params="MIN_DP=${MIN_DP}"
    local MAX_RD=""

    if grep -q "##FORMAT=<ID=DP" "$vcf"; then
        MAX_RD=$(bcftools query -f '[%DP\n]' "$vcf" | \
            awk '$1!="." {sum+=$1; n++} END {printf "%.2f", 2*sum/n}')
            echo "  Maximum read depth: $MAX_RD"
        bcftools filter -S . -e "FMT/DP<=${MIN_DP} | FMT/DP>=${MAX_RD}" -O z -o "$output_file" "$vcf"
        gunzip "$output_file"
        final_vcf="${output_file%.gz}"
        dp_params="MIN_DP=${MIN_DP};MAX_DP=${MAX_RD}"

    elif grep -q -e "##INFO=<ID=DP" "$vcf"; then
        MAX_RD=$(bcftools query -f '[%DP\n]' "$vcf" | \
            awk '$1!="." {sum+=$1; n++} END {printf "%.2f", 2*sum/n}')
            echo "  Maximum read depth: $MAX_RD"
        bcftools filter -S . -e "INFO/DP<=${MIN_DP} | INFO/DP>=${MAX_RD}" -O z -o "$output_file" "$vcf"
        gunzip "$output_file"
        final_vcf="${output_file%.gz}"
        dp_params="MIN_DP=${MIN_DP};MAX_DP=${MAX_RD}"
            
    else
        echo "No read depth data for $vcf file"
        cp "$vcf" "$output_file_n"
        final_vcf="${output_file_n}"
        dp_params="No DP field found"
    fi
    
    ##### Verify that filtered VCF is not empty ######
        if [[ ! -f "$final_vcf" ]]; then
            echo "ERROR: Output VCF not created: $final_vcf" >&2
            exit 1
        fi

        if ! bcftools view -H "$final_vcf" | head -n 1 | grep -q .; then
            echo "ERROR: Filtered VCF contains no variants."
            exit 1
        fi

        CURRENT_VCF="$final_vcf"
        SUFFIX="${SUFFIX}${tag}"
        log_stats "$STEP" "D_DP" "${dp_params}" "$CURRENT_VCF"
}

#######################################################################
#Function: vcf_filtering_biallelic
#Description: Filters the VCF to retain only biallelic SNPs
#######################################################################

vcf_filtering_biallelic(){
    ##### Parameters #####
    local vcf="$1"
    local tag="_bialSNP"

    echo ">>> [Step $STEP] Filter E: Biallelic SNPs"

    ##### Keep biallelic SNPs only #####
    local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"
        
    # Apply filter
    bcftools view -v snps -m2 -M2 "$vcf" -o "$output_file"

    ##### Verify that filtered VCF is not empty ######
    if [[ ! -s "$output_file" ]]; then
        echo "Output VCF not created: $output_file" >&2
        return
    fi

    if ! bcftools view -H "$output_file" | head -n 1 | grep -q .; then
        echo "ERROR: Filtered VCF contains no variants."
        exit 1
    fi   

    CURRENT_VCF="$output_file"
    SUFFIX="${SUFFIX}${tag}"
    log_stats "$STEP" "E_biallelic" "biallelic_SNPs_only" "$CURRENT_VCF"
    
}

######################################################
#Function: vcf_filtering_MAC
#Description: Applied minor allele count filter
######################################################

vcf_filtering_MAC(){
    ##### Parameters #####
    local vcf="$1"
    local MAC="$2"
    local tag="_MAC"
        
        echo ">>> [Step $STEP] Filter F: Minor allele count (threshold: $MAC)"

        #Final output file
        local output_file_gz="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf.gz"
        local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"

        bcftools filter -e "MAC < ${MAC}" -O z -o "$output_file_gz" "$vcf"
        gunzip "${output_file_gz}"

        ##### Verify that filtered VCF is not empty ######
        if [[ ! -f "$output_file" ]]; then
            echo "ERROR: Output VCF not created: $output_file" >&2
            exit 1
        fi

        if ! bcftools view -H "$output_file" | head -n 1 | grep -q .; then
            echo "ERROR: Filtered VCF contains no variants."
            exit 1
        fi  

        CURRENT_VCF="$output_file"
        SUFFIX="${SUFFIX}${tag}"
        log_stats "$STEP" "F_MAC" "MIN_MAC=${MAC}" "$CURRENT_VCF"

}

#######################################################################
#Function: vcf_filtering_biallelic
#Description: Applies a filter to conserve only biallelic SNP
#######################################################################

vcf_filtering_heterozygosity(){
    ##### Parameters #####
    local vcf="$1"
    local MAX_Ho="$2"
    local tag="_Ho"

        echo ">>> [Step $STEP] Filter G: Heterozygosity (threshold: $MAX_Ho)"

        ##### Heterozygosity filter #####
        local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"
        local positions_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.txt"
        
        # Calculating observed heterozygosity for each variant and conserve position
        #Het : number of heterozygotes
        #total : number of inidividuals with valid genotype
        echo "Calculating observed heterozygosity for each variant..." >&2
        bcftools query -f '%CHROM\t%POS[\t%GT]\n' "$vcf" | \
        awk -v prop="${MAX_Ho}" 'BEGIN {OFS="\t"} {
            ho=0; total=0;
            for(i=3; i<=NF; i++) {
                if($i != "./." && $i != ".|.") {
                    total++;
                    if($i ~ /0\/1/ || $i ~ /1\/0/ || $i ~ /0\|1/ || $i ~ /1\|0/) ho++;
                }
            }
            if(total > 0 && (ho/total) < prop) print $1, $2;
        }' > "$positions_file"
        
        # Check whether variants have passed the filter
        if [[ -s "$positions_file" ]]; then
            echo "Extracting filtered variants..." >&2
            bcftools view -T "$positions_file" -Ov -o "$output_file" "$vcf"
            
            n_variants=$(wc -l < "$positions_file")
            echo "Successfully created: $output_file (${n_variants} variants kept)" >&2
        else
            echo "WARNING: No variants passed the filter (Ho < ${MAX_Ho}) for $vcf" >&2
            bcftools view -h "$vcf" > "$output_file"
            echo "Created empty VCF with header only: $output_file" >&2
        fi
        
        ##### Verify that filtered VCF is not empty ######
        if [[ ! -f "$output_file" ]]; then
            echo "ERROR: Output VCF not created: $final_vcf" >&2
            exit 1
        fi

        if ! bcftools view -H "$output_file" | head -n 1 | grep -q .; then
            echo "ERROR: Filtered VCF contains no variants."
            exit 1
        fi  

        # Cleanup
        rm -f "$positions_file"

        CURRENT_VCF="$output_file"
        SUFFIX="${SUFFIX}${tag}"
        log_stats "$STEP" "G_heterozygosity" "MAX_Ho=${MAX_Ho}" "$CURRENT_VCF"

}

########################################################
# Main execution
#######################################################
IFS=',' read -ra FILTERS <<< "$ORDER"

for FILTER in "${FILTERS[@]}"; do
    STEP=$((STEP + 1))

    case $FILTER in
        A) vcf_filtering_IND_missing_data "$CURRENT_VCF" "$MAX_MISSING_IND"   ;;
        B) vcf_filtering_SNP_missingdata "$CURRENT_VCF" "$MAX_MISSING_LOCI" ;;
        C) vcf_filtering_gen_qual "$CURRENT_VCF" "$MIN_GQ" ;;
        D) vcf_filtering_depth "$CURRENT_VCF" "$MIN_DP" ;;
        E) vcf_filtering_biallelic "$CURRENT_VCF" ;;
        F) vcf_filtering_MAC "$CURRENT_VCF" "$MAC" ;;
        G) vcf_filtering_heterozygosity "$CURRENT_VCF" "$MAX_Ho" ;;
        *) echo "ERROR: Unknown filter '$FILTER' in order string." >&2; exit 1 ;;
    esac
done

FINAL_OUTPUT="${vcf_dir}/${base_name}_filtered.vcf"
cp "$CURRENT_VCF" "$FINAL_OUTPUT"


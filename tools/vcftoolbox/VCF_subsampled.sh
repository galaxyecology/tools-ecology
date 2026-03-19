#!/bin/bash

# Print an error message to stderr and exit with code 1
die() {
    echo "ERROR: $*" >&2
    exit 1
}

##### Load arguments #####
vcf_input=""
vcf_name=""
SUBSET_SNPS_NB=""
NB_REPLICATE_VCF=""
MIN_SNP_NB=""
POLY=""

# Parse named flags; each flag consumes its value with a first shift,
# then the outer shift moves to the next flag.
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --input)            vcf_input="$2";           shift ;;
        --name)             vcf_name="$2";            shift ;;
        --subset_snps_nb)   SUBSET_SNPS_NB="$2";               shift ;;
        --nb_replicate_vcf) NB_REPLICATE_VCF="$2";     shift ;;
        --min_snp_nb)       MIN_SNP_NB="$2";    shift ;;
        --poly)             POLY="$2";              shift ;;
        *) echo "Unknown argument: $1"; exit 1 ;;
    esac
    shift
done

##### Validate inputs #####
# Ensure bcftools and vcftools are available in PATH
command -v bcftools >/dev/null 2>&1 || die "bcftools is not installed or not in PATH."
command -v vcftools >/dev/null 2>&1 || die "vcftools is not installed or not in PATH."

# Check that input files exist on disk
[[ -f "$vcf_input" ]] || die "Input VCF was not found: $vcf_input"

# Check taht input VCF is not empty
if ! bcftools view -H "$vcf_input" | head -n 1 | grep -q .; then
    die "Input VCF contains no variant records"
fi

##### Output directory #####
readonly vcf_dir_sub="vcf_subsampled"

##### Build output filename #####
name_without_ext="$(basename -- "$vcf_name")"
name_without_ext="${name_without_ext%.vcf.gz}"
name_without_ext="${name_without_ext%.vcf}"

# In Galaxy, dataset names may contain a trailing label in parentheses,
# e.g. "Tool name (dataset 42)". Extract the content inside the last
# parentheses if present; otherwise use the full name.
regex='\(([^)]+)\)[[:space:]]*$'
if [[ "$name_without_ext" =~ $regex ]]; then
    base_name="${BASH_REMATCH[1]}"
else
    base_name="$name_without_ext"
fi

[[ -n "$base_name" ]] || die "Could not derive a valid output filename from: $vcf_name"

##############################################################
# Function: detect_filter_mode
# Description: Detects whether to filter by ID or CHROM/POS
#   - Returns "id"  if CHROM=0 or CHROM missing
#   - Returns "pos" if IDs are all '.' (not set)
#   - Returns "id"  otherwise (more robust default)
##############################################################
 
detect_filter_mode(){
    local vcf="$1"

    local chrom_check
    chrom_check=$(bcftools query -f '%CHROM\n' "$vcf" | head -n 5 | sort -u)

    # Invalid CHROM → fallback sur ID
    if [[ "$chrom_check" == "0" ]] || [[ -z "$chrom_check" ]]; then
        echo "id"
        return
    fi

    # IDs not filled in → CHROM/POS mandatory
    local id_missing
    id_missing=$(bcftools query -f '%ID\n' "$vcf" | head -n 5 | grep -c '^\.$' || true)
    if (( id_missing == 5 )); then
        echo "pos"
        return
    fi

    # Valid CHROM/POS and IDs present → CHROM/POS (original behaviour)
    echo "pos"
}


##############################################################
#Function: vcf_subsampled
#Description : VCF subsampling with a given number of SNPs
##############################################################

vcf_subsampled(){
    ##### Parameters #####
    local vcf="$1"
    local base_name="$2"
    local SUBSET_SNPS_NB="$3"
    local NB_REPLICATE_VCF="$4"
    local MIN_SNP_NB="$5"

        # Count SNPs and individuals
        local total_snps
        total_snps=$(bcftools view -H "$vcf" | wc -l)

        # Check thresholds
        if (( total_snps <= MIN_SNP_NB )); then
            if [[ "$POLY" == "true" ]]; then
                echo "$base_name ignored: polymocphic SNPs=$total_snps < thresholds ($MIN_SNP_NB)"
                local output_vcf="$vcf_dir_sub/${base_name}_full.vcf"
                cp "$vcf" "$output_vcf"
            else
                echo "$base_name ignored: SNPs=$total_snps < thresholds ($MIN_SNP_NB)"
                local output_vcf="$vcf_dir_sub/${base_name}_full.vcf"
                cp "$vcf" "$output_vcf"
            fi
            return
        fi

        # Detect filter mode: by ID or by CHROM/POS
        local filter_mode
        filter_mode=$(detect_filter_mode "$vcf")
        echo "Filter mode for ${base_name}: ${filter_mode}"
 
        # Extract SNP list according to filter mode
        local full_snps_list="${base_name}_full.txt"
 
        if [[ "$filter_mode" == "id" ]]; then
            bcftools query -f '%ID\n' "$vcf" > "$full_snps_list"
        else
            bcftools query -f '%CHROM\t%POS\n' "$vcf" > "$full_snps_list"
        fi
        #Perform the requested number of subsamples
    for j in $(seq 1 "$NB_REPLICATE_VCF"); do
 
        local subsample_snps="${base_name}_subsample_${j}.txt"
        local output_vcf="${vcf_dir_sub}/${base_name}_${SUBSET_SNPS_NB}snps_subsample_${j}.vcf"
 
        if [[ "$filter_mode" == "id" ]]; then
            # Random selection by ID
            shuf -n "$SUBSET_SNPS_NB" "$full_snps_list" > "$subsample_snps"
            bcftools view --include "ID=@${subsample_snps}" "$vcf" -o "$output_vcf"
        else
            # Random selection by CHROM/POS
            shuf -n "$SUBSET_SNPS_NB" "$full_snps_list" | sort -k1,1 -k2,2n > "$subsample_snps"
            vcftools --vcf "$vcf" --positions "$subsample_snps" --recode --stdout > "$output_vcf" 2>/dev/null
        fi
                ##### Verify that filtered VCF is not empty ######
                if [[ ! -f "$output_vcf" ]]; then
                    die "ERROR: Output VCF not created: $output_vcf" >&2
                fi

                if ! bcftools view -H "$output_vcf" | head -n 1 | grep -q .; then
                    die "ERROR: Filtered VCF contains no variants."
                fi  
        done
}

######################
# Main execution
######################

if [[ "$POLY" == "true" ]]; then
    #MAC output file
        MAC_file_gz="${base_name}_MAC.vcf.gz"
        MAC_file="${base_name}_MAC.vcf"

        MAC=2
        bcftools filter -e "MAC < ${MAC}" -O z -o "$MAC_file_gz" "$vcf_input"
        gunzip "${MAC_file_gz}"

        vcf_subsampled "$MAC_file" "$base_name" "$SUBSET_SNPS_NB" "$NB_REPLICATE_VCF" "$MIN_SNP_NB"

else
    vcf_subsampled "$vcf_input" "$base_name" "$SUBSET_SNPS_NB" "$NB_REPLICATE_VCF" "$MIN_SNP_NB"
fi
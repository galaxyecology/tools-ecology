#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_names="$2"

vcf_dir="vcf_filtered_directory"
summ_dir="summary"


##### Verify that the output directories exist #####
if [[ ! -d "${vcf_dir}" ]]; then
    echo "ERROR: Failed to create output VCF directory" >&2
    exit 1
fi

if [[ ! -d "${summ_dir}" ]]; then
    echo "ERROR: Failed to create summary output directory" >&2
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

summary_file="summary/n_snps.tabular"
echo -e "Dataset\tN_SNPs_before\tN_SNPs_after_biallelic" >"${summary_file}"

#######################################################################
#Function: vcf_filtering_biallelic
#Description: Filters the VCF to retain only biallelic SNPs
#######################################################################

vcf_filtering_biallelic(){
    ##### Parameters #####
    local vcf="$1"
    local original_name="$2"

    ##### Check if file exists #####
    if [[ ! -f "$vcf" ]]; then
        echo "File not found, ignored: $vcf"
        return
    fi

    # Extract base name
    local base_name
    local regex='\(([^)]+)\)[[:space:]]*$'
    if [[ "$original_name" =~ $regex ]]; then
        #Extract content inside thhe last parentheses
        base_name="${BASH_REMATCH[1]}"
    else
        # No parentheses, use original name
        base_name=$(basename "$original_name")
    fi
    #Remove extension
    base_name=${base_name%.*}

    ##### Keep biallelic SNPs only #####
    output_file="${vcf_dir}/${base_name}_biallelicSNPs.vcf"
        
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

    ##### Count SNPs before and after filtering #####
    n_snp_before=$(bcftools view -H "$vcf" | wc -l)
    n_snp_after=$(bcftools view -H "$output_file" | wc -l)

    ##### Append results to output file #####
    echo -e "${base_name}\t${n_snp_before}\t${n_snp_after}" >> "${summary_file}"
    
}

##################
# Main execution
##################

vcf_filtering_biallelic "$vcf_input" "$vcf_names"

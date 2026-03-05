#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_names="$2"
MIN_GQ="$3"

vcf_dir="vcf_filtered_directory"

##### Verify that the output directory exists #####
if [[ ! -d "${vcf_dir}" ]]; then
    echo "ERROR: Failed to create output VCF directory" >&2
    exit 1
fi

##### Check input files #####
if [[ -z "$vcf_input" ]]; then
    echo "ERROR: VCF file is not provided." >&2
    exit 1
fi

# Verify that input VCF contains at least one variant
if ! bcftools view -H "$vcf_input" | head -n 1 | grep -q .; then
    echo "ERROR: Input VCF contains no variant records."
    exit 1
fi

##############################################################
#Function: vcf_filtering_gen_qual
#Description: Filters variants based on genotype quality (GQ)
# and replaces low-quality genotypes with missing values.
##############################################################

vcf_filtering_gen_qual(){
    ##### Parameters #####
    local vcf="$1"
    local original_name="$2"
    local MIN_GQ="$3"

        ##### Check if file exists #####
        if [[ ! -f "$vcf" ]]; then
            echo "File not found, ignored: $vcf"
            return
        fi

        # Extract base name
        local base_name
        local regex='\(([^)]+)\)[[:space:]]*$'
        if [[ "$original_name" =~ $regex ]]; then
            #Extract content inside the last parentheses
            base_name="${BASH_REMATCH[1]}"
        else
            # No parentheses, use original name
            base_name=$(basename "$original_name")
        fi
        base_name=${base_name%.*} #remove entension

        ###### Filtering variants based on genotype quality (GQ))######
        output_file="${vcf_dir}/${base_name}_GQ.vcf.gz"
        output_file_n="${vcf_dir}/${base_name}_nGQ.vcf"

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
}

##################
# Main execution
##################

vcf_filtering_gen_qual "$vcf_input" "$vcf_names" "$MIN_GQ"

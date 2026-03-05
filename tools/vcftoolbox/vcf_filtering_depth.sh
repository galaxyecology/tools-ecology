#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_names="$2"
MIN_DP="$3"

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

#######################################################################
#Function: vcf_filtering_depth
#Description: Applies a filter for reading depth (minimum and maximum)
#######################################################################

vcf_filtering_depth(){
    ##### Parameters #####
    local vcf="$1"
    local original_name="$2"
    local MIN_DP="$3"

    ##### Check if file exists #####
    if [[ ! -f "$vcf" ]]; then
        echo "File not found, ignored: $vcf"
        return
    fi

    # Extract base name
    local base_name
    local regex='\(([^)]+)\)[[:space:]]*$'
    if [[ "$original_name" =~ $regex ]]; then
        #Extract content between last parentheses
        base_name="${BASH_REMATCH[1]}"
    else
        # No parentheses, use original name
        base_name=$(basename "$original_name")
    fi
    #Remove extension file
    base_name=${base_name%.*}

    ###### Filtering min and maximum read depth ######
    output_file="${vcf_dir}/${base_name}_DP.vcf.gz"
    output_file_n="${vcf_dir}/${base_name}_DP.vcf"

    #Estimate maximum reading depth as twice the average reading depth

    if grep -q "##FORMAT=<ID=DP" "$vcf"; then
        MAX_RD=$(bcftools query -f '[%DP\n]' "$vcf" | \
            awk '$1!="." {sum+=$1; n++} END {printf "%.2f", 2*sum/n}')
            echo "  Maximum read depth: $MAX_RD"
        bcftools filter -S . -e "FMT/DP<=${MIN_DP} | FMT/DP>=${MAX_RD}" -O z -o "$output_file" "$vcf"
        gunzip "$output_file"
        final_vcf="${output_file%.gz}"

    elif grep -q -e "##INFO=<ID=DP" "$vcf"; then
        MAX_RD=$(bcftools query -f '[%DP\n]' "$vcf" | \
            awk '$1!="." {sum+=$1; n++} END {printf "%.2f", 2*sum/n}')
            echo "  Maximum read depth: $MAX_RD"
        bcftools filter -S . -e "INFO/DP<=${MIN_DP} | INFO/DP>=${MAX_RD}" -O z -o "$output_file" "$vcf"
        gunzip "$output_file"
        final_vcf="${output_file%.gz}"
            
    else
        echo "No read depth data for $vcf file"
        cp "$vcf" "$output_file_n"
        final_vcf="${output_file_n}"
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


##### Execution #####
vcf_filtering_depth "$vcf_input" "$vcf_names" "$MIN_DP"
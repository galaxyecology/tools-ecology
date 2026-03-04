#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_names="$2"

output_dir="ind_list_directory"

##### Check output directory #####
if [[ ! -d "${output_dir}" ]]; then
    echo "ERROR: Failed to create output Indlist directory" >&2
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

##########################################################
#Function: vcf_list_ind
#Description: Extract the individual list from a VCF file
##########################################################

vcf_list_ind(){
    ##### Parameters #####
    local vcf="$1"
    local original_name="$2"

        ##### Check if file exists #####
        if [[ ! -f "$vcf" ]]; then
            echo "File not found, ignored: $vcf"
            continue
        fi

        local base_name
        local regex='\(([^)]+)\)[[:space:]]*$'
        if [[ "$original_name" =~ $regex ]]; then
            #Extract content between last parentheses
            base_name="${BASH_REMATCH[1]}"
        else
            # No parentheses, use original name
            base_name=$(basename "$original_name")
        fi
        base_name=${base_name%.vcf}

        ##### Extract individual list #####
        output_file="${output_dir}/${base_name}_indlist.tabular"

        bcftools query -l "${vcf}" > "${output_file}"

}

##### Execution #####
vcf_list_ind "$vcf_input" "$vcf_names"
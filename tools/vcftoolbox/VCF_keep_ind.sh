#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_names="$2"
list_ind="$3"

output_dir="vcf_directory"

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

if [[ -z "$list_ind" ]]; then
    echo "ERROR: Input list of individuals is not provided." >&2
    exit 1
fi

##########################################################
#Function: vcf_remove_ind
#Description: Remove individual from a list
##########################################################

vcf_remove_ind(){
    ##### Parameters #####
    local vcft="$1"
    local original_name="$2"
    local list_ind="$3"

    ##### Check if file exists #####
    if [[ ! -f "$vcf" ]]; then
        echo "File not found, ignored: $vcf"
        return
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

    ##### Remove individuals #####
    output_file="${output_dir}/${base_name}.vcf"

    bcftools view -S "${list_ind}" "${vcf}" -o "${output_file}" --force-samples #keep individuals on the list_ind

    ##### Verify that filtered VCF is not empty ######
        if [[ ! -f "$output_file" ]]; then
            echo "ERROR: Output VCF not created: $output_file" >&2
            exit 1
        fi

        if ! bcftools view -H "$output_file" | head -n 1 | grep -q .; then
            echo "ERROR: Filtered VCF contains no variants."
            exit 1
        fi  
}

##### Execution #####
vcf_remove_ind "$vcf_input" "$vcf_names" "$list_ind"

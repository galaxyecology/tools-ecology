#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_name="$2"
action="$3"
list_ind="$4"

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

##### Build output filename #####
#Extract basename
regex='\(([^)]+)\)[[:space:]]*$'
    if [[ "$original_name" =~ $regex ]]; then
        #Extract content between last parentheses
        base_name="${BASH_REMATCH[1]}"
    else
        # No parentheses, use original name
        base_name=$(basename "$vcf_name")
    fi
        
    base_name=${base_name%.vcf}

# Output file name
output_file="${output_dir}/${base_name}.vcf"

##### Main execution #####
if [[ "$action" == "keep" ]]; then
    # Keep only the individuals listed in list_ind
    echo "Keeping individuals listed"
    bcftools view -S "${list_ind}" "${vcf_input}" -o "${output_file}" --force-samples
else
    # Remove the individuals listed in list_ind
    echo "Removing individuals listed"
    bcftools view -S "^${list_ind}" "${vcf_input}" -o "${output_file}" --force-samples #remove individuals on the list_ind
fi

##### Verify that filtered VCF is not empty ######
    if [[ ! -f "$output_file" ]]; then
        echo "ERROR: Output VCF not created: $output_file" >&2
        exit 1
    fi

    if ! bcftools view -H "$output_file" | head -n 1 | grep -q .; then
        echo "ERROR: Filtered VCF contains no variants."
        exit 1
    fi  

##### SUmmary #####
n_ind_b=$(bcftools query -l "$vcf_input" | wc -l)
n_ind_a=$(bcftools query -l "$output_file" | wc -l)

echo "Individuals before: ${n_ind_b}"
echo "Individuals after: ${n_ind_a}"

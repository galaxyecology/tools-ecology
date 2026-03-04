#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_names="$2"
MAX_MISSING_IND="$3"

vcf_dir="vcf_filtered_directory"
summ_dir="summary"

##### Check output directories ####
if [[ ! -d "${vcf_dir}" ]]; then
    echo "ERROR: Failed to create output VCF directory" >&2
    exit 1
fi

if [[ ! -d "${summ_dir}" ]]; then
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

summary_file="summary/n_individuals.tabular"
echo -e "Dataset\tN_individuals_before\tN_individuals_after" >"${summary_file}"

###################################################
#Function: vcf_filtering_IND_missing_data
#Description: 
###################################################

vcf_filtering_IND_missing_data(){
    ##### Parameters #####
    local vcf="$1"
    local original_name="$2"
    local MAX_MISSING_IND="$3"
        
        ##### Check if file exists #####
        if [[ ! -f "$vcf" ]]; then
            echo "File not found, ignored: $vcf"
            return
        fi

        # Extract base name (handle .vcf)
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

        ##### Filtering on individuals with a high amount of missing data #####
        output_file="$vcf_dir/${base_name}_mdIND.vcf"

        intermed_files="${base_name}_IND_MISSING_DATA"

        vcftools --vcf  "$vcf" --missing-indv --out "$intermed_files"

        imiss="${intermed_files}.imiss"

        ind_miss="${base_name}_ind_missing_SNPs.txt" #list of individuals to be retained
        
        awk -v threshold="$MAX_MISSING_IND" 'NR > 1 && $5 < threshold { print $1 }' "$imiss" > "$ind_miss"

        bcftools view -S "$ind_miss" -O v -o "$output_file" "$vcf"

        ##### Verify that filtered VCF is not empty ######
        if [[ ! -f "$output_file" ]]; then
            echo "ERROR: Output VCF not created: $final_vcf" >&2
            exit 1
        fi

        if ! bcftools view -H "$output_file" | head -n 1 | grep -q .; then
            echo "ERROR: Filtered VCF contains no variants."
            exit 1
        fi  

        ##### Count individuals before and after filtering #####
        n_ind_before=$(bcftools query -l "$vcf" | wc -l)
        n_ind_after=$(bcftools query -l "$output_file" | wc -l)

        ##### Append results to output file #####
        echo -e "${base_name}\t${n_ind_before}\t${n_ind_after}" >> "${summary_file}"
        
}

##################
# Main execution
##################

vcf_filtering_IND_missing_data "$vcf_input" "$vcf_names" "$MAX_MISSING_IND"
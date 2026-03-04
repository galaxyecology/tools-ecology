#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_names="$2"
MAC="$3"

vcf_dir="vcf_filtered_directory"
summ_dir="summary"

##### Check output directories #####
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

summary_file="summary/n_snps.tabular"
echo -e "Dataset\tN_SNPs_before\tN_SNPs_after_MAC" >"${summary_file}"


######################################################
#Function: vcf_filtering_MAC
#Description: Applied minor allele count filter
######################################################

vcf_filtering_MAC(){
    ##### Parameters #####
    local vcf="$1"
    local original_name="$2"
    local MAC="$3"

        ##### Check if file exists #####
        if [[ ! -f "$vcf" ]]; then
            echo "File not found, ignored: $vcf"
            continue
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

        #Final output file
        output_file_gz="${vcf_dir}/${base_name}_MAC.vcf.gz"
        output_file="${vcf_dir}/${base_name}_MAC.vcf"


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

        ##### Count individuals before and after filtering #####
        n_snp_before=$(bcftools view -H "$vcf" | wc -l)
        n_snp_after=$(bcftools view -H "$output_file" | wc -l)

        ##### Append results to output file #####
        echo -e "${base_name}\t${n_snp_before}\t${n_snp_after}" >> "${summary_file}"

}


vcf_filtering_MAC "$vcf_input" "$vcf_names" "$MAC"
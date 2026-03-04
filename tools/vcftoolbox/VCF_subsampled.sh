#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_names="$2"
SUBSET_SNPS_NB="$3"
NB_REPLICATE_VCF="$4"
MIN_SNP_NB="$5"
POLY="$6"

vcf_dir_sub="vcf_subsampled"

##### Check output directory ####
if [[ ! -d "${vcf_dir_sub}" ]]; then
    echo "ERROR: Failed to create output VCF directory: ${vcf_dir_sub}" >&2
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
#Function: vcf_subsampled
#Description : VCF subsampling with a given number of SNPs
##############################################################

vcf_subsampled(){
    ##### Parameters #####
    local vcf="$1"
    local original_name="$2"
    local SUBSET_SNPS_NB="$3"
    local NB_REPLICATE_VCF="$4"
    local MIN_SNP_NB="$5"

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

        # Extract full SNP list
        local full_snps_list="${base_name}_full.txt"
        bcftools query -f '%CHROM\t%POS\n' "$vcf" > "$full_snps_list"

        #Perform the requested number of subsamples
        for j in $(seq 1 "$NB_REPLICATE_VCF"); do

                # Random selection of SNPs
                local subsample_snps="${base_name}_subsample_${j}.txt"
                shuf -n "$SUBSET_SNPS_NB" "$full_snps_list" | sort > "$subsample_snps"

                # Create subsampled VCF
                local output_vcf="$vcf_dir_sub/${base_name}_${SUBSET_SNPS_NB}snps_subsample_${j}.vcf"
                vcftools --vcf "$vcf" --positions "$subsample_snps" --recode --stdout > "$output_vcf" 2>/dev/null

                ##### Verify that filtered VCF is not empty ######
                if [[ ! -f "$output_vcf" ]]; then
                    echo "ERROR: Output VCF not created: $output_file" >&2
                    exit 1
                fi

                if ! bcftools view -H "$output_vcf" | head -n 1 | grep -q .; then
                    echo "ERROR: Filtered VCF contains no variants."
                    exit 1
                fi  
        done
}

######################
# Main execution
######################

##### Check if file exists #####
        if [[ ! -f "$vcf_input" ]]; then
            echo "File not found, ignored: $vcf_input"
            exit 1
        fi

        # Extract base name (handle .vcf)
        regex='\(([^)]+)\)[[:space:]]*$'
        if [[ "$vcf_name" =~ $regex ]]; then
            #Extract content between last parentheses
            base_name="${BASH_REMATCH[1]}"
        else
            # No parentheses, use original name
            base_name=$(basename "$vcf_name")
        fi
        
        base_name=${base_name%.vcf}

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

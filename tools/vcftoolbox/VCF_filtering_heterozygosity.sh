#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_names="$2"
MAX_Ho="$3"

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
echo -e "Dataset\tN_SNPs_before\tN_SNPs_after_Ho" >"${summary_file}"

#######################################################################
#Function: vcf_filtering_biallelic
#Description: Applies a filter to conserve only biallelic SNP
#######################################################################

vcf_filtering_heterozygosity(){
    ##### Parameters #####
    local vcf="$1"
    local original_name="$2"
    local MAX_Ho="$3"

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

        ##### Heterozygosity filter #####
        output_file="${vcf_dir}/${base_name}_Ho.vcf"
        positions_file="${vcf_dir}/${base_name}_positions.txt"
        
        # Calculating observed heterozygosity for each variant and conserve position
        #Het : number of heterozygotes
        #total : number of inidividuals with valid genotype
        echo "Calculating observed heterozygosity for each variant..." >&2
        bcftools query -f '%CHROM\t%POS[\t%GT]\n' "$vcf" | \
        awk -v prop="${MAX_Ho}" 'BEGIN {OFS="\t"} {
            ho=0; total=0;
            for(i=3; i<=NF; i++) {
                if($i != "./." && $i != ".|.") {
                    total++;
                    if($i ~ /0\/1/ || $i ~ /1\/0/ || $i ~ /0\|1/ || $i ~ /1\|0/) ho++;
                }
            }
            if(total > 0 && (ho/total) < prop) print $1, $2;
        }' > "$positions_file"
        
        # Check whether variants have passed the filter
        if [[ -s "$positions_file" ]]; then
            echo "Extracting filtered variants..." >&2
            bcftools view -T "$positions_file" -Ov -o "$output_file" "$vcf"
            
            n_variants=$(wc -l < "$positions_file")
            echo "Successfully created: $output_file (${n_variants} variants kept)" >&2
        else
            echo "WARNING: No variants passed the filter (Ho < ${MAX_Ho}) for $vcf" >&2
            bcftools view -h "$vcf" > "$output_file"
            echo "Created empty VCF with header only: $output_file" >&2
        fi
        
        ##### Verify that filtered VCF is not empty ######
        if [[ ! -f "$output_file" ]]; then
            echo "ERROR: Output VCF not created: $final_vcf" >&2
            exit 1
        fi

        if ! bcftools view -H "$output_file" | head -n 1 | grep -q .; then
            echo "ERROR: Filtered VCF contains no variants."
            exit 1
        fi  

        # Cleanup
        rm -f "$positions_file"

        ##### Count individuals before and after filtering #####
        n_snp_before=$(bcftools view -H "$vcf" | wc -l)
        n_snp_after=$(bcftools view -H "$output_file" | wc -l)

        ##### Append results to output file #####
        echo -e "${base_name}\t${n_snp_before}\t${n_snp_after}" >> "${summary_file}"

        

}

##################
# Main execution
##################

vcf_filtering_heterozygosity "$vcf_input" "$vcf_names" "$MAX_Ho"
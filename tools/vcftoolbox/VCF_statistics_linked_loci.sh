#!/bin/bash

# Print an error message to stderr and exit with code 1
die() {
    echo "ERROR: $*" >&2
    exit 1
}

##### Load arguments #####
vcf_input=""
vcf_name=""
max_SNPs=""

#Parse named flags
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --input)            vcf_input="$2";           shift ;;
        --name)             vcf_name="$2";            shift ;;
        --max_SNPs)         max_SNPs="$2";            shift ;;
        *) echo "Unknown argument: $1"; exit 1 ;;
    esac
    shift
done

##### Validate inputs #####
# Ensure bcftools and vcftools are available in PATH
command -v bcftools >/dev/null 2>&1 || die "bcftools is not installed or not in PATH."
command -v vcftools >/dev/null 2>&1 || die "vcftools is not installed or not in PATH."

# Check that input files exist on disk
[[ -f "$vcf_input" ]] || die "Input VCF was not found: $vcf_input"

# Check taht input VCF is not empty
if ! bcftools view -H "$vcf_input" | head -n 1 | grep -q .; then
    die "Input VCF contains no variant records"
fi

##### Output directory #####
readonly output_dir="r2_results_directory"
temp="temporary"
temp_dir="vcf_tmp_preprocessing"

##### Build output filename #####
name_without_ext="$(basename -- "$vcf_name")"
name_without_ext="${name_without_ext%.vcf.gz}"
name_without_ext="${name_without_ext%.vcf}"

# In Galaxy, dataset names may contain a trailing label in parentheses,
# e.g. "Tool name (dataset 42)". Extract the content inside the last
# parentheses if present; otherwise use the full name.
regex='\(([^)]+)\)[[:space:]]*$'
if [[ "$name_without_ext" =~ $regex ]]; then
    base_name="${BASH_REMATCH[1]}"
else
    base_name="$name_without_ext"
fi

[[ -n "$base_name" ]] || die "Could not derive a valid output filename from: $vcf_name"

###############################################################################################################
# Function : add_missing_contigs
# Description : Somes VCF do not include #contig= lines, which causes bcftools to faile in sample-subset mode.
# This function extracts all unique #CHROM values from the VCF body and injects them as ##contig = lines
# into the header, producing a fixed VCF that bcftools can process fully
###############################################################################################################

add_missing_contigs(){
    local vcf_in="$1"
    local -n _out_var="$2"          # nameref: writes directly into the caller's variable
 
    local vcf_out="${temp_dir}/input_reheadered.vcf"
 
    # If contig lines already present, return the original path unchanged
    if bcftools view -h "$vcf_in" 2>/dev/null | grep -q "^##contig="; then
        echo "INFO: ##contig= lines already present, skipping reheadering." >&2
        _out_var="$vcf_in"
        return
    fi
 
    echo "INFO: Adding missing ##contig= lines to VCF header..." >&2
 
    local tmp_header
    tmp_header=$(mktemp)
 
    # Rebuild header: original lines minus #CHROM, then contig lines, then #CHROM
    bcftools view -h "$vcf_in" 2>/dev/null | grep -v "^#CHROM"  >  "$tmp_header"
    bcftools view -H "$vcf_in" 2>/dev/null | awk '{print $1}' | sort -u | \
        awk '{print "##contig=<ID=" $1 ">"}' >> "$tmp_header"
    bcftools view -h "$vcf_in" 2>/dev/null | grep "^#CHROM"     >> "$tmp_header"
 
    bcftools reheader -h "$tmp_header" "$vcf_in" > "$vcf_out"
    rm -f "$tmp_header"
 
    if [[ ! -s "$vcf_out" ]]; then
        echo "WARNING: reheadering failed, using original VCF." >&2
        _out_var="$vcf_in"
        return
    fi
 
    echo "INFO: Reheadered VCF written to $vcf_out" >&2
    _out_var="$vcf_out"
}

#########################################################
# Function : add_snp_ids.sh
# Description : vcftools --geno-r2 need ID on ID column.
# This function add ID when there are missing.
##########################################################
add_snp_ids() {
    local input="$1"
    
    new_file="$temp/${base_name}_ID.vcf"

    bcftools annotate --set-id '%CHROM\_%POS\_%REF\_%ALT' "$input" -i 'ID="."' -O v -o "$new_file"

    if [[ ! -s "$new_file" ]]; then
        echo "WARNING: add_snp_ids failed, using original VCF." >&2
        _out_var="$input"
        return
    fi

    _out_var="$new_file"
}

# Apply reheadering — result goes directly into CURRENT_VCF via nameref
add_missing_contigs "$vcf_input" CURRENT_VCF

#Add ID
add_snp_ids "$CURRENT_VCF" CURRENT_VCF

temp_file="r2_LD"
temp_path="$temp/${temp_file}.geno.ld"

vcftools --vcf "$CURRENT_VCF" --geno-r2 --ld-window "$max_SNPs" --out "$temp/$temp_file"

output_file="$output_dir/$temp_file.geno.ld.txt"
cp "$temp_path" "$output_file"

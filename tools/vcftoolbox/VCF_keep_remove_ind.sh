#!/usr/bin/env bash

set -euo pipefail

# Print an error message to stderr and exit with code 1
die() {
    echo "ERROR: $*" >&2
    exit 1
}

# Count the number of variant records in a VCF file
count_variants() {
    bcftools view -H "$1" | awk 'END { print NR }'
}

# Count the number of individuals (samples) in a VCF file
count_individuals() {
    bcftools query -l "$1" | awk 'END { print NR }'
}

##### Load arguments #####
vcf_input=""
vcf_name=""
action=""
list_ind=""

# Parse named flags; each flag consumes its value with a first shift,
# then the outer shift moves to the next flag.
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --input)    vcf_input="$2";  shift ;;
        --name)     vcf_name="$2";   shift ;;
        --action)   action="$2";     shift ;;
        --list)     list_ind="$2";   shift ;;
        *) die "Unknown argument: $1" ;;
    esac
    shift
done

##### Output directory #####
readonly output_dir="vcf_directory"
temp_dir="vcf_tmp_preprocessing"

##### Validate inputs #####
# Ensure bcftools is available in PATH
command -v bcftools >/dev/null 2>&1 || die "bcftools is not installed or not in PATH."

# Check that all required arguments were provided
[[ -n "$vcf_name" ]] || die "VCF name is not provided."
[[ -n "$vcf_input" ]] || die "Input VCF was not found: $vcf_input"
[[ -n "$action"    ]] || die "Action is not provided (--action)."
[[ -n "$list_ind"  ]] || die "Individual list is not provided (--list)."

# Check that input files exist on disk
[[ -f "$vcf_input" ]] || die "Input VCF was not found: $vcf_input"
[[ -f "$list_ind"  ]] || die "Individual list was not found: $list_ind"

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
 
# Apply reheadering — result goes directly into CURRENT_VCF via nameref
add_missing_contigs "$vcf_input" CURRENT_VCF
vcf_input="$CURRENT_VCF"

# Check taht input VCF is not empty
input_variant_count="$(count_variants "$vcf_input")"
(( input_variant_count > 0 )) || die "Input VCF contains no variant records."

# Validate the action value
case "$action" in
    keep|remove) ;;
    *) die "Action must be 'keep' or 'remove', got: $action" ;;
esac

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

output_file="${output_dir}/${base_name}.vcf"

##### Main execution #####

if [[ "$action" == "keep" ]]; then
    echo "Keeping individuals listed"
    bcftools view -S "$list_ind" "$vcf_input" -o "$output_file" --force-samples
else
    echo "Removing individuals listed"
    bcftools view -S "^${list_ind}" "$vcf_input" -o "$output_file" --force-samples
fi

##### Verify that filtered VCF is not empty #####
[[ -f "$output_file" ]] || die "Output VCF was not created: $output_file"

output_variant_count="$(count_variants "$output_file")"
(( output_variant_count > 0 )) || die "Filtered VCF contains no variants."

##### Summary #####
n_ind_b="$(count_individuals "$vcf_input")"
n_ind_a="$(count_individuals "$output_file")"

echo "Individuals before: ${n_ind_b}"
echo "Individuals after: ${n_ind_a}"

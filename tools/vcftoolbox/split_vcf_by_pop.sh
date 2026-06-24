#!/bin/bash

# Print an error message to stderr and exit with code 1
die() {
    echo "ERROR: $*" >&2
    exit 1
}

##### Load arguments
vcf_input=""
vcf_names=""
indpop_file=""

# Parse named flags; each flag consumes its value with a first shift,
# then the outer shift moves to the next flag.
while [[ "$#" -gt 0 ]]; do
    case $1 in
        --input)            vcf_input="$2";           shift ;;
        --name)             vcf_names="$2";            shift ;;
        --indpop)           indpop_file="$2";               shift ;;
        *) echo "Unknown argument: $1"; exit 1 ;;
    esac
    shift
done

##### Output directory #####
vcf_dir="vcf_split_directory" 
temp_dir="vcf_tmp_preprocessing"

##### Validate inputs #####
# Ensure bcftools is available in PATH
command -v bcftools >/dev/null 2>&1 || die "bcftools is not installed or not in PATH."

# Check that input files exist on disk
[[ -f "$vcf_input" ]] || die "Input VCF was not found: $vcf_input"
[[ -f "$indpop_file" ]] || die "Input VCF was not found: $indpop_file"

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
if ! bcftools view -H "$vcf_input" | head -n 1 | grep -q .; then
    die "Input VCF contains no variant records"
fi

##### Build output filename #####
name_without_ext="$(basename -- "$vcf_names")"
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

##### Check indpop_file structure #####
awk -F'\t' 'NF != 2 && NF != 0 { print "ERROR: Population file does not contain exactly 2 tab-separated columns (line " NR ")" > "/dev/stderr"; found=1 } END { exit found }' "$indpop_file" \
    || die "Population file has an invalid structure. Expected: 2 tab-separated columns and no header."

echo "INFO: indpop_file structure OK."

##### Global variables ######
pop_file_list="${vcf_dir}/population_files_list.txt"

#########################################################
# Function: split_individuals_by_pop
#########################################################

split_individuals_by_pop(){
    local indpop_file="$1"

    [[ ! -f "$indpop_file" ]] && die "ERROR: Population file not found: $indpop_file"

    declare -A pop_inds

    while IFS=$'\t' read -r ind pop; do
        # Strip carriage returns in case of CRLF file
        ind="${ind%$'\r'}"
        pop="${pop%$'\r'}"
        [[ -z "$ind" || -z "$pop" ]] && continue
        pop_inds[$pop]+="${ind}"$'\n'
    done < "$indpop_file"

    [[ "${#pop_inds[@]}" -eq 0 ]] && die "ERROR: No populations detected in population file."

    > "$pop_file_list"

    for pop in "${!pop_inds[@]}"; do
        local output_file="${vcf_dir}/Ind_list_${pop}.txt"
        echo -n "${pop_inds[$pop]}" > "$output_file"
        echo "$output_file|$pop" >> "$pop_file_list"
        echo "Created ind list: $output_file ($(wc -l < "$output_file") individuals)" >&2
    done

    local pop_count="${#pop_inds[@]}"
    echo "INFO: ${pop_count} population(s) detected: ${!pop_inds[*]}"

    echo "Pop_file_list contents:" >&2
    cat "$pop_file_list" >&2

    unset pop_inds
}

#########################################################
# Function: split_vcf_by_pop
#########################################################

split_vcf_by_pop() {
    local vcf="$1"

    if [[ ! -f "$vcf" ]]; then
        echo "ERROR: VCF file not found: $vcf" >&2
        exit 1
    fi

    [[ ! -f "$pop_file_list" ]] && die "ERROR: Population files list not found: $pop_file_list"

    echo "pop_file_list has $(wc -l < "$pop_file_list") lines" >&2

    local vcf_created=0

    while IFS='|' read -r ind_list pop_name || [[ -n "$ind_list" ]]; do
        # Strip carriage returns
        ind_list="${ind_list%$'\r'}"
        pop_name="${pop_name%$'\r'}"

        echo "Reading line -> ind_list='$ind_list' pop_name='$pop_name'" >&2

        if [[ ! -f "$ind_list" ]]; then
            echo "ind_list file not found, skipping: $ind_list" >&2
            continue
        fi

        local output_vcf="${vcf_dir}/${base_name}_${pop_name}.vcf"

        bcftools view -S "$ind_list" --force-samples "$vcf" -Ov -o "$output_vcf"

        [[ ! -s "$output_vcf" ]] &&  die "ERROR: Output VCF is empty or missing for population '${pop_name}': $output_vcf"

        bcftools view -H "$output_vcf" 2>/dev/null | head -n 1 | grep -q . \
            || die "Output VCF contains no variants for population '${pop_name}': $output_vcf"

        echo "INFO: VCF successfully created for population '${pop_name}': $output_vcf"
        vcf_created=$(( vcf_created + 1 ))
        echo "vcf_created = $vcf_created" >&2

    done < "$pop_file_list"

    [[ "$vcf_created" -eq 0 ]] && die "No VCF files were created. Check your population file and VCF sample names."

    echo "INFO: ${vcf_created} VCF file(s) successfully created."
}

########################################
# Main execution
########################################
main(){
    local vcf_input="$1"
    local indpop_file="$2"

    split_individuals_by_pop "$indpop_file"
    split_vcf_by_pop "$vcf_input"
}

main "$vcf_input" "$indpop_file"

# Cleanup temporary files
rm -f "${vcf_dir}"/Ind_list_*.txt
rm -f "${pop_file_list}"

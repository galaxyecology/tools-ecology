#!/bin/bash

#Exit on error
set -e

vcf_input="$1"
vcf_names="$2"
indpop_file="$3"

# FIX: use absolute path based on $PWD to avoid any relative path issues
vcf_dir="vcf_split_directory"

##### Verify that the output directory exists #####
if [[ ! -d "${vcf_dir}" ]]; then
    echo "ERROR: Output VCF directory not found: ${vcf_dir}" >&2
    exit 1
fi

##### Check input files #####
if [[ -z "$vcf_input" ]]; then
    echo "ERROR: VCF file is not provided." >&2
    exit 1
fi

if [[ ! -f "$vcf_input" ]]; then
    echo "ERROR: VCF file not found: $vcf_input" >&2
    exit 1
fi

set +e
first_variant=$(bcftools view -H "$vcf_input" 2>/dev/null | head -n 1)
set -e
if [[ -z "$first_variant" ]]; then
    echo "ERROR: Input VCF contains no variant records." >&2
    exit 1
fi

if [[ -z "$indpop_file" ]]; then
    echo "ERROR: Population file is empty or not provided." >&2
    exit 1
fi

if [[ ! -f "$indpop_file" ]]; then
    echo "ERROR: Population file not found: $indpop_file" >&2
    exit 1
fi

##### Check indpop_file structure #####
error_found=0
line_num=0
while IFS= read -r line || [[ -n "$line" ]]; do
    line_num=$(( line_num + 1 ))
    [[ -z "$line" ]] && continue
    col_count=$(echo "$line" | awk -F'\t' '{print NF}')
    if [[ "$col_count" -ne 2 ]]; then
        echo "ERROR: Population file does not contain exactly 2 tab-separated columns (line ${line_num})" >&2
        error_found=1
    fi
done < "$indpop_file"

if [[ "$error_found" -eq 1 ]]; then
    echo "ERROR: Population file has an invalid structure. Expected: 2 tab-separated columns and no header." >&2
    exit 1
fi

echo "INFO: indpop_file structure OK."

##### Global variables ######
pop_file_list="${vcf_dir}/population_files_list.txt"

#########################################################
# Function: split_individuals_by_pop
#########################################################

split_individuals_by_pop(){
    local indpop_file="$1"

    if [[ ! -f "$indpop_file" ]]; then
        echo "ERROR: Population file not found: $indpop_file" >&2
        exit 1
    fi

    declare -A pop_inds

    while IFS=$'\t' read -r ind pop; do
        # Strip carriage returns in case of CRLF file
        ind="${ind%$'\r'}"
        pop="${pop%$'\r'}"
        [[ -z "$ind" || -z "$pop" ]] && continue
        pop_inds[$pop]+="${ind}"$'\n'
    done < "$indpop_file"

    if [[ "${#pop_inds[@]}" -eq 0 ]]; then
        echo "ERROR: No populations detected in population file." >&2
        exit 1
    fi

    > "$pop_file_list"

    for pop in "${!pop_inds[@]}"; do
        local output_file="${vcf_dir}/Ind_list_${pop}.txt"
        echo -n "${pop_inds[$pop]}" > "$output_file"
        echo "$output_file|$pop" >> "$pop_file_list"
        echo "DEBUG: Created ind list: $output_file ($(wc -l < "$output_file") individuals)" >&2
    done

    local pop_count="${#pop_inds[@]}"
    echo "INFO: ${pop_count} population(s) detected: ${!pop_inds[*]}"

    echo "DEBUG: pop_file_list contents:" >&2
    cat "$pop_file_list" >&2

    unset pop_inds
}

#########################################################
# Function: split_vcf_by_pop
#########################################################

split_vcf_by_pop() {
    local vcf="$1"
    local original_name="$2"

    if [[ ! -f "$vcf" ]]; then
        echo "ERROR: VCF file not found: $vcf" >&2
        exit 1
    fi

    if [[ ! -f "$pop_file_list" ]]; then
        echo "ERROR: Population files list not found: $pop_file_list" >&2
        exit 1
    fi

    echo "DEBUG: pop_file_list has $(wc -l < "$pop_file_list") lines" >&2

    local vcf_created=0

    while IFS='|' read -r ind_list pop_name || [[ -n "$ind_list" ]]; do
        # Strip carriage returns
        ind_list="${ind_list%$'\r'}"
        pop_name="${pop_name%$'\r'}"

        echo "DEBUG: Reading line -> ind_list='$ind_list' pop_name='$pop_name'" >&2

        if [[ ! -f "$ind_list" ]]; then
            echo "DEBUG: ind_list file not found, skipping: $ind_list" >&2
            continue
        fi

        # Extract base name
        local vcf_basename
        local regex='\(([^)]+)\)[[:space:]]*$'
        if [[ "$original_name" =~ $regex ]]; then
            vcf_basename="${BASH_REMATCH[1]}"
        else
            vcf_basename=$(basename "$original_name")
        fi
        vcf_basename=${vcf_basename%.*}

        local output_vcf="${vcf_dir}/${vcf_basename}_${pop_name}.vcf"

        bcftools view -S "$ind_list" --force-samples "$vcf" -Ov -o "$output_vcf"

        if [[ ! -s "$output_vcf" ]]; then
            echo "ERROR: Output VCF is empty or missing for population '${pop_name}': $output_vcf" >&2
            exit 1
        fi

        set +e
        first_variant=$(bcftools view -H "$output_vcf" 2>/dev/null | head -n 1)
        set -e
        if [[ -z "$first_variant" ]]; then
            echo "ERROR: Output VCF contains no variants for population '${pop_name}': $output_vcf" >&2
            exit 1
        fi

        echo "INFO: VCF successfully created for population '${pop_name}': $output_vcf"
        vcf_created=$(( vcf_created + 1 ))
        echo "DEBUG: vcf_created = $vcf_created" >&2

    done < "$pop_file_list"

    if [[ "$vcf_created" -eq 0 ]]; then
        echo "ERROR: No VCF files were created. Check your population file and VCF sample names." >&2
        exit 1
    fi

    echo "INFO: ${vcf_created} VCF file(s) successfully created."
}

########################################
# Main execution
########################################
main(){
    local vcf_input="$1"
    local vcf_names="$2"
    local indpop_file="$3"

    split_individuals_by_pop "$indpop_file"
    split_vcf_by_pop "$vcf_input" "$vcf_names"
}

main "$vcf_input" "$vcf_names" "$indpop_file"

# Cleanup temporary files
rm -f "${vcf_dir}"/Ind_list_*.txt
rm -f "${pop_file_list}"
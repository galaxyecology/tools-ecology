#!/bin/bash

# Print an error message to stderr and exit with code 1
die() {
    echo "ERROR: $*" >&2
    exit 1
}

##### Load arguments #####
vcf_input=""
vcf_name=""
ORDER=""
MAX_MISSING_IND=""
MAX_MISSING_LOCI=""
MIN_GQ=""
MIN_DP=""
MAC=""
MAX_Ho=""

# Parse named flags; each flag consumes its value with a first shift,
# then the outer shift moves to the next flag.
while [[ "$#" -gt 0 ]]; do
    [[ "$#" -ge 2 ]] || die "Missing value for argument: $1"
    case $1 in
        --input)            vcf_input="$2";           shift ;;
        --name)             vcf_name="$2";            shift ;;
        --order)            ORDER="$2";               shift ;;
        --max-missing-ind)  MAX_MISSING_IND="$2";     shift ;;
        --max-missing-loci) MAX_MISSING_LOCI="$2";    shift ;;
        --min-gq)           MIN_GQ="$2";              shift ;;
        --min-dp)           MIN_DP="$2";              shift ;;
        --min-mac)          MAC="$2";                 shift ;;
        --max-ho)           MAX_Ho="$2";              shift ;;
        *) echo "Unknown argument: $1"; exit 1 ;;
    esac
    shift
done

##### Output directory #####
readonly vcf_dir="vcf_filtered_directory"
readonly summ_dir="summary"
tmp_dir="vcf_filtered_tmp"
temp_dir="vcf_tmp_preprocessing"
mkdir -p "$vcf_dir" "$summ_dir" "$tmp_dir" "$temp_dir"

##### Validate inputs #####
# Ensure bcftools and vcftools are available in PATH
command -v bcftools >/dev/null 2>&1 || die "bcftools is not installed or not in PATH."
command -v vcftools >/dev/null 2>&1 || die "vcftools is not installed or not in PATH."

# Check that input files exist on disk
[[ -f "$vcf_input" ]] || die "Input VCF was not found: $vcf_input"

# Repair headers whose #CHROM line contains stray whitespace inside a column name
# (e.g. "INFO    "), which makes bcftools fail with "Could not parse the #CHROM line"
if ! bcftools view -h "$vcf_input" >/dev/null 2>&1; then
    echo "WARNING: VCF header could not be parsed; stripping stray whitespace from the #CHROM line." >&2
    fixed_vcf="${temp_dir}/input_header_fixed.vcf"
    zcat -f "$vcf_input" | \
        awk 'BEGIN {FS=OFS="\t"} /^#CHROM/ {for (i=1; i<=NF; i++) gsub(/[[:space:]]+/, "", $i)} {print}' \
        > "$fixed_vcf"
    vcf_input="$fixed_vcf"
    bcftools view -h "$vcf_input" >/dev/null 2>&1 || die "VCF header could not be parsed, even after whitespace cleanup."
fi

# Check that input VCF is not empty (stderr hidden: contig warnings are handled below)
if ! bcftools view -H "$vcf_input" 2>/dev/null | head -n 1 | grep -q .; then
    die "Input VCF contains no variant records"
fi

# The filters act on genotypes: a sites-only VCF cannot be processed
if ! bcftools query -l "$vcf_input" 2>/dev/null | head -n 1 | grep -q .; then
    die "Input VCF contains no individuals (sites-only VCF)"
fi

##### Filter order and parameters #####
# Normalise the order string: upper case, no spaces
ORDER="${ORDER^^}"
ORDER="${ORDER//[[:space:]]/}"
IFS=',' read -ra FILTERS <<< "$ORDER"

require_number() {
    [[ "$2" =~ ^([0-9]+([.][0-9]*)?|[.][0-9]+)$ ]] || die "Invalid value for $1: '$2' (expected a non-negative number)."
}

require_fraction() {
    require_number "$1" "$2"
    awk -v v="$2" 'BEGIN {exit !(v <= 1)}' || die "Invalid value for $1: '$2' (expected a number between 0 and 1)."
}

# Validate the parameters of the selected filters before doing any work
for FILTER in "${FILTERS[@]}"; do
    case $FILTER in
        A) require_fraction "--max-missing-ind"  "$MAX_MISSING_IND" ;;
        B) require_fraction "--max-missing-loci" "$MAX_MISSING_LOCI" ;;
        C) require_number   "--min-gq"           "$MIN_GQ" ;;
        D) require_number   "--min-dp"           "$MIN_DP" ;;
        E) ;;
        F) require_number   "--min-mac"          "$MAC" ;;
        G) require_fraction "--max-ho"           "$MAX_Ho" ;;
        *) die "Unknown filter '$FILTER' in order string." ;;
    esac
done

[[ "${#FILTERS[@]}" -gt 0 ]] || echo "WARNING: no filter selected, the input VCF is returned unchanged." >&2

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

##### Initialize summary #####
SUMMARY="${summ_dir}/summary.tabular"
echo -e "File\tStep\tFilter\tParameters\tN_individuals\tN_SNPs" > "$SUMMARY"

log_stats() {
    local step="$1"
    local filter="$2"
    local params="$3"
    local vcf="$4"
    local n_ind n_snps
    n_ind=$(bcftools query -l "$vcf" | wc -l)
    n_snps=$(bcftools view -H "$vcf" | wc -l)
    echo -e "${base_name}\t${step}\t${filter}\t${params}\t${n_ind}\t${n_snps}" >> "$SUMMARY"
}

###############################################################################################################
# Helper functions shared by the filters
###############################################################################################################

# Verify that a filtered VCF exists and still contains individuals and variants; exit otherwise
check_not_empty() {
    local label="$1"
    local file="$2"

    [[ -s "$file" ]] || die "${label} : Output VCF not created: ${file}"

    if ! bcftools query -l "$file" 2>/dev/null | head -n 1 | grep -q .; then
        die "${label} : Filtered VCF contains no individuals."
    fi

    if ! bcftools view -H "$file" 2>/dev/null | head -n 1 | grep -q .; then
        die "${label} : Filtered VCF contains no variants."
    fi
}

# Return 0 if the header declares the tag: header_has_tag <vcf> <INFO|FORMAT> <TAG>
# The trailing comma makes the match exact (GQ does not match GQX, DP does not match DP4)
header_has_tag() {
    bcftools view -h "$1" 2>/dev/null | grep -q "^##$2=<ID=$3,"
}

# Print how many distinct non-missing values a bcftools query format returns,
# capped at 2 (0 = no value at all, 1 = constant, 2 = at least two different values).
# A header declaration is not enough: records may declare a tag without ever carrying it.
count_distinct_values() {
    local query_format="$1"
    local vcf="$2"
    bcftools query -f "$query_format" "$vcf" 2>/dev/null | awk '
        $1 != "." && $1 != "" {
            if (n == 0)          { first = $1; n = 1 }
            else if ($1 != first) { n = 2; exit }
        }
        END { print n + 0 }'
}

###############################################################################################################
# Function : fix_vcf_header
# Description : Some VCF have an incomplete header, which makes bcftools fail in sample-subset mode
# ("Undefined tags in the header, cannot proceed in the sample subset mode"):
#   - ##contig= lines missing (or only some of them);
#   - INFO / FORMAT / FILTER tags used in the records but never declared.
# This function finds them and injects the missing header lines. It also guarantees that the pipeline
# works on an uncompressed text VCF (the input may be a bgzipped VCF or a BCF).
###############################################################################################################

# Print the ##INFO / ##FORMAT header line for an undeclared tag: declare_tag <INFO|FORMAT> <TAG>
# DP and GQ are declared as integers (the depth and quality filters compare them to numbers);
# GT is a string; any other tag is declared as a free-text string, as bcftools itself assumes.
declare_tag() {
    local number="." type="String"
    case "$1:$2" in
        FORMAT:GT)                          number="1" ;;
        INFO:DP|FORMAT:DP|INFO:GQ|FORMAT:GQ) number="1"; type="Integer" ;;
    esac
    echo "##$1=<ID=$2,Number=${number},Type=${type},Description=\"Declared by VCF_filtering: tag missing from the input header\">"
}

is_plain_vcf() {
    [[ "$(head -c 13 "$1")" == "##fileformat=" ]]
}

fix_vcf_header(){
    local vcf_in="$1"
    local -n _out_var="$2"          # nameref: writes directly into the caller's variable

    local vcf_out="${temp_dir}/input_normalized.vcf"

    # Contigs present in the records but not declared in the header
    local missing_contigs
    missing_contigs=$(LC_ALL=C comm -13 \
        <(bcftools view -h "$vcf_in" 2>/dev/null | sed -n 's/^##contig=<ID=\([^,>]*\).*/\1/p' | LC_ALL=C sort -u) \
        <(bcftools query -f '%CHROM\n' "$vcf_in" 2>/dev/null | uniq | LC_ALL=C sort -u))

    # Undeclared INFO / FORMAT / FILTER tags: bcftools reports each of them once while parsing the records
    local parse_warnings info_tags format_tags filter_tags
    parse_warnings=$(bcftools view -H "$vcf_in" 2>&1 >/dev/null)
    info_tags=$(echo "$parse_warnings"   | sed -n "s/^\[W::vcf_parse_info\] INFO '\([^']*\)' is not defined.*/\1/p" | LC_ALL=C sort -u)
    format_tags=$(echo "$parse_warnings" | sed -n "s/^\[W::vcf_parse_format[a-z0-9_]*\] FORMAT '\([^']*\)'.*is not defined.*/\1/p" | LC_ALL=C sort -u)
    filter_tags=$(echo "$parse_warnings" | sed -n "s/^\[W::vcf_parse_filter\] FILTER '\([^']*\)' is not defined.*/\1/p" | LC_ALL=C sort -u)

    if [[ -z "$missing_contigs" && -z "$info_tags" && -z "$format_tags" && -z "$filter_tags" ]]; then
        # Header already complete: keep the original file if it is a plain-text VCF
        if is_plain_vcf "$vcf_in"; then
            echo "INFO: header is complete, no reheadering needed." >&2
            _out_var="$vcf_in"
            return
        fi
        echo "INFO: converting input to an uncompressed VCF..." >&2
        bcftools view -Ov -o "$vcf_out" "$vcf_in" || die "Could not convert the input to an uncompressed VCF."
        _out_var="$vcf_out"
        return
    fi

    [[ -z "$missing_contigs" ]] || echo "INFO: Adding $(echo "$missing_contigs" | wc -l) missing ##contig= line(s) to the VCF header..." >&2
    [[ -z "$info_tags" ]]       || echo "INFO: Declaring INFO tag(s) missing from the header: $(echo $info_tags)" >&2
    [[ -z "$format_tags" ]]     || echo "INFO: Declaring FORMAT tag(s) missing from the header: $(echo $format_tags)" >&2
    [[ -z "$filter_tags" ]]     || echo "INFO: Declaring FILTER value(s) missing from the header: $(echo $filter_tags)" >&2

    local tmp_header tag
    tmp_header=$(mktemp "${temp_dir}/header.XXXXXX")

    # Rebuild header: original lines minus #CHROM, then the missing lines, then #CHROM
    bcftools view -h "$vcf_in" 2>/dev/null | grep -v "^#CHROM" >  "$tmp_header"
    if [[ -n "$missing_contigs" ]]; then
        echo "$missing_contigs" | awk '{print "##contig=<ID=" $1 ">"}' >> "$tmp_header"
    fi
    for tag in $info_tags;   do declare_tag INFO   "$tag" >> "$tmp_header"; done
    for tag in $format_tags; do declare_tag FORMAT "$tag" >> "$tmp_header"; done
    for tag in $filter_tags; do echo "##FILTER=<ID=${tag},Description=\"Declared by VCF_filtering: value missing from the input header\">" >> "$tmp_header"; done
    bcftools view -h "$vcf_in" 2>/dev/null | grep "^#CHROM"    >> "$tmp_header"

    bcftools reheader -h "$tmp_header" "$vcf_in" 2>/dev/null | bcftools view -Ov -o "$vcf_out" 2>/dev/null
    rm -f "$tmp_header"

    if [[ ! -s "$vcf_out" ]]; then
        # Fall back to the original file, only if the pipeline can read it as is
        is_plain_vcf "$vcf_in" || die "Reheadering failed and the input is not a plain-text VCF."
        echo "WARNING: reheadering failed, using original VCF." >&2
        _out_var="$vcf_in"
        return
    fi

    echo "INFO: Reheadered VCF written to $vcf_out" >&2
    _out_var="$vcf_out"
}

# Apply reheadering / normalisation — result goes directly into CURRENT_VCF via nameref
fix_vcf_header "$vcf_input" CURRENT_VCF

# Temporary vcf to make connexion between the diferent filter
STEP=0
SUFFIX=""

# Log initial
log_stats 0 "input" "raw" "$CURRENT_VCF"

###################################################
#Function: vcf_filtering_IND_missing_data
#Description:Remove individuals with missing data
###################################################

vcf_filtering_IND_missing_data(){
    ##### Parameters #####
    local vcf="$1"
    local MAX_MISSING_IND="$2"
    local tag="_mdIND"

        echo ">>> [Step $STEP] Filter A: Individuals missing data (threshold: $MAX_MISSING_IND)"

        ##### Filtering on individuals with a high amount of missing data #####
        local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"
        local intermed_files="${tmp_dir}/${base_name}_IND_MISSING_DATA"
        local ind_miss="${tmp_dir}/${base_name}_ind_missing_SNPs.txt"  #list of individuals to be retained
        local imiss="${intermed_files}.imiss"

        vcftools --vcf  "$vcf" --missing-indv --out "$intermed_files" || die "Filter A : vcftools failed while computing missing data per individual."

        [[ -s "$imiss" ]] || die "Filter A : Missing-data report not created: $imiss"

        awk -v threshold="$MAX_MISSING_IND" 'NR > 1 && $5 < threshold { print $1 }' "$imiss" > "$ind_miss"

        ##### Stop early, with an explicit reason, if no individual passes the threshold #####
        if [[ ! -s "$ind_miss" ]]; then
            local min_miss
            min_miss=$(awk 'NR > 1 && (min == "" || $5 < min) {min = $5} END {print min}' "$imiss")
            die "Filter A : no individual passes MAX_MISSING_IND=${MAX_MISSING_IND} (lowest missing-data fraction observed: ${min_miss})."
        fi

        bcftools view -S "$ind_miss" -O v -o "$output_file" "$vcf" || die "Filter A : bcftools failed while subsetting individuals (see the message above; a frequent cause is a tag used in the records but not declared in the header)."

        ##### Verify that filtered VCF is not empty ######
        check_not_empty "Filter A" "$output_file"

        CURRENT_VCF="$output_file"
        SUFFIX="${SUFFIX}${tag}"

        #Complete summary file
        log_stats "$STEP" "A_ind_missing" "MAX_MISSING_IND=${MAX_MISSING_IND}" "$CURRENT_VCF"

}

######################################################
#Function: vcf_filtering_SNP_missingdata_MAC
#Description: Remove SNPs with missing data
#######################################################

vcf_filtering_SNP_missingdata(){
    ##### Parameters #####
    local vcf="$1"
    local MAX_MISSING_LOCI="$2"
    local tag="_SNPmd"

        echo ">>> [Step $STEP] Filter B: Loci missing data (threshold: $MAX_MISSING_LOCI)"

        ##### Remove SNPs with a high amount of missing data #####
        local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf" #Final output file

        bcftools filter -e "F_MISSING > ${MAX_MISSING_LOCI}" -O v -o "$output_file" "$vcf" || die "Filter B : bcftools filter failed."

        ##### Verify that filtered VCF is not empty ######
        check_not_empty "Filter B" "$output_file"

        CURRENT_VCF="$output_file"
        SUFFIX="${SUFFIX}${tag}"
        log_stats "$STEP" "B_loci_missing" "MAX_MISSING_LOCI=${MAX_MISSING_LOCI}" "$CURRENT_VCF"

}


##############################################################
#Function: vcf_filtering_gen_qual
#Description: Filters variants based on genotype quality (GQ)
# and replaces low-quality genotypes with missing values.
# FORMAT/GQ (per genotype) is preferred; INFO/GQ (per site) is
# used as a fallback and removes the whole site. The filter is
# skipped if the VCF carries no usable GQ values.
##############################################################

vcf_filtering_gen_qual(){
    ##### Parameters #####
    local vcf="$1"
    local MIN_GQ="$2"
    local tag="_GQ"

        echo ">>> [Step $STEP] Filter C: Genotype quality (threshold: $MIN_GQ)"

        ###### Filtering variants based on genotype quality (GQ))######
        local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"

        local gq_params="MIN_GQ=${MIN_GQ}"

        if header_has_tag "$vcf" FORMAT GQ && [[ "$(count_distinct_values '[%GQ\n]' "$vcf")" -ge 1 ]]; then
            bcftools filter -S . -e "FMT/GQ<${MIN_GQ}" -O v -o "$output_file" "$vcf" || die "Filter C : bcftools filter failed on FORMAT/GQ."

        elif header_has_tag "$vcf" INFO GQ && [[ "$(count_distinct_values '%INFO/GQ\n' "$vcf")" -ge 2 ]]; then
            bcftools filter -e "INFO/GQ<${MIN_GQ}" -O v -o "$output_file" "$vcf" || die "Filter C : bcftools filter failed on INFO/GQ."

        else
            echo "WARNING: no usable genotype quality data (no FORMAT/GQ values; INFO/GQ absent or constant) in $base_name - GQ filter skipped" >&2
            cp "$vcf" "$output_file" #Allows the pipeline to continue without filtering
            gq_params="No GQ field found"
        fi

        ##### Verify that filtered VCF is not empty ######
        check_not_empty "Filter C" "$output_file"

        CURRENT_VCF="$output_file"
        SUFFIX="${SUFFIX}${tag}"
        log_stats "$STEP" "C_GQ" "${gq_params}" "$CURRENT_VCF"
}

#######################################################################
#Function: vcf_filtering_depth
#Description: Applies a filter for reading depth (minimum and maximum)
# FORMAT/DP (per genotype) is preferred; INFO/DP (per site) is used as
# a fallback and removes the whole site. The filter is skipped if the VCF
# carries no usable depth values (absent, or constant across all sites).
#######################################################################

vcf_filtering_depth(){
    ##### Parameters #####
    local vcf="$1"
    local MIN_DP="$2"
    local tag="_DP"

    echo ">>> [Step $STEP] Filter D: Depth coverage (threshold: $MIN_DP)"

    ###### Filtering min and maximum read depth ######
    local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"

    #Estimate maximum reading depth as twice the average reading depth

    local dp_params="MIN_DP=${MIN_DP}"
    local MAX_RD=""

    # A header declaration is not enough: the records must actually carry DP values
    if header_has_tag "$vcf" FORMAT DP && [[ "$(count_distinct_values '[%DP\n]' "$vcf")" -ge 1 ]]; then
        MAX_RD=$(bcftools query -f '[%DP\n]' "$vcf" | \
            awk '$1!="." {sum+=$1; n++} END {if (n>0) printf "%.2f", 2*sum/n}')
        [[ -n "$MAX_RD" ]] || die "Filter D : could not estimate mean depth (no FORMAT/DP values found)."
        echo "  Maximum read depth: $MAX_RD"
        bcftools filter -S . -e "FMT/DP<=${MIN_DP} | FMT/DP>=${MAX_RD}" -O v -o "$output_file" "$vcf" || die "Filter D : bcftools filter failed on FORMAT/DP."
        dp_params="MIN_DP=${MIN_DP};MAX_DP=${MAX_RD}"

    # INFO/DP is a per-site value: it must vary between sites to carry depth information
    elif header_has_tag "$vcf" INFO DP && [[ "$(count_distinct_values '%INFO/DP\n' "$vcf")" -ge 2 ]]; then
        MAX_RD=$(bcftools query -f '%INFO/DP\n' "$vcf" | \
            awk '$1!="." {sum+=$1; n++} END {if (n>0) printf "%.2f", 2*sum/n}')
        [[ -n "$MAX_RD" ]] || die "Filter D : could not estimate mean depth (no INFO/DP values found)."
        echo "  Maximum read depth: $MAX_RD"
        bcftools filter -e "INFO/DP<=${MIN_DP} | INFO/DP>=${MAX_RD}" -O v -o "$output_file" "$vcf" || die "Filter D : bcftools filter failed on INFO/DP."
        dp_params="MIN_DP=${MIN_DP};MAX_DP=${MAX_RD}"

    else
        echo "WARNING: no usable read depth data (no FORMAT/DP values; INFO/DP absent or constant) in $base_name - depth filter skipped" >&2
        cp "$vcf" "$output_file"
        dp_params="No DP field found"
    fi

    ##### Verify that filtered VCF is not empty ######
    check_not_empty "Filter D" "$output_file"

    CURRENT_VCF="$output_file"
    SUFFIX="${SUFFIX}${tag}"
    log_stats "$STEP" "D_DP" "${dp_params}" "$CURRENT_VCF"
}

#######################################################################
#Function: vcf_filtering_biallelic
#Description: Filters the VCF to retain only biallelic SNPs
#######################################################################

vcf_filtering_biallelic(){
    ##### Parameters #####
    local vcf="$1"
    local tag="_bialSNP"

    echo ">>> [Step $STEP] Filter E: Biallelic SNPs"

    ##### Keep biallelic SNPs only #####
    local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"

    # Apply filter
    bcftools view -v snps -m2 -M2 -O v -o "$output_file" "$vcf" || die "Filter E : bcftools view failed."

    ##### Verify that filtered VCF is not empty ######
    check_not_empty "Filter E" "$output_file"

    CURRENT_VCF="$output_file"
    SUFFIX="${SUFFIX}${tag}"
    log_stats "$STEP" "E_biallelic" "biallelic_SNPs_only" "$CURRENT_VCF"

}

######################################################
#Function: vcf_filtering_MAC
#Description: Applied minor allele count filter
######################################################

vcf_filtering_MAC(){
    ##### Parameters #####
    local vcf="$1"
    local MAC="$2"
    local tag="_MAC"

        echo ">>> [Step $STEP] Filter F: Minor allele count (threshold: $MAC)"

        #Final output file
        local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"

        bcftools filter -e "MAC < ${MAC}" -O v -o "$output_file" "$vcf" || die "Filter F : bcftools filter failed."

        ##### Verify that filtered VCF is not empty ######
        check_not_empty "Filter F" "$output_file"

        CURRENT_VCF="$output_file"
        SUFFIX="${SUFFIX}${tag}"
        log_stats "$STEP" "F_MAC" "MIN_MAC=${MAC}" "$CURRENT_VCF"

}

#######################################################################
#Function: vcf_filtering_heterozygosity
#Description: Applies a filter to conserve only variants with an observed
# heterozygosity below the threshold. A genotype is heterozygous when its
# alleles differ (works for multi-allelic sites); genotypes with a missing
# allele are ignored.
#######################################################################

vcf_filtering_heterozygosity(){
    ##### Parameters #####
    local vcf="$1"
    local MAX_Ho="$2"
    local tag="_Ho"

        echo ">>> [Step $STEP] Filter G: Heterozygosity (threshold: $MAX_Ho)"

        ##### Heterozygosity filter #####
        local output_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.vcf"
        local positions_file="${tmp_dir}/${base_name}${SUFFIX}${tag}.txt"

        # Calculating observed heterozygosity for each variant and conserve position
        #ho : number of heterozygotes
        #total : number of inidividuals with valid genotype
        echo "Calculating observed heterozygosity for each variant..." >&2
        bcftools query -f '%CHROM\t%POS[\t%GT]\n' "$vcf" | \
        awk -v prop="${MAX_Ho}" 'BEGIN {OFS="\t"} {
            ho=0; total=0;
            for(i=3; i<=NF; i++) {
                if($i ~ /\./) continue;
                total++;
                n = split($i, al, /[\/|]/);
                for(j=2; j<=n; j++) if(al[j] != al[1]) { ho++; break }
            }
            if(total > 0 && (ho/total) < prop) print $1, $2;
        }' > "$positions_file"
        [[ "${PIPESTATUS[0]}" -eq 0 ]] || die "Filter G : bcftools query failed."

        # Check whether variants have passed the filter
        [[ -s "$positions_file" ]] || die "Filter G : no variant passes the heterozygosity filter (Ho < ${MAX_Ho})."

        echo "Extracting filtered variants..." >&2
        bcftools view -T "$positions_file" -O v -o "$output_file" "$vcf" || die "Filter G : bcftools view failed."

        local n_variants
        n_variants=$(wc -l < "$positions_file")
        echo "Successfully created: $output_file (${n_variants} variants kept)" >&2

        ##### Verify that filtered VCF is not empty ######
        check_not_empty "Filter G" "$output_file"

        # Cleanup
        rm -f "$positions_file"

        CURRENT_VCF="$output_file"
        SUFFIX="${SUFFIX}${tag}"
        log_stats "$STEP" "G_heterozygosity" "MAX_Ho=${MAX_Ho}" "$CURRENT_VCF"

}

########################################################
# Main execution
#######################################################
for FILTER in "${FILTERS[@]}"; do
    STEP=$((STEP + 1))

    case $FILTER in
        A) vcf_filtering_IND_missing_data "$CURRENT_VCF" "$MAX_MISSING_IND"   ;;
        B) vcf_filtering_SNP_missingdata "$CURRENT_VCF" "$MAX_MISSING_LOCI" ;;
        C) vcf_filtering_gen_qual "$CURRENT_VCF" "$MIN_GQ" ;;
        D) vcf_filtering_depth "$CURRENT_VCF" "$MIN_DP" ;;
        E) vcf_filtering_biallelic "$CURRENT_VCF" ;;
        F) vcf_filtering_MAC "$CURRENT_VCF" "$MAC" ;;
        G) vcf_filtering_heterozygosity "$CURRENT_VCF" "$MAX_Ho" ;;
        *) die "Unknown filter '$FILTER' in order string." ;;
    esac
done

FINAL_OUTPUT="${vcf_dir}/${base_name}${SUFFIX}.vcf"
cp "$CURRENT_VCF" "$FINAL_OUTPUT"

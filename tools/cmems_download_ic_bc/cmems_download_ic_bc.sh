#!/usr/bin/env bash

# !/bin/bash -i
set -euo pipefail

MIN_LAT="${1}"
MAX_LAT="${2}"
MIN_LON="${3}"
MAX_LON="${4}"
MIN_DEPTH="${5}"
MAX_DEPTH="${6}"
START_DATE="${7}"
END_DATE="${8}"
BC_SOUTH="${9}"
BC_NORTH="${10}"
BC_WEST="${11}"
BC_EAST="${12}"

cm_xmin=`echo "${MIN_LON} - 0.1" | bc`
# store_var cm_xmin
cm_ymin=`echo "${MIN_LAT} - 0.1" | bc`
# store_var cm_ymin
cm_xmax=`echo "${MAX_LON} + 0.1" | bc`
# store_var cm_ymin
cm_ymax=`echo "${MAX_LAT} + 0.1" | bc`
# store_var cm_ymax

# Detect this script's directory (so we can find the Python helper file)
TOOL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo ">> startdate: ${START_DATE}"
echo ">> Tool directory set to: ${TOOL_DIR}"

# Define dataset IDs
REANALYSIS_TEM_DATASET_ID="med-cmcc-tem-rean-d"
FCANALYSIS_TEM_DATASET_ID="cmems_mod_med_phy-tem_anfc_4.2km_P1D-m"

# Define functions

check_product_availability(){
	local product_id="$1"
	local start_date="$2"
	local end_date="$3"
	
	echo ">> Checking availability for $product_id from $start_date to $end_date"
	
	# Get metadata in JSON
	local metadata_json
	copernicusmarine login --username "$CMEMS_USERNAME" --password "$CMEMS_PASSWORD" --force-overwrite
	metadata_json=$(copernicusmarine describe --dataset-id "$product_id" --return-fields datasets 2>/dev/null)
	
	if [ -z "$metadata_json" ]; then
		echo ">> Warning: Could not fetch metadata for $product_id"
		return 1
	fi

	echo ">> DEBUG: Metadata JSON for $product_id:"
	echo ">> $metadata_json"

	# Extract min/max time (milliseconds since epoch)
	local start_epoch_ms end_epoch_ms
	start_epoch_ms=$(echo "$metadata_json" | jq -r '.products[0].datasets[0].versions[0].parts[].services[] | select(.service_name == "arco-geo-series").variables[] | select(.short_name=="thetao").coordinates[] | select(.coordinate_id=="time") | .minimum_value')
	end_epoch_ms=$(echo "$metadata_json" | jq -r '.products[0].datasets[0].versions[0].parts[].services[] | select(.service_name == "arco-geo-series").variables[] | select(.short_name=="thetao").coordinates[] | select(.coordinate_id=="time") | .maximum_value')

	if [[ -z "$start_epoch_ms" || -z "$end_epoch_ms" ]]; then
		echo ">> Warning: Could not extract the time range in metadata for $product_id"
		return 1
	fi

	# Detect if we are on macOS
	local is_mac=false
	if [[ "$(uname)" == "Darwin" ]]; then
    		is_mac=true
	fi

	# Convert milliseconds to seconds (integer division)
	local product_start_epoch product_end_epoch
	product_start_epoch=$(( ${start_epoch_ms%.*} / 1000 ))
	product_end_epoch=$(( ${end_epoch_ms%.*} / 1000 ))

	# Convert input dates to epoch (seconds since Unix epoch macOS/Linux compatible)
	start_date=$(echo "$start_date" | xargs)
	end_date=$(echo "$end_date" | xargs)
	
	local start_date_epoch end_date_epoch
	if $is_mac; then
    		start_date_epoch=$(date -j -f "%Y-%m-%d" "$start_date" +"%s")
    		end_date_epoch=$(date -j -f "%Y-%m-%d" "$end_date" +"%s")
	else
    		start_date_epoch=$(date -d "$start_date" +"%s")
    		end_date_epoch=$(date -d "$end_date" +"%s")
	fi

	# Include the full last day
        end_date_epoch=$((end_date_epoch + 86399))

	if [[ -z "$start_date_epoch" || -z "$end_date_epoch" ]]; then
    		echo ">> ERROR: Failed to parse input dates: '$start_date', '$end_date'"
    		return 1
	fi

	# Convert product epoch to human-readable dates for logging
	local product_start_date product_end_date
	
	if $is_mac; then
    		product_start_date=$(date -u -r "$product_start_epoch" +"%Y-%m-%d")
    		product_end_date=$(date -u -r "$product_end_epoch" +"%Y-%m-%d")
	else
    		product_start_date=$(date -u -d @"$product_start_epoch" +"%Y-%m-%d")
    		product_end_date=$(date -u -d @"$product_end_epoch" +"%Y-%m-%d")
	fi

	echo ">> start_date_epoch=$start_date_epoch, end_date_epoch=$end_date_epoch"
	echo ">> product_start_epoch=$product_start_epoch, product_end_epoch=$product_end_epoch"	

	echo ">> Checking if date range $start_date → $end_date is within product range $product_start_date → $product_end_date for $product_id"
        
	# Check if requested range is fully within product range
	if [[ $start_date_epoch -ge $product_start_epoch && $end_date_epoch -le $product_end_epoch ]]; then
		return 0  # Entire  range is available
	else
		echo ">> Date range $start_date to $end_date is not fully covered by product $product_id"
		return 1  # Not available
	fi
	
}

select_datasets() {
	if check_product_availability "$REANALYSIS_TEM_DATASET_ID" "$START_DATE" "$END_DATE"; then
        	cm_tem_daily_product="$REANALYSIS_TEM_DATASET_ID"
	        cm_sal_daily_product="med-cmcc-sal-rean-d"
		cm_cur_daily_product="med-cmcc-cur-rean-d"
		prod_version="202012"
		echo  ">> Using Copernicus Mediterranean Sea Physics Reanalysis product for ${START_DATE} to ${END_DATE}"
	else
		cm_tem_daily_product="$FCANALYSIS_TEM_DATASET_ID"
		cm_sal_daily_product="cmems_mod_med_phy-sal_anfc_4.2km_P1D-m"
		cm_cur_daily_product="cmems_mod_med_phy-cur_anfc_4.2km_P1D-m"
		prod_version="202411"
		echo ">> Using Copernicus Mediterranean Sea Physics Analysis and Forecast product for ${START_DATE} to ${END_DATE}"
	fi
}

download_ic(){

	# log_print ">>"
	# log_print ">> Downloading ocean fields for initial conditions"
	# log_print ">> Bounding box petition: (xmin,xmax)=($cm_xmin,$cm_xmax), (ymin,ymax)=($cm_ymin,$cm_ymax)"

	for VAR in thetao so cur; do
		echo ">> Downloading variable $VAR..."

		case "$VAR" in
			thetao | so)
				case "$VAR" in
					thetao) FILENAME="IC_cm_t.nc"; DATASET="$cm_tem_daily_product" ;;
					so)     FILENAME="IC_cm_s.nc"; DATASET="$cm_sal_daily_product" ;;
				esac
				copernicusmarine subset \
					--dataset-id "$DATASET" \
					--dataset-version "${prod_version}" \
					--minimum-longitude "$cm_xmin" \
					--maximum-longitude "$cm_xmax" \
					--minimum-latitude "$cm_ymin" \
					--maximum-latitude "$cm_ymax" \
					--start-datetime "${START_DATE}T00:00:00" \
					--end-datetime "${END_DATE}T23:59:59" \
					--minimum-depth "$MIN_DEPTH" \
					--maximum-depth "$MAX_DEPTH" \
					--variable "$VAR" \
					--output-filename "$FILENAME" \
					--overwrite  ;;

			cur)  FILENAME="IC_cm_cur.nc"; DATASET="$cm_cur_daily_product"
				
				copernicusmarine subset \
					--dataset-id "$DATASET" \
					--dataset-version "${prod_version}" \
					--minimum-longitude "$cm_xmin" \
					--maximum-longitude "$cm_xmax" \
					--minimum-latitude "$cm_ymin" \
					--maximum-latitude "$cm_ymax" \
					--start-datetime "${START_DATE}T00:00:00" \
					--end-datetime "${END_DATE}T23:59:59" \
					--minimum-depth "$MIN_DEPTH" \
					--maximum-depth "$MAX_DEPTH" \
					-v uo -v vo \
					--output-filename "$FILENAME" \
					--overwrite  ;;
		esac

	done

}

download_obc(){
	echo ">>"
	echo ">> Dowloading data for ${OBC} open boundary..."
	echo ">> Bounding box petition: (xmin,xmax)=($cm_bc_xmin,$cm_bc_xmax), (ymin,ymax)=($cm_bc_ymin,$cm_bc_ymax)"

	for VAR in thetao so cur; do
		echo ">> Downloading variable $VAR for ${OBC} open boundary..."
		
		case "$VAR" in
			thetao | so)
				case "$VAR" in
					thetao) FILENAME="OB${OBC}_cm_t.nc"; DATASET="$cm_tem_daily_product" ;;
					so)     FILENAME="OB${OBC}_cm_s.nc"; DATASET="$cm_sal_daily_product" ;;
				esac
				copernicusmarine subset \
					--dataset-id "$DATASET" \
					--dataset-version "${prod_version}" \
					--minimum-longitude "$cm_bc_xmin" \
					--maximum-longitude "$cm_bc_xmax" \
					--minimum-latitude "$cm_bc_ymin" \
					--maximum-latitude "$cm_bc_ymax" \
					--start-datetime "${START_DATE}T00:00:00" \
					--end-datetime "${END_DATE}T23:59:59" \
					--minimum-depth "$MIN_DEPTH" \
					--maximum-depth "$MAX_DEPTH" \
					--variable "$VAR" \
					--coordinates-selection-method "nearest" \
					--output-filename "$FILENAME" \
					--overwrite   ;;

			cur)  FILENAME="OB${OBC}_cm_cur.nc"; DATASET="$cm_cur_daily_product" 
			
				copernicusmarine subset \
					--dataset-id "$DATASET" \
					--dataset-version "${prod_version}" \
					--minimum-longitude "$cm_bc_xmin" \
					--maximum-longitude "$cm_bc_xmax" \
					--minimum-latitude "$cm_bc_ymin" \
					--maximum-latitude "$cm_bc_ymax" \
					--start-datetime "${START_DATE}T00:00:00" \
					--end-datetime "${END_DATE}T23:59:59" \
					--minimum-depth "$MIN_DEPTH" \
					--maximum-depth "$MAX_DEPTH" \
					-v uo -v vo \
					--coordinates-selection-method "nearest" \
					--output-filename "$FILENAME" \
					--overwrite  ;;
					
		esac

	done
	
	echo "download obc finished"
}

#-------------------------------------------------------------------------------------
# Main execution
#--------------------------------------------------------------------------------------
# Determine which dataset is available for the date -----------------------------------

select_datasets

max_attempts=50
attempt=1

# Download the data for IC -------------------------------------------------------------

while (( attempt <= max_attempts )); do
	download_ic
	wait
	if [[ -f "IC_cm_cur.nc" && -f "IC_cm_s.nc" && -f "IC_cm_t.nc" ]]; then
		echo ">> All IC files are present after $attempt attempts"
		break
	else
		if (( attempt == max_attempts )); then
			echo ">> Files not found after $max_attempts attempts"
			exit 1
		else
			echo ">> Attempt $((attempt))/$max_attempts: Some files missing"
			((attempt++))
		fi
	fi
done

# Find model boundaries in Copernicus MedSea Products ---------------------------------

echo ">> DEBUG: calling -> python3 ${TOOL_DIR}/medsea_bc_limits.py --xmin='${MIN_LON}' --xmax='${MAX_LON}' --ymin='${MIN_LAT}' --ymax='${MAX_LAT}' --infile='IC_cm_t.nc'" >> debug.log

python3 "${TOOL_DIR}/medsea_bc_limits.py" --xmin="${MIN_LON}" --xmax="${MAX_LON}" --ymin="${MIN_LAT}" --ymax="${MAX_LAT}" --infile='IC_cm_t.nc'

echo ">> copernicus_lims.inc file created"


# Download the data for each boundary--------------------------------------------------

source copernicus_lims.inc

declare -a OBC_LABEL=("S" "N" "W" "E")
declare -a OBC_VALUE=(${BC_SOUTH} ${BC_NORTH} ${BC_WEST} ${BC_EAST})

NOBC=${#OBC_LABEL[@]}

for (( i=0; i<${NOBC}; i++ ));
do
	OBC=${OBC_LABEL[$i]}
	OBC_SELEC=${OBC_VALUE[$i]}

	echo "${OBC}"
	echo "${OBC_SELEC}"

	if [ "${OBC_SELEC}" == "true" ]; then
		if [ "${OBC}" == "S" ]; then
			cm_bc_xmin=${COPERNICUS_WEST_X1}
			cm_bc_xmax=${COPERNICUS_EAST_X2}
			cm_bc_ymin=${COPERNICUS_SOUTH_Y1}
			cm_bc_ymax=${COPERNICUS_SOUTH_Y2}

		elif [ "${OBC}" == "N" ]; then
			cm_bc_xmin=${COPERNICUS_WEST_X1}
			cm_bc_xmax=${COPERNICUS_EAST_X2} 
			cm_bc_ymin=${COPERNICUS_NORTH_Y1}
			cm_bc_ymax=${COPERNICUS_NORTH_Y2}

		elif [ "${OBC}" == "W" ]; then
			cm_bc_xmin=${COPERNICUS_WEST_X1}
			cm_bc_xmax=${COPERNICUS_WEST_X2}
			cm_bc_ymin=${COPERNICUS_SOUTH_Y1}
			cm_bc_ymax=${COPERNICUS_NORTH_Y2}
		
		elif [ "${OBC}" == "E" ]; then
			cm_bc_xmin=${COPERNICUS_EAST_X1}
			cm_bc_xmax=${COPERNICUS_EAST_X2}
			cm_bc_ymin=${COPERNICUS_SOUTH_Y1}
			cm_bc_ymax=${COPERNICUS_NORTH_Y2}

		fi

		max_attempts=50
		attempt=1
		echo ">> Starting obc download for ${OBC} boundary"

		while (( attempt <= max_attempts )); do
			download_obc
			echo ">> download obc files finished"
			wait
			if [[ -f "OB${OBC}_cm_cur.nc" && -f "OB${OBC}_cm_s.nc" && -f "OB${OBC}_cm_t.nc" ]]; then
				echo ">> All files for $OBC boundary are present after $attempt attempts"
				break
			else
				if (( attempt == max_attempts )); then
					echo ">> Files not found after $max_attempts attempts"
					exit 1
				else
					echo ">> Attempt $((attempt))/$max_attempts: Some files missing"
					((attempt++))
				fi
			fi
		done
	fi
done

# ---- Create ZIP of NetCDF outputs ----
ZIP_NAME="cmems_ic_bc_data.zip"
echo ">> Creating zip archive: ${ZIP_NAME}"

# Check for .nc files before zipping
NC_FILES=(*.nc)
if [ ${#NC_FILES[@]} -gt 0 ]; then
    echo ">> Found ${#NC_FILES[@]} NetCDF file(s) to zip:"
    ls -lh "${NC_FILES[@]}"

    # Zip all .nc files (quietly, replacing any existing zip)
    zip -j -r "$ZIP_NAME" "${NC_FILES[@]}" >/dev/null 2>&1

    if [ -f "$ZIP_NAME" ]; then
        echo ">> Zip file created successfully:"
        ls -lh "$ZIP_NAME"
    else
        echo ">> Zip creation failed for unknown reason."
        exit 1
    fi
else
    echo ">> No .nc files found to zip! Skipping zip creation."
    # Create an empty placeholder so Galaxy still has an output
    touch "$ZIP_NAME"
fi

echo ">> Final working directory check:"
pwd
ls -lh

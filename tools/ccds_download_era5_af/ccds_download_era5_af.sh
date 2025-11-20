#!/usr/bin/env bash
set -euo pipefail

MIN_LAT="${1}"
MAX_LAT="${2}"
MIN_LON="${3}"
MAX_LON="${4}"
START_DATE="${5}"
END_DATE="${6}"

echo ">> Dowloading ERA5 atmospheric fields"

# Detect this script's directory (so we can find the Python helper file)
TOOL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
echo ">> Tool directory set to: ${TOOL_DIR}"

era5_xmin=`echo "${MIN_LON} - 0.5" | bc`
era5_ymin=`echo "${MIN_LAT} - 0.5" | bc`
era5_xmax=`echo "${MAX_LON} + 0.5" | bc`
era5_ymax=`echo "${MAX_LAT} + 0.5" | bc`

echo ">> Requested bounding box is (lon_min, lon_max)=(${era5_xmin}, ${era5_xmax}) and (lat_min, lat_max)=(${era5_ymin}, ${era5_ymax})" 

max_attempts=50
attempt=1

#Download the data

while (( attempt <= max_attempts )); do
	python3 ${TOOL_DIR}/get_era5.py ${START_DATE} ${END_DATE} \
		  ${era5_xmin} ${era5_xmax} ${era5_ymin} ${era5_ymax}
	wait
	if [[ -f "ccds_era5_af_data.zip" ]]; then
		echo ">> File is present after $attempt attempts"
		break
	else
		if (( attempt == max_attempts )); then
			echo ">> File not found after $max_attempts attempts"
			exit 1
		else
			echo ">> Attempt $((attempt))/$max_attempts: File still missing"
			((attempt++))
		fi
	fi
done

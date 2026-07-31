"""Python script to fetch NASA Earth science data from Earthdata."""

import argparse
import os
from calendar import monthrange
from collections import defaultdict
from datetime import datetime, timedelta

import earthaccess

# ---------------------------
# Argument parser
# ---------------------------
parser = argparse.ArgumentParser()

parser.add_argument("--short_name", type=str, required=True)

parser.add_argument("--lat_min", type=float, required=True)
parser.add_argument("--lat_max", type=float, required=True)
parser.add_argument("--lon_min", type=float, required=True)
parser.add_argument("--lon_max", type=float, required=True)

parser.add_argument("--start_date", type=str, required=True)
parser.add_argument("--end_date", type=str, required=True)

parser.add_argument("--exclude_dates", type=str, required=False, default="",
                    help="Comma-separated dates to exclude (YYYY-MM-DD)")
parser.add_argument("--exclude_ranges", type=str, default="",
                    help="Comma-separated date ranges (YYYY-MM-DD:YYYY-MM-DD)")

parser.add_argument("--out_dir", type=str, required=True)
parser.add_argument("--out_file", type=str, required=True)

args = parser.parse_args()

# ---------------------------
# Parse excluded dates
# ---------------------------
excluded_dates = set()
if args.exclude_dates:
    excluded_dates = set(d.strip() for d in args.exclude_dates.split(","))

# ---------------------------
# Login
# ---------------------------
earthaccess.login(strategy="environment")


# ---------------------------
# Detect temporal resolution
# ---------------------------
def detect_resolution(short_name):
    """
    Detect the temporal resolution of a dataset using Earthaccess granules.

    This function queries the Earthaccess API for two granules of the given
    dataset (`short_name`), extracts their beginning timestamps, and computes
    the difference in days between them to infer the temporal resolution.

    Parameters
    ----------
    short_name : str
        The Earthdata short name identifying the dataset.

    Returns
    -------
    str
        The inferred temporal resolution:
        - "monthly" if the time difference is approximately 1 month (>=27days)
        - "daily" if the time difference is 1 day
        - "other" for any other interval
        - "unknown" if the resolution cannot be determined (e.g.,
        insufficient data or parsing errors)

    Notes
    -----
    - This method assumes that consecutive granules are representative of
    the dataset's temporal frequency.
    - If fewer than two granules are available or if metadata extraction
    fails, the function returns "unknown".
    """
    sample = earthaccess.search_data(
        short_name=short_name,
        count=2  # get a couple of granules
    )

    if len(sample) < 2:
        return "unknown"

    try:
        t0 = sample[0]["umm"]["TemporalExtent"][
            "RangeDateTime"
        ]["BeginningDateTime"]
        t1 = sample[1]["umm"]["TemporalExtent"][
            "RangeDateTime"
        ]["BeginningDateTime"]

        d0 = datetime.fromisoformat(t0.replace("Z", ""))
        d1 = datetime.fromisoformat(t1.replace("Z", ""))

        delta = abs((d1 - d0).days)

        if delta >= 27:
            return "monthly"
        elif delta == 1:
            return "daily"
        else:
            return "other"

    except Exception:
        return "unknown"


RESOLUTION = detect_resolution(args.short_name)

# ---------------------------
# Prepare excluded dates
# ---------------------------
excluded = excluded_dates

start = datetime.strptime(args.start_date, "%Y-%m-%d")
end = datetime.strptime(args.end_date, "%Y-%m-%d")

DOWNLOAD_PATH = args.out_dir
os.makedirs(DOWNLOAD_PATH, exist_ok=True)

all_files = []

# ---------------------------
# Parse excluded ranges
# format: YYYY-MM-DD:YYYY-MM-DD
# ---------------------------
if hasattr(args, "exclude_ranges") and args.exclude_ranges:
    for r in args.exclude_ranges.split(","):
        if ":" in r:
            start_r, end_r = r.split(":")
            start_r = datetime.strptime(start_r.strip(), "%Y-%m-%d")
            end_r = datetime.strptime(end_r.strip(), "%Y-%m-%d")

            current = start_r
            while current <= end_r:
                excluded_dates.add(current.strftime("%Y-%m-%d"))
                current += timedelta(days=1)

# ---------------------------
# MONTHLY logic
# ---------------------------
if RESOLUTION == "monthly":

    month_days = defaultdict(set)

    current = start
    while current <= end:
        date_str = current.strftime("%Y-%m-%d")
        month_key = current.strftime("%Y-%m")

        if date_str not in excluded:
            month_days[month_key].add(current.day)

        current += timedelta(days=1)

    valid_months = [m for m, days in month_days.items() if days]

    for month in sorted(valid_months):
        year, mon = map(int, month.split("-"))
        last_day = monthrange(year, mon)[1]

        start_date = f"{month}-01"
        end_date = f"{month}-{last_day:02d}"

        results = earthaccess.search_data(
            short_name=args.short_name,
            temporal=(start_date, end_date),
            bounding_box=(
                args.lon_min, args.lat_min,
                args.lon_max, args.lat_max)
        )

        if results and isinstance(results, list):
            files = earthaccess.download(results, DOWNLOAD_PATH)
            if files:
                all_files.extend(files)
        else:
            print(f"No data found for {start_date} → {end_date}")

# ---------------------------
# DAILY logic
# ---------------------------
else:
    current = start
    while current <= end:
        date_str = current.strftime("%Y-%m-%d")

        if date_str not in excluded:
            results = earthaccess.search_data(
                short_name=args.short_name,
                temporal=(date_str, date_str),
                bounding_box=(
                    args.lon_min, args.lat_min,
                    args.lon_max, args.lat_max)
            )

            if results and isinstance(results, list):
                files = earthaccess.download(results, DOWNLOAD_PATH)
                if files:
                    all_files.extend(files)
            else:
                print(f"No data found for {start_date} → {end_date}")

        current += timedelta(days=1)

# ---------------------------
# Output
# ---------------------------
with open(args.out_file, "w") as f:
    if all_files:
        for file in all_files:
            f.write(f"{file}\n")
    else:
        f.write("No files downloaded\n")

print(f"Detected temporal resolution: {RESOLUTION}")

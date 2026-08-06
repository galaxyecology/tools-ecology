"""Python script to fetch NASA Earth science data from Earthdata."""

import argparse
import os
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

parser.add_argument(
    "--exclude_dates",
    type=str,
    required=False,
    default="",
    help="Comma-separated dates to exclude (YYYY-MM-DD)"
)

parser.add_argument(
    "--exclude_ranges",
    type=str,
    default="",
    help="Comma-separated date ranges (YYYY-MM-DD:YYYY-MM-DD)"
)

parser.add_argument(
    "--resolution",
    choices=["daily", "monthly"],
    required=True,
    help="Increment dates daily or monthly")

parser.add_argument("--out_file", type=str, required=True)

ARGS = parser.parse_args()

# ---------------------------
# Login
# ---------------------------

# os.environ["EARTHDATA_USERNAME"] = ""
# os.environ["EARTHDATA_PASSWORD"] = ""

earthaccess.login(strategy="environment", persist=True)


def increment(current, step):
    """
    Increment a datetime object by one step based on the specified temporal \
    resolution.

    Parameters
    ----------
    current : datetime
        The current datetime to increment.
    step : str
        The step type. Supported values:
        - "monthly": advances to the first day of the next month
        - "daily": advances by one day

    Returns
    -------
    datetime
        A new datetime object incremented according to the specified step.

    Notes
    -----
    - For monthly increments, the returned date is always normalized to the
      first day of the next month to avoid invalid dates (e.g., transitioning
      from January 31 to February).
    - For daily increments, a standard timedelta of one day is applied.
    """
    if step == "monthly":
        year = current.year + (current.month // 12)
        month = (current.month % 12) + 1
        return datetime(year, month, 1)  # ALWAYS safe

    return current + timedelta(days=1)


# ---------------------------
# Parse & Prepare excluded dates
# ---------------------------
RESOLUTION = ARGS.resolution

DATE_FORMAT = "%Y-%m-%d"


def parse_date(value, name):
    """Validate date format."""
    try:
        return datetime.strptime(value, DATE_FORMAT)
    except ValueError:
        raise ValueError(
            f"Invalid {name}: expected YYYY-MM-DD"
        )


# ---------------------------
# Prepare dates
# ---------------------------
start = parse_date(ARGS.start_date, "start_date")
end = parse_date(ARGS.end_date, "end_date")

if start > end:
    raise ValueError("start_date must be earlier than or equal to end_date")

excluded = set()

# Single excluded dates
if ARGS.exclude_dates:
    for value in ARGS.exclude_dates.split(","):
        excluded.add(
            parse_date(value.strip(), "exclude_date")
            .strftime(DATE_FORMAT)
        )

# Excluded ranges
if ARGS.exclude_ranges:
    for r in ARGS.exclude_ranges.split(","):
        begin, finish = r.split(":")

        current = parse_date(begin.strip(), "range start")
        stop = parse_date(finish.strip(), "range end")

        while current <= stop:
            excluded.add(current.strftime(DATE_FORMAT))
            current = increment(current, ARGS.resolution)


# ---------------------------
# Prepare output path
# ---------------------------

DOWNLOAD_PATH = os.path.join(os.getcwd(), "Data")
os.makedirs(DOWNLOAD_PATH, exist_ok=True)

all_files = []

# ---------------------------
# Main retrieval loop
# ---------------------------
current = start

while current <= end:

    date_str = current.strftime(DATE_FORMAT)
    if date_str not in excluded:

        start_period = current
        end_period = current
        if RESOLUTION == "monthly":
            next_month = increment(current, "monthly")
            end_period = min(next_month - timedelta(days=1), end)

        results = earthaccess.search_data(
            short_name=ARGS.short_name,
            temporal=(
                start_period.strftime(DATE_FORMAT),
                end_period.strftime(DATE_FORMAT)
            ),
            bounding_box=(
                ARGS.lon_min, ARGS.lat_min,
                ARGS.lon_max, ARGS.lat_max)
        )

        if results:
            try:
                files = earthaccess.download(results, DOWNLOAD_PATH)
                if files:
                    all_files.extend(files)

            except Exception as e:
                print(f"Download failed: {e}")

    # increment month
    current = increment(current, ARGS.resolution)


# ---------------------------
# Output
# ---------------------------
with open(ARGS.out_file, "w") as f:
    if all_files:
        for file in all_files:
            f.write(f"{os.path.basename(file)}\n")
    else:
        f.write("No files downloaded\n")

print(f"Resolution: {RESOLUTION} | Files downloaded: \
      {len(os.listdir(DOWNLOAD_PATH))}")

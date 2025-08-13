"""Import OPeNDAP dataset using xarray to a netCDF file."""

import argparse

import xarray as xr

parser = argparse.ArgumentParser()

parser.add_argument(
    "opendap_url",
    help=(
        "A valid OPeNDAP URL, also see "
        "https://docs.xarray.dev/en/stable/user-guide/io.html#opendap"
    ),
)
parser.add_argument(
    "decode_times",
    type=lambda x: x == "true",
    help='If time should be decoded, e.g. "True" or "False"',
)
parser.add_argument(
    "decode_cf",
    type=lambda x: x == "true",
    help=(
        "Whether to decode according to "
        'CF conventions e.g. "true" or "false"'
    ),
)
parser.add_argument("output_dataset", help="netCDF file to output")
args = parser.parse_args()

xr.open_dataset(
    args.opendap_url.strip(),
    decode_cf=args.decode_cf,
    decode_times=args.decode_times,
).to_netcdf(args.output_dataset)

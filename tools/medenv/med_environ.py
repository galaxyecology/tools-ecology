# coding: utf-8

# Standard imports
import argparse
from datetime import datetime
import pathlib
import os
import functools

# External imports
import pandas as pd
import medenv


def environment_observation(
    row,
    fetcher,
    lat_key,
    long_key,
    depth_key,
    date_key,
    tol_spatial_coordinates,
    verbose,
):
    lat0 = (
        row[lat_key] - tol_spatial_coordinates / 2.0,
        row[lat_key] + tol_spatial_coordinates / 2.0,
    )
    long0 = (
        row[long_key] - tol_spatial_coordinates / 2.0,
        row[long_key] + tol_spatial_coordinates / 2.0,
    )
    depth = row[depth_key]
    date = datetime.strptime(row[date_key], "%Y-%m-%dT%H:%M:%SZ")

    values, info_values = fetcher.get_values(date, (long0, lat0), depth)

    if verbose:
        print(
            f"Was fetching for date={date}, long={long0}, lat={lat0}, depth={depth} and got {values} at the coordinates {info_values}"
        )
    for fname, fvalue in values.items():
        row[fname] = fvalue
    return row


def environment_dataset(args):
    # Note: galaxy is given us, for boolean parameters, the string
    # "true" or the string "false" that we need to convert to a bool
    verbose = True if args.verbose == "true" else False

    os.environ["CMEMS_USERNAME"] = args.cmems_username
    os.environ["CMEMS_PASSWORD"] = args.cmems_password

    features = args.variables.split(",")
    fetcher = medenv.Fetcher(features, reduction="mean")

    # Loads the input dataset
    df = pd.read_csv(args.datafile)

    df = df.apply(
        environment_observation,
        args=(
            fetcher,
            args.lat_key,
            args.long_key,
            args.depth_key,
            args.date_key,
            args.tol_spatial_coordinates,
            verbose,
        ),
        axis=1,
    )

    df.to_csv(args.out_file, header=True, index=False)


def __main__():
    parser = argparse.ArgumentParser()
    parser.add_argument("--datafile", type=pathlib.Path, required=True)
    parser.add_argument("--out_file", type=pathlib.Path, required=True)
    parser.add_argument("--lat_key", type=str, required=True)
    parser.add_argument("--long_key", type=str, required=True)
    parser.add_argument("--date_key", type=str, required=True)
    parser.add_argument("--depth_key", type=str, required=True)
    parser.add_argument("--variables", type=str, required=True)
    parser.add_argument("--tol_spatial_coordinates", type=float, required=True)
    parser.add_argument("--cmems_username", type=str, default=None)
    parser.add_argument("--cmems_password", type=str, default=None)
    parser.add_argument("--verbose", type=str, default=False)
    args = parser.parse_args()

    environment_dataset(args)


if __name__ == "__main__":
    __main__()

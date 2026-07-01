#!/usr/bin/env python3
"""Reproject CSV coordinates or WKT geometries between CRS using pyproj."""

import argparse

import pandas as pd
from pyproj import Transformer
from shapely import wkt as shapely_wkt
from shapely.ops import transform as shapely_transform


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input-crs", required=True)
    parser.add_argument("--output-crs", required=True)
    parser.add_argument("--csv", required=True)
    parser.add_argument("--lat-col", default="latitude")
    parser.add_argument("--lon-col", default="longitude")
    parser.add_argument("--wkt-col", default=None)
    return parser.parse_args()


def main():
    args = parse_args()

    df = pd.read_csv(args.csv)
    transformer = Transformer.from_crs(args.input_crs, args.output_crs, always_xy=True)

    if args.wkt_col:
        out_col = f"{args.wkt_col}_{args.output_crs}"

        def reproject_geom(wkt_str):
            geom = shapely_wkt.loads(wkt_str)
            return shapely_wkt.dumps(shapely_transform(transformer.transform, geom))

        df[out_col] = df[args.wkt_col].apply(reproject_geom)
    else:
        xs, ys = transformer.transform(df[args.lon_col].values, df[args.lat_col].values)

        lon_out = f"{args.lon_col}_{args.output_crs}"
        lat_out = f"{args.lat_col}_{args.output_crs}"

        df.rename(
            columns={
                args.lon_col: f"{args.lon_col}_{args.input_crs}",
                args.lat_col: f"{args.lat_col}_{args.input_crs}",
            },
            inplace=True,
        )
        df[lon_out] = xs
        df[lat_out] = ys

    df.to_csv("output.csv", index=False)


if __name__ == "__main__":
    main()

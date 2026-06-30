import argparse
import pandas as pd
from pyproj import Transformer
########
from shapely import wkt
from shapely.ops import transform as geom_transform

parser = argparse.ArgumentParser(
        description="Transform coordinates in a CSV file using pyproj"
    )

# CRS arguments
parser.add_argument("--input-crs", required=True, help="Input CRS (e.g. EPSG:4326)")
parser.add_argument("--output-crs", required=True, help="Output CRS (e.g. EPSG:3857)")

# CSV + column arguments
parser.add_argument("--csv", required=True, help="Path to input CSV file")
parser.add_argument("--lat-col", required=False, help="Latitude column name")
parser.add_argument("--lon-col", required=False, help="Longitude column name")
##########
parser.add_argument("--wkt-col", required=False, help="Column containing WKT geometries")

# Optional output
parser.add_argument("--out", default="output.csv", help="Output CSV file")

args = parser.parse_args()

 # Load data
df = pd.read_csv(args.csv)

# Create transformer (force lon/lat order for consistency)
transformer = Transformer.from_crs(
    args.input_crs,
    args.output_crs,
    always_xy=True
)

if args.wkt_col:
    # Cas géométrie WKT
    def project_geom(geom_wkt):
        geom = wkt.loads(geom_wkt)
        return geom_transform(transformer.transform, geom).wkt

    new_geom_col = f"{args.wkt_col}_{args.output_crs}"
    df[new_geom_col] = df[args.wkt_col].apply(project_geom)

    old_geom_col = f"{args.wkt_col}_{args.input_crs}"
    df.rename(columns={args.wkt_col: old_geom_col}, inplace=True)
else:
    # Transform coordinates
    x, y = transformer.transform(
        df[args.lon_col].values,
        df[args.lat_col].values
    )

    # Add results to dataframe
    old_lon_col = f"{args.lon_col}_{args.input_crs}"
    old_lat_col = f"{args.lat_col}_{args.input_crs}"
    new_lon_col = f"{args.lon_col}_{args.output_crs}"
    new_lat_col = f"{args.lat_col}_{args.output_crs}"
    df.rename(columns={
        args.lon_col: old_lon_col,
        args.lat_col: old_lat_col
    }, inplace=True)
    df[new_lon_col] = x
    df[new_lat_col] = y

# Save output
df.to_csv("output.csv", index=False)

print(f"Saved transformed coordinates to output.csv")

# Land Cover TE Algorithm Without GEE

# How to Execute:
# - Load the two images (.TIFF) of the rasters 
# to be processed in the "/data/land_cover/input" folder
# - Setting the two filenames in the specific cell of this script (0)
# - Run all cells of the script
# - Check the script results in the "/data/land_cover/output" folder
#    - Land Cover Degradation "/data/land_cover/output/lc_dg.tiff"
#   - Land Cover Transition "/data/land_cover/output/lc_tr.tiff"

# Librairies import
import json
import os
import argparse
import sys

import numpy as np
import rasterio
import matplotlib.pyplot as plt
from rasterio.plot import show
from matplotlib import image as im

from te_schemas.land_cover import LCLegendNesting
from te_schemas.land_cover import LCTransitionDefinitionDeg

# Methods to manage Rasters

def remap(raster, problem_numbers, alternative_numbers):
    n_min, n_max = raster.min(), raster.max()
    replacer = np.arange(n_min, n_max + 1)
    mask = (problem_numbers >= n_min) & (problem_numbers <= n_max)
    replacer[problem_numbers[mask] - n_min] = alternative_numbers[mask]
    raster_replaced = replacer[raster - n_min]
    return raster_replaced


def saveRaster(dataset, datasetPath, cols, rows, projection, namedataset=None):
    if namedataset:
        rasterSet = rasterio.open(datasetPath, 
                                 'w', 
                                 driver='GTiff', 
                                 height=rows, 
                                 width=cols, 
                                 count=1, 
                                 dtype=np.int8, 
                                 crs=projection, 
                                 transform=transform, )
        rasterSet.write(dataset, 1)
        rasterSet.close()
    else:
        rasterSet = rasterio.open(datasetPath, 
                                  'w', 
                                  driver='GTiff', 
                                  height=rows, 
                                  width=cols, 
                                  count=1, 
                                  dtype=np.int8, 
                                  crs=projection, 
                                  transform=transform, )
        rasterSet.write(dataset, 1)
        rasterSet.set_band_description(1, namedataset)
        rasterSet.close()


def plot(ndviImage, cmap):
    src = rasterio.open(ndviImage, 'r')
    fig, ax = plt.subplots(1, figsize=(12, 12))
    show(src, cmap=cmap, ax=ax)
    ax.set_xlabel('Est')
    ax.set_ylabel('Nord')
    plt.show()


def plotContour(ndviImage, cmap):
    src = rasterio.open(ndviImage, 'r')
    fig, ax = plt.subplots(1, figsize=(12, 12))
    show(src, cmap=cmap, contour=True, ax=ax)
    ax.set_xlabel('Est')
    ax.set_ylabel('Nord')
    plt.show()


# Setting inputs
command_line_args = sys.argv[1:]


parser = argparse.ArgumentParser(description="landcover inputs and outputs")
# Add arguments
print(parser)
parser.add_argument("--raster_1", help="raster 1")
parser.add_argument("--raster_2", help="raster 2")
parser.add_argument("--json", help="json")

args = parser.parse_args(command_line_args)

# Parse the command line arguments

# Import data

path_raster_yi = args.raster_1
path_raster_yf = args.raster_2
fjson = args.json
# Input Rasters

# Outputs
if not os.path.exists(os.getcwd()+"/data/land_cover/output/"): 
    os.makedirs(os.getcwd()+"/data/land_cover/output/") 

# Output Rasters
path_lc_tr = os.getcwd()+'/data/land_cover/output/lc_tr.tiff'
path_lc_bl = os.getcwd()+'/data/land_cover/output/lc_bl.tiff'
path_lc_dg = os.getcwd()+'/data/land_cover/output/lc_dg.tiff'
path_lc_tg = os.getcwd()+'/data/land_cover/output/lc_tg.tiff'
path_change_yf_yi = os.getcwd()+'/data/land_cover/output/change_yf_yi.tiff'
path_lc_baseline_esa = os.getcwd()+'/data/land_cover/output/lc_baseline_esa.tiff'
path_lc_target_esa = os.getcwd()+'/data/land_cover/output/lc_target_esa.tiff'
path_out_img = os.getcwd()+'/data/land_cover/output/stack.tiff'

# Contours
contours_change_yf_yi = os.getcwd()+'/data/land_cover/output/change_yf_yi0.shp'



# Parsing Inputs
# Load static inputs
# Transition Matrix, ESA Legend, IPCC Legend
#Input Raster and Vector Paths
params = json.load(open(fjson))
# print(params)

crs = params.get("crs")
# print("crs", crs)

trans_matrix = LCTransitionDefinitionDeg.Schema().load(params.get("trans_matrix"))
# print("trans_matrix", trans_matrix)

esa_to_custom_nesting = LCLegendNesting.Schema().load(
    params.get("legend_nesting_esa_to_custom")
)
# print("esa_to_custom_nesting", esa_to_custom_nesting)

ipcc_nesting = LCLegendNesting.Schema().load(
    params.get("legend_nesting_custom_to_ipcc")
)
# print("ipcc_nesting", ipcc_nesting)

class_codes = sorted([c.code for c in esa_to_custom_nesting.parent.key])
class_positions = [*range(1, len(class_codes) + 1)]

# Load dynamic inputs 
# Baseline ESA Raster
raster_yi = rasterio.open(path_raster_yi)
lc_baseline_esa = raster_yi.read(1)
yi_dict_profile = dict(raster_yi.profile)

for k in yi_dict_profile:
    print(k.upper(), yi_dict_profile[k])

# Target ESA Raster 
raster_yf = rasterio.open(path_raster_yf)
lc_target_esa = raster_yf.read(1)
yf_dict_profile = dict(raster_yf.profile)

for k in yf_dict_profile:
    print(k.upper(), yf_dict_profile[k])

# Check inputs for consistency

if raster_yi.crs.to_proj4()==raster_yf.crs.to_proj4(): print('SRC OK')

if raster_yi.shape==raster_yf.shape: print('Array Size OK')

if raster_yi.get_transform()==raster_yf.get_transform(): print('Geotransformation OK')

cols = raster_yi.width
print("Columns", cols)

rows = raster_yf.height
print("Rows", rows)

transform = raster_yi.transform
print("Transform", transform)

projection = raster_yi.crs
print("Projection", projection)

# Setting up output
saveRaster(lc_baseline_esa.astype('int8'), path_lc_baseline_esa, cols, rows, projection, "Land_cover_yi")
print("Baseline ESA Raster Saved:", path_lc_baseline_esa)

saveRaster(lc_target_esa.astype('int8'), path_lc_target_esa, cols, rows, projection, "Land_cover_yf")
print("Target ESA Raster Saved:", path_lc_target_esa)

# Algorithm execution
#Transition codes are based on the class code indices (i.e. their order when sorted by class code) - not the class codes themselves. So need to reclass the land cover used for the transition calculations from the raw class codes to the positional indices of those class codes. And before doing that, need to reclassified initial and final layers to the IPCC (or custom) classes.

# Processing baseline raster
bl_remap_1 = remap(lc_baseline_esa, np.asarray(esa_to_custom_nesting.get_list()[0]), 
                np.asarray(esa_to_custom_nesting.get_list()[1]))
lc_bl = remap(bl_remap_1, np.asarray(class_codes), np.asarray(class_positions))

saveRaster(lc_bl.astype('int8'), path_lc_bl, cols, rows, projection, "Land_cover_yi")
print("Processed Baseline Raster Saved:", path_lc_bl)

# Processing Target Raster
tg_remap_1 = remap(lc_target_esa, np.asarray(esa_to_custom_nesting.get_list()[0]), 
                np.asarray(esa_to_custom_nesting.get_list()[1]))
lc_tg = remap(tg_remap_1, np.asarray(class_codes), np.asarray(class_positions))

saveRaster(lc_tg.astype('int8'), path_lc_tg, cols, rows, projection, "Land_cover_yf")
print("Processed Target Raster Saved:", path_lc_tg)

# Processing Transition Map 
# Compute transition map (first digit for baseline land cover, and second digit for target year land cover)
lc_tr = (lc_bl * esa_to_custom_nesting.parent.get_multiplier()) + lc_tg
lc_tr_pre_remap = (lc_bl * esa_to_custom_nesting.parent.get_multiplier()) + lc_tg
# print(lc_tr_pre_remap)

# Compute land cover transition
# Remap persistence classes so they are sequential. This makes it easier to assign a clear color ramp in QGIS.
lc_tr_pre_remap = remap(lc_tr, np.asarray(trans_matrix.get_persistence_list()[0]), 
                    np.asarray(trans_matrix.get_persistence_list()[1]))

saveRaster(lc_tr_pre_remap.astype('int8'), path_lc_tr, cols, rows, projection, "Land_cover_transitions_yi-yf")
print("Land Cover Transition Raster Saved:", path_lc_tr)

# Compute land cover degradation
# definition of land cover transitions as degradation (-1), improvement (1), or no relevant change (0)
lc_dg = remap(lc_tr, np.asarray(trans_matrix.get_list()[0]), np.asarray(trans_matrix.get_list()[1]))
        
saveRaster(lc_dg.astype('int8'), path_lc_dg, cols, rows, projection, "Land_cover_degradation")
print("Land Cover Degradation Raster Saved:", path_lc_dg)

# Compute  Mutlibands stack
# Land Cover Degradation + Baseline ESA + Target ESA + Land Cover Transition
dg_raster = rasterio.open(path_lc_dg, "r")
dg = dg_raster.read(1, masked=True)
baseline_esa = rasterio.open(path_lc_baseline_esa, "r").read(1, masked=True)
target_esa = rasterio.open(path_lc_target_esa, "r").read(1, masked=True)
tr = rasterio.open(path_lc_tr, "r").read(1, masked=True)

band_list = [dg, lc_baseline_esa, lc_target_esa, tr]
out_meta = dg_raster.meta.copy()
out_meta.update({"count": 4})

with rasterio.open(path_out_img, 'w', **out_meta) as dest:
    for band_nr, src in enumerate(band_list, start=1):
        dest.write(src, band_nr)

print("Land Cover Multibands Stack Image Saved:", path_out_img)

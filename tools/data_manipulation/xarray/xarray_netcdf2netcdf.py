#!/usr/bin/env python3
#
#  Apply operations on selected variables
# - scale
# one can also select the range of time (for timeseries) to apply these operations over the range only
# when a range of time is selected and when scaling, one can choose to save the entire timeseries or
# the selected range only.
# when scaling, one can add additional filters on dimensions (typically used to filter over latitudes and longitudes)


import argparse
import ast
import warnings
from pathlib import Path

import xarray as xr  # noqa: E402


class netCDF2netCDF ():
    def __init__(self, infile, varname, scale="", output="output.netcdf", write_all=False, filter_list="", verbose=False):
        self.infile = infile
        self.verbose = verbose
        self.varname = varname
        self.write_all = write_all
        self.filter = filter_list
        self.selection = {}
        if scale == "" or scale is None:
            self.scale = 1
        else:
            self.scale = float(scale)
        if output is None:
            self.output="output.netcdf"
        else:
            self.output = output
        # initialization
        self.dset = None
        self.subset = None
        if self.verbose:
            print("infile: ", self.infile)
            print("varname: ", self.varname)
            print("filter_list: ", self.filter)
            print("scale: ", self.scale)
            print("write_all: ", self.write_all)
            print("output: ", self.output)
            
    def dimension_selection(self, single_filter):
        split_filter = single_filter.split('#')
        dimension_varname = split_filter[0]
        op = split_filter[1]
        ll = int(split_filter[2])
        if (op == 'sl'):
            rl = int(split_filter[3])
            self.selection[dimension_varname] = slice(ll, rl)
        elif (op == 'to'):
            self.selection[dimension_varname] = slice(None, ll)
        elif (op == 'from'):
            self.selection[dimension_varname] = slice(ll, None)
        elif (op == 'is'):
            self.selection[dimension_varname] = ll
             
    def filter_selection(self):
        for single_filter in self.filter:
            self.dimension_selection(single_filter)
        if self.write_all:
            self.ds[self.varname] = self.ds[self.varname].isel(self.selection)*self.scale
        else:
            self.dset = self.ds[self.varname].isel(self.selection)*self.scale

    def compute(self):
        if self.dset is None:
            self.ds = xr.open_dataset(self.infile)
            if self.filter:
                self.filter_selection()
                if self.verbose:
                    print(self.selection)
            elif self.write_all is not None:
                self.dset = self.ds[self.varname]

    def save(self):
        if self.write_all:
            self.ds.to_netcdf(self.output)
        else:
            self.dset.to_netcdf(self.output)
                

if __name__ == '__main__':
    warnings.filterwarnings("ignore")
    parser = argparse.ArgumentParser()
    parser.add_argument(
        'input',
        help='input filename in netCDF format'
    )
    parser.add_argument(
        'varname',
        help='Specify which variable to plot (case sensitive)'
    )
    parser.add_argument(
        '--filter',
        nargs="*",
        help='Filter list variable#operator#value_s#value_e'
    )
    parser.add_argument(
        '--output',
        help='Output filename to store the resulting netCDF file'
    )
    parser.add_argument(
        '--scale',
        help='scale factor to apply to selection (float)'
    )
    parser.add_argument(
        "--write_all",
        help="write all data to netCDF",
        action="store_true")
    parser.add_argument(
        "-v", "--verbose",
        help="switch on verbose mode",
        action="store_true")
    args = parser.parse_args()

    dset = netCDF2netCDF(infile=args.input, varname=args.varname, scale=args.scale, output=args.output, filter_list=args.filter, write_all=args.write_all, verbose=args.verbose)
    dset.compute()
    dset.save()

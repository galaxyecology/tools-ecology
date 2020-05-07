# xarray tool for:
# - getting metadata information
# - select data and save results in csv file for further post-processing

import argparse
import os
import shutil
import warnings

import csv
import numpy as np
import xarray as xr


class XarrayTool ():
    def __init__(self, infile, outfile_info="", outfile_summary="",
		select="", outfile="", outputdir="", latname="", latval="",
		lonname="", lonval="", filter_list="", coords="", verbose=False
		):
        self.infile = infile
        self.outfile_info = outfile_info
        self.outfile_summary = outfile_summary
        self.select = select
        self.outfile = outfile
        self.outputdir = outputdir
        self.latname = latname
        self.latval = float(latval)
        self.lonname = lonname
        self.lonval = float(lonval)
        self.filter = filter_list
        self.coords = coords
        self.verbose = verbose
        if self.verbose:
            print("infile: ", self.infile)
            print("outfile_info: ", self.outfile_info)
            print("outfile_summary: ", self.outfile_summary)
            print("outfile: ", self.outfile)
            print("select: ", self.select)
            print("outfile: ", self.outfile)
            print("outputdir: ", self.outputdir)
            print("latname: ", self.latname)
            print("latval: ", self.latval)
            print("lonname: ", self.lonname)
            print("lonval: ", self.lonval)
            print("filter: ", self.filter)
            print("coords: ", self.coords)

    def info(self):
        f = open(self.outfile_info, 'w')
        ds = xr.open_dataset(self.infile)
        ds.info(f)
        f.close()

    def summary(self):
        f = open(self.outfile_summary, 'w')
        ds = xr.open_dataset(self.infile)
        writer = csv.writer(f, delimiter='\t')
        header = ['VariableName', 'NumberOfDimensions']
        for idx, val in enumerate(ds.dims.items()):
                header.append('Dim'+str(idx)+'Name')
                header.append('Dim'+str(idx)+'Size')
        writer.writerow(header)
        for name, da in ds.data_vars.items():
                line = [name]
                line.append(len(ds[name].shape))
                for d,s in zip(da.shape, da.sizes):
                        line.append(s)
                        line.append(d)
                writer.writerow(line)
        for name, da in ds.coords.items():
                line = [name]
                line.append(len(ds[name].shape))
                for d,s in zip(da.shape, da.sizes):
                        line.append(s)
                        line.append(d)
                writer.writerow(line)
        f.close()
     
    def extract_singleloc(self, ds):
        print("single location")
    
    def selection(self):
        print("Select ", self.select)
        ds = xr.open_dataset(self.infile)
        df = ds[self.select].to_dataframe().dropna(how='all').reset_index()
        if self.filter:
           print("Filter") 
        if self.latval and self.lonval:
           print("select lat lon single location")
           self.extract_singleloc(ds)
        elif self.coords != "":
           print("Coord file") 
        if self.coords == "" or self.coords is None:
           df.to_csv(self.outfile, header=True, sep='\t')

if __name__ == '__main__':
        warnings.filterwarnings("ignore")
        parser = argparse.ArgumentParser()

        parser.add_argument(
             'infile',
             help='netCDF input filename'
        )
        parser.add_argument(
             '--info',
             help='Output filename where metadata information is stored'
        )
        parser.add_argument(
            '--summary',
            help='Output filename where data summary information is stored'
        )
        parser.add_argument(
            '--select',
            help='Variable name to select'
        )
        parser.add_argument(
            '--latname',
            help='Latitude name'
        )
        parser.add_argument(
            '--latval',
            help='Latitude value'
        )
        parser.add_argument(
            '--lonname',
            help='Longitude name'
        )
        parser.add_argument(
            '--lonval',
            help='Longitude value'
        )
        parser.add_argument(
            '--coords',
            help='Input file containing Latitude Longitude for geographical selection'
        )
        parser.add_argument(
            '--filter',
            nargs="*",
            help='Filter list variable#operator#value_s#value_e'
        )
        parser.add_argument(
            '--outfile',
            help='csv outfile for storing results of the selection (valid only when --select)'
        )
        parser.add_argument(
            '--outputdir',
            help='folder name for storing results with multiple selections (valid only when --select)'
        )
        parser.add_argument(
            "-v", "--verbose",
            help="switch on verbose mode",
            action="store_true")
        args = parser.parse_args()

        p = XarrayTool(args.infile, args.info, args.summary, args.select, args.outfile,
                       args.outputdir, args.latname, args.latval, args.lonname, args.lonval, 
                       args.filter, args.coords, args.verbose)
        if args.info:
    	        p.info()
        if args.summary:
    	        p.summary()
        if args.select:
                p.selection()


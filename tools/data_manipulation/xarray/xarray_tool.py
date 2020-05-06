# xarray tool for:
# - getting metadata information
# - select data and save results in csv file for further post-processing

import argparse
import os
import shutil
import warnings

import csv
import xarray as xr


class XarrayTool ():
    def __init__(self, infile, outfile_info, outfile_summary,
                 verbose=False
                 ):
        self.infile = infile
        self.outfile_info = outfile_info
        self.outfile_summary = outfile_summary

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


if __name__ == '__main__':
        warnings.filterwarnings("ignore")
        parser = argparse.ArgumentParser()

        print("Start program")
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
            "-v", "--verbose",
            help="switch on verbose mode",
            action="store_true")
        args = parser.parse_args()

        print("infile = ", args.infile)
        print("info = ", args.info)
        print("summary = ", args.summary)
        p = XarrayTool(args.infile, args.info, args.summary, args.verbose)
        if args.info != "":
    	        p.info()
        if args.summary != "":
    	        p.summary()


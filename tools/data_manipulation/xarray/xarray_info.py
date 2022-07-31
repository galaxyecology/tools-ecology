# xarray tool for:
# - getting metadata information
# - select data and save results in csv file for further post-processing

import argparse
import csv
import os
import warnings

import xarray as xr


class XarrayInfo ():
    def __init__(self, infile, outfile_info="", outfile_summary="",
                 verbose=False, coords_info=None):
        self.infile = infile
        self.outfile_info = outfile_info
        self.outfile_summary = outfile_summary
        self.coords_info = coords_info
        self.verbose = verbose
        # initialization
        self.dset = None
        self.gset = None
        if self.verbose:
            print("infile: ", self.infile)
            print("outfile_info: ", self.outfile_info)
            print("outfile_summary: ", self.outfile_summary)
            print("coords_info: ", self.coords_info)

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
            header.append('Dim' + str(idx) + 'Name')
            header.append('Dim' + str(idx) + 'Size')
        writer.writerow(header)
        for name, da in ds.data_vars.items():
            line = [name]
            line.append(len(ds[name].shape))
            for d, s in zip(da.shape, da.sizes):
                line.append(s)
                line.append(d)
            writer.writerow(line)
        for name, da in ds.coords.items():
            line = [name]
            line.append(len(ds[name].shape))
            for d, s in zip(da.shape, da.sizes):
                line.append(s)
                line.append(d)
            writer.writerow(line)
        f.close()

    def get_coords_info(self):
        ds = xr.open_dataset(self.infile)
        for c in ds.coords:
            filename = os.path.join(self.coords_info,
                                    c.strip() +
                                    '.tabular')
            pd = ds.coords[c].to_pandas()
            pd.index = range(len(pd))
            pd.to_csv(filename, header=False, sep='\t')


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
        '--coords_info',
        help='output-folder where for each coordinate, coordinate values '
             ' are being printed in the corresponding outputfile'
    )
    parser.add_argument(
        "-v", "--verbose",
        help="switch on verbose mode",
        action="store_true"
    )
    args = parser.parse_args()

    p = XarrayInfo(args.infile, args.info, args.summary,
                   args.verbose, args.coords_info)
    if args.info:
        p.info()
    elif args.coords_info:
        p.get_coords_info()
    if args.summary:
        p.summary()

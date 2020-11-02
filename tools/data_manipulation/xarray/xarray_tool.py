# xarray tool for:
# - getting metadata information
# - select data and save results in csv file for further post-processing

import argparse
import csv
import warnings

import geopandas as gdp

import pandas as pd

from shapely.geometry import Point
from shapely.ops import nearest_points

import xarray as xr


class XarrayTool ():
    def __init__(self, infile, outfile_info="", outfile_summary="",
                 select="", outfile="", outputdir="", latname="",
                 latvalN="", latvalS="", lonname="", lonvalE="",
                 lonvalW="", filter_list="", coords="", time="",
                 verbose=False
                 ):
        self.infile = infile
        self.outfile_info = outfile_info
        self.outfile_summary = outfile_summary
        self.select = select
        self.outfile = outfile
        self.outputdir = outputdir
        self.latname = latname
        if latvalN != "" and latvalN is not None:
            self.latvalN = float(latvalN)
        else:
            self.latvalN = ""
        if latvalS != "" and latvalS is not None:
            self.latvalS = float(latvalS)
        else:
            self.latvalS = ""
        self.lonname = lonname
        if lonvalE != "" and lonvalE is not None:
            self.lonvalE = float(lonvalE)
        else:
            self.lonvalE = ""
        if lonvalW != "" and lonvalW is not None:
            self.lonvalW = float(lonvalW)
        else:
            self.lonvalW = ""
        self.filter = filter_list
        self.time = time
        self.coords = coords
        self.verbose = verbose
        # initialization
        self.dset = None
        self.gset = None
        if self.verbose:
            print("infile: ", self.infile)
            print("outfile_info: ", self.outfile_info)
            print("outfile_summary: ", self.outfile_summary)
            print("outfile: ", self.outfile)
            print("select: ", self.select)
            print("outfile: ", self.outfile)
            print("outputdir: ", self.outputdir)
            print("latname: ", self.latname)
            print("latvalN: ", self.latvalN)
            print("latvalS: ", self.latvalS)
            print("lonname: ", self.lonname)
            print("lonvalE: ", self.lonvalE)
            print("lonvalW: ", self.lonvalW)
            print("filter: ", self.filter)
            print("time: ", self.time)
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

    def rowfilter(self, single_filter):
        split_filter = single_filter.split('#')
        filter_varname = split_filter[0]
        op = split_filter[1]
        ll = float(split_filter[2])
        if (op == 'bi'):
            rl = float(split_filter[3])
        if filter_varname == self.select:
            # filter on values of the selected variable
            if op == 'bi':
                self.dset = self.dset.where((self.dset <= rl) & (self.dset >= ll))
            elif op == 'le':
                self.dset = self.dset.where(self.dset <= ll)
            elif op == 'ge':
                self.dset = self.dset.where(self.dset >= ll)
            elif op == 'e':
                self.dset = self.dset.where(self.dset == ll)
        else:  # filter on other dimensions of the selected variable
            if op == 'bi':
                self.dset = self.dset.sel({filter_varname: slice(ll, rl)})
            elif op == 'le':
                self.dset = self.dset.sel({filter_varname: slice(None, ll)})
            elif op == 'ge':
                self.dset = self.dset.sel({filter_varname: slice(ll, None)})
            elif op == 'e':
                self.dset = self.dset.sel({filter_varname: ll},
                                          method='nearest')

    def selection(self):
        if self.dset is None:
            self.ds = xr.open_dataset(self.infile)
            self.dset = self.ds[self.select]  # select variable
            if self.time:
                self.datetime_selection()
            if self.filter:
                self.filter_selection()

        self.area_selection()
        # convert to dataframe
        self.gset = self.gset.to_dataframe().dropna(how='all').reset_index()
        self.gset.to_csv(self.outfile, header=True, sep='\t')

    def datetime_selection(self):
        split_filter = self.time.split('#')
        time_varname = split_filter[0]
        op = split_filter[1]
        ll = split_filter[2]
        if (op == 'sl'):
            rl = split_filter[3]
            self.dset = self.dset.sel({time_varname: slice(ll, rl)})
        elif (op == 'to'):
            self.dset = self.dset.sel({time_varname: slice(None, ll)})
        elif (op == 'from'):
            self.dset = self.dset.sel({time_varname: slice(ll, None)})
        elif (op == 'is'):
            self.dset = self.dset.sel({time_varname: ll}, method='nearest')

    def filter_selection(self):
        for single_filter in self.filter:
            self.rowfilter(single_filter)

    def area_selection(self):
        if self.latvalS != "" and self.lonvalW != "":
            # Select geographical area
            self.gset = self.dset.sel({self.latname:
                                       slice(self.latvalS, self.latvalN),
                                       self.lonname:
                                       slice(self.lonvalW, self.lonvalE)})
        elif self.latvalN != "" and self.lonvalE != "":
            # select nearest location
            self.nearest_location()  # find nearest location without NaN values
            self.gset = self.dset.sel({self.latname: self.nearest_latvalN,
                                       self.lonname: self.nearest_lonvalE},
                                      method='nearest')
        else:
            self.gset = self.dset

    def nearest_location(self):
        # Build a geopandas dataframe with all first elements in each dimension
        # so we assume null values correspond to a mask that is the same for
        # all dimensions in the dataset.
        dsel_frame = self.dset
        for dim in self.dset.dims:
            if dim != self.latname and dim != self.lonname:
                dsel_frame = dsel_frame.isel({dim: 0})
        # transform to pandas dataframe
        dff = dsel_frame.to_dataframe().dropna().reset_index()
        # transform to geopandas to collocate
        gdf = gdp.GeoDataFrame(dff,
                               geometry=gdp.points_from_xy(dff[self.lonname],
                                                           dff[self.latname]))
        # Find nearest location where values are not null
        point = Point(self.lonvalE, self.latvalN)
        multipoint = gdf.geometry.unary_union
        queried_geom, nearest_geom = nearest_points(point, multipoint)
        self.nearest_latvalN = nearest_geom.y
        self.nearest_lonvalE = nearest_geom.x

    def selection_from_coords(self):
        fcoords = pd.read_csv(self.coords, sep='\t')
        for row in fcoords.itertuples():
            self.latvalN = row[0]
            self.lonvalE = row[1]
            self.outfile = (self.outputdir + '/' + self.select + '_' + str(row.Index) + '.tabular')
            self.selection()


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
        '--latvalN',
        help='North latitude value'
    )
    parser.add_argument(
        '--latvalS',
        help='South latitude value'
    )
    parser.add_argument(
        '--lonname',
        help='Longitude name'
    )
    parser.add_argument(
        '--lonvalE',
        help='East longitude value'
    )
    parser.add_argument(
        '--lonvalW',
        help='West longitude value'
    )
    parser.add_argument(
        '--coords',
        help='Input file containing Latitude and Longitude'
             'for geographical selection'
    )
    parser.add_argument(
        '--filter',
        nargs="*",
        help='Filter list variable#operator#value_s#value_e'
    )
    parser.add_argument(
        '--time',
        help='select timeseries variable#operator#value_s[#value_e]'
    )
    parser.add_argument(
        '--outfile',
        help='csv outfile for storing results of the selection'
             '(valid only when --select)'
    )
    parser.add_argument(
        '--outputdir',
        help='folder name for storing results with multiple selections'
             '(valid only when --select)'
    )
    parser.add_argument(
        "-v", "--verbose",
        help="switch on verbose mode",
        action="store_true"
    )
    args = parser.parse_args()

    p = XarrayTool(args.infile, args.info, args.summary, args.select,
                   args.outfile, args.outputdir, args.latname,
                   args.latvalN, args.latvalS, args.lonname,
                   args.lonvalE, args.lonvalW, args.filter,
                   args.coords, args.time, args.verbose)
    if args.info:
        p.info()
    if args.summary:
        p.summary()
    if args.coords:
        p.selection_from_coords()
    elif args.select:
        p.selection()

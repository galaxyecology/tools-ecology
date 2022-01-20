#!/usr/bin/env python3
#
#
# usage: xarray_mapplot.py [-h] [--proj PROJ]
#                               [--cmap CMAP]
#                               [--output OUTPUT]
#                               [--time TIMES]
#                               [--nrow NROW]
#                               [--ncol NCOL]
#                               [--title title]
#                               [--latitude LATITUDE]
#                               [--longitude LONGITUDE]
#                               [--land ALPHA-LAND]
#                               [--ocean ALPHA-OCEAN]
#                               [--coastline ALPHA-COASTLINE]
#                               [--borders ALPHA-BORDERS]
#                               [--xlim "x1,x2"]
#                               [--ylim "y1,y2"]
#                               [--range "valmin,valmax"]
#                               [--threshold VAL]
#                               [--label label-colorbar]
#                               [--config config-file]
#                               [--shift]
#                               [-v]
#                               input varname
#
# positional arguments:
#  input            input filename with geographical coordinates (netCDF
#                   format)
#  varname          Specify which variable to plot (case sensitive)
#
# optional arguments:
#  -h, --help       show this help message and exit
#  --proj PROJ      Specify the projection on which we draw
#  --cmap CMAP      Specify which colormap to use for plotting
#  --output OUTPUT  output filename to store resulting image (png format)
#  --time TIMES     time index from the file for multiple plots ("0 1 2 3")
#  --title          plot or subplot title
#  --latitude        variable name for latitude
#  --longitude       variable name for longitude
#  --land            add land on plot with alpha value [0-1]
#  --ocean           add oceans on plot with alpha value [0-1]
#  --coastline       add coastline with alpha value [0-1]
#  --borders         add country borders with alpha value [0-1]
#  --xlim            limited geographical area longitudes "x1,x2"
#  --ylim            limited geographical area latitudes "y1,y2"
#  --range           "valmin,valmax" for plotting
#  --threshold       do not plot values below threshold
#  --label           set a label for colormap
#  --config          plotting parameters are passed via a config file
#                    (overwrite other plotting options)
#  --shift           shift longitudes if specified
#  -v, --verbose    switch on verbose mode
#

import argparse
import ast
import warnings
from pathlib import Path

import cartopy.crs as ccrs
import cartopy.feature as feature

from cmcrameri import cm

import matplotlib as mpl
mpl.use('Agg')
from matplotlib import pyplot  # noqa: I202,E402

import xarray as xr  # noqa: E402


class MapPlotXr ():
    def __init__(self, input, varname, output, verbose=False,
                 config_file="", proj="", shift=False):

        li = list(input.split(","))
        if len(li) > 1:
            self.input = li
        else:
            self.input = input

        if proj != "" and proj is not None and Path(proj).exists():
            f = open(proj)
            sdict = ''.join(
                f.read().replace("\n", "").split('{')[1].split('}')[0]
                )
            self.proj = '{' + sdict.strip() + '}'
        else:
            self.proj = None
        self.varname = varname
        self.shift = shift
        self.xylim_supported = False
        self.colorbar = True
        if output is None:
            if type(self.input) is list:
                self.output = Path(self.input[0]).stem + '.png'
            else:
                self.output = Path(self.input).stem + '.png'
        else:
            self.output = output
        self.verbose = verbose
        self.label = {}
        self.time = []
        self.xlim = []
        self.ylim = []
        self.range = []
        self.latitude = "latitude"
        self.longitude = "longitude"
        self.land = 0
        self.ocean = 0
        self.coastline = 0
        self.borders = 0
        self.cmap = "coolwarm"
        self.threshold = ""
        self.title = ""

        if config_file != "" and config_file is not None:
            with open(config_file) as f:
                sdict = ''.join(
                    f.read().replace("\n", "").split('{')[1].split('}')[0]
                    )
                tmp = ast.literal_eval('{' + sdict.strip() + '}')
                for key in tmp:
                    if key == 'time':
                        time = tmp[key]
                        self.time = list(map(int, time.split(",")))
                    if key == 'cmap':
                        self.get_cmap(tmp[key])
                    if key == 'latitude':
                        self.latitude = tmp[key]
                    if key == 'longitude':
                        self.longitude = tmp[key]
                    if key == 'land':
                        self.land = float(tmp[key])
                    if key == 'ocean':
                        self.ocean = float(tmp[key])
                    if key == 'coastline':
                        self.coastline = float(tmp[key])
                    if key == 'borders':
                        self.borders = float(tmp[key])
                    if key == 'xlim':
                        xlim = tmp[key]
                        self.xlim = list(map(float, xlim.split(",")))
                    if key == 'ylim':
                        ylim = tmp[key]
                        self.ylim = list(map(float, ylim.split(",")))
                    if key == 'range':
                        range_values = tmp[key]
                        self.range = list(map(float, range_values.split(",")))
                    if key == 'threshold':
                        self.threshold = float(tmp[key])
                    if key == 'label':
                        self.label['label'] = tmp[key]
                    if key == 'title':
                        self.title = tmp[key]

        if type(self.input) is list:
            self.dset = xr.open_mfdataset(self.input, use_cftime=True)
        else:
            self.dset = xr.open_dataset(self.input, use_cftime=True)

        if verbose:
            print("input: ", self.input)
            print("proj: ", self.proj)
            print("varname: ", self.varname)
            print("time: ", self.time)
            print("minval, maxval: ", self.range)
            print("title: ", self.title)
            print("output: ", self.output)
            print("label: ", self.label)
            print("shift: ", self.shift)
            print("ocean: ", self.ocean)
            print("land: ", self.land)
            print("coastline: ", self.coastline)
            print("borders: ", self.borders)
            print("latitude: ", self.latitude)
            print("longitude: ", self.longitude)
            print("xlim: ", self.xlim)
            print("ylim: ", self.ylim)

    def get_cmap(self, cmap):
        if cmap[0:3] == 'cm.':
            self.cmap = cm.__dict__[cmap[3:]]
        else:
            self.cmap = cmap

    def projection(self):
        if self.proj is None:
            return ccrs.PlateCarree()

        proj_dict = ast.literal_eval(self.proj)
        user_proj = proj_dict.pop("proj")
        if user_proj == 'PlateCarree':
            self.xylim_supported = True
            return ccrs.PlateCarree(**proj_dict)
        elif user_proj == 'AlbersEqualArea':
            return ccrs.AlbersEqualArea(**proj_dict)
        elif user_proj == 'AzimuthalEquidistant':
            return ccrs.AzimuthalEquidistant(**proj_dict)
        elif user_proj == 'EquidistantConic':
            return ccrs.EquidistantConic(**proj_dict)
        elif user_proj == 'LambertConformal':
            return ccrs.LambertConformal(**proj_dict)
        elif user_proj == 'LambertCylindrical':
            return ccrs.LambertCylindrical(**proj_dict)
        elif user_proj == 'Mercator':
            return ccrs.Mercator(**proj_dict)
        elif user_proj == 'Miller':
            return ccrs.Miller(**proj_dict)
        elif user_proj == 'Mollweide':
            return ccrs.Mollweide(**proj_dict)
        elif user_proj == 'Orthographic':
            return ccrs.Orthographic(**proj_dict)
        elif user_proj == 'Robinson':
            return ccrs.Robinson(**proj_dict)
        elif user_proj == 'Sinusoidal':
            return ccrs.Sinusoidal(**proj_dict)
        elif user_proj == 'Stereographic':
            return ccrs.Stereographic(**proj_dict)
        elif user_proj == 'TransverseMercator':
            return ccrs.TransverseMercator(**proj_dict)
        elif user_proj == 'UTM':
            return ccrs.UTM(**proj_dict)
        elif user_proj == 'InterruptedGoodeHomolosine':
            return ccrs.InterruptedGoodeHomolosine(**proj_dict)
        elif user_proj == 'RotatedPole':
            return ccrs.RotatedPole(**proj_dict)
        elif user_proj == 'OSGB':
            self.xylim_supported = False
            return ccrs.OSGB(**proj_dict)
        elif user_proj == 'EuroPP':
            self.xylim_supported = False
            return ccrs.EuroPP(**proj_dict)
        elif user_proj == 'Geostationary':
            return ccrs.Geostationary(**proj_dict)
        elif user_proj == 'NearsidePerspective':
            return ccrs.NearsidePerspective(**proj_dict)
        elif user_proj == 'EckertI':
            return ccrs.EckertI(**proj_dict)
        elif user_proj == 'EckertII':
            return ccrs.EckertII(**proj_dict)
        elif user_proj == 'EckertIII':
            return ccrs.EckertIII(**proj_dict)
        elif user_proj == 'EckertIV':
            return ccrs.EckertIV(**proj_dict)
        elif user_proj == 'EckertV':
            return ccrs.EckertV(**proj_dict)
        elif user_proj == 'EckertVI':
            return ccrs.EckertVI(**proj_dict)
        elif user_proj == 'EqualEarth':
            return ccrs.EqualEarth(**proj_dict)
        elif user_proj == 'Gnomonic':
            return ccrs.Gnomonic(**proj_dict)
        elif user_proj == 'LambertAzimuthalEqualArea':
            return ccrs.LambertAzimuthalEqualArea(**proj_dict)
        elif user_proj == 'NorthPolarStereo':
            return ccrs.NorthPolarStereo(**proj_dict)
        elif user_proj == 'OSNI':
            return ccrs.OSNI(**proj_dict)
        elif user_proj == 'SouthPolarStereo':
            return ccrs.SouthPolarStereo(**proj_dict)

    def plot(self, ts=None):
        if self.shift:
            if self.longitude == 'longitude':
                self.dset = self.dset.assign_coords(
                                 longitude=(((
                                        self.dset[self.longitude]
                                        + 180) % 360) - 180))
            elif self.longitude == 'lon':
                self.dset = self.dset.assign_coords(
                                 lon=(((self.dset[self.longitude]
                                        + 180) % 360) - 180))

        pyplot.figure(1, figsize=[20, 10])

        # Set the projection to use for plotting
        ax = pyplot.subplot(1, 1, 1, projection=self.projection())
        if self.land:
            ax.add_feature(feature.LAND, alpha=self.land)

        if self.ocean:
            ax.add_feature(feature.OCEAN, alpha=self.ocean)
        if self.coastline:
            ax.coastlines(resolution='10m', alpha=self.coastline)
        if self.borders:
            ax.add_feature(feature.BORDERS, linestyle=':', alpha=self.borders)

        if self.xlim:
            min_lon = min(self.xlim[0], self.xlim[1])
            max_lon = max(self.xlim[0], self.xlim[1])
        else:
            min_lon = self.dset[self.longitude].min()
            max_lon = self.dset[self.longitude].max()

        if self.ylim:
            min_lat = min(self.ylim[0], self.ylim[1])
            max_lat = max(self.ylim[0], self.ylim[1])
        else:
            min_lat = self.dset[self.latitude].min()
            max_lat = self.dset[self.latitude].max()

        if self.xylim_supported:
            pyplot.xlim(min_lon, max_lon)
            pyplot.ylim(min_lat, max_lat)

        # Fix extent
        if self.threshold == "" or self.threshold is None:
            threshold = self.dset[self.varname].min()
        else:
            threshold = float(self.threshold)

        if self.range == []:
            minval = self.dset[self.varname].min()
            maxval = self.dset[self.varname].max()
        else:
            minval = self.range[0]
            maxval = self.range[1]

        if self.verbose:
            print("minval: ", minval)
            print("maxval: ", maxval)

        # pass extent with vmin and vmax parameters
        proj_t = ccrs.PlateCarree()
        if ts is None:
            self.dset.where(
                 self.dset[self.varname] > threshold
                 )[self.varname].plot(ax=ax,
                                      vmin=minval,
                                      vmax=maxval,
                                      transform=proj_t,
                                      cmap=self.cmap,
                                      cbar_kwargs=self.label
                                      )
            if self.title != "" and self.title is not None:
                pyplot.title(self.title)
            pyplot.savefig(self.output)
        else:
            if self.colorbar:
                self.dset.where(
                     self.dset[self.varname] > threshold
                     )[self.varname].isel(time=ts).plot(ax=ax,
                                                        vmin=minval,
                                                        vmax=maxval,
                                                        transform=proj_t,
                                                        cmap=self.cmap,
                                                        cbar_kwargs=self.label
                                                        )
            else:
                self.dset.where(
                     self.dset[self.varname] > minval
                     )[self.varname].isel(time=ts).plot(ax=ax,
                                                        vmin=minval,
                                                        vmax=maxval,
                                                        transform=proj_t,
                                                        cmap=self.cmap,
                                                        add_colorbar=False)
            if self.title != "" and self.title is not None:
                pyplot.title(self.title + "(time = " + str(ts) + ')')
            pyplot.savefig(self.output[:-4] + "_time" + str(ts) +
                           self.output[-4:])  # assume png format


if __name__ == '__main__':
    warnings.filterwarnings("ignore")
    parser = argparse.ArgumentParser()
    parser.add_argument(
        'input',
        help='input filename with geographical coordinates (netCDF format)'
    )
    parser.add_argument(
        '--proj',
        help='Config file with the projection on which we draw'
    )
    parser.add_argument(
        'varname',
        help='Specify which variable to plot (case sensitive)'
    )
    parser.add_argument(
        '--output',
        help='output filename to store resulting image (png format)'
    )
    parser.add_argument(
        '--config',
        help='pass plotting parameters via a config file'
    )
    parser.add_argument(
        '--shift',
        help='shift longitudes if specified',
        action="store_true"
    )
    parser.add_argument(
        "-v", "--verbose",
        help="switch on verbose mode",
        action="store_true")
    args = parser.parse_args()

    dset = MapPlotXr(input=args.input, varname=args.varname,
                     output=args.output, verbose=args.verbose,
                     config_file=args.config, proj=args.proj,
                     shift=args.shift)

    if dset.time == []:
        dset.plot()
    else:
        for t in dset.time:
            dset.plot(t)
            dset.shift = False   # only shift once
            dset.colorbar = True

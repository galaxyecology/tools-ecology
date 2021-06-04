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
    def __init__(self, input, proj, varname, cmap, output, verbose=False,
                 time=[], title="", latitude="latitude",
                 longitude="longitude", land=0, ocean=0,
                 coastline=0, borders=0, xlim=[], ylim=[],
                 threshold="", label="", shift=False,
                 range_values=[]):
        self.input = input
        self.proj = proj
        self.varname = varname
        self.cmap = cmap
        self.time = time
        self.latitude = latitude
        self.longitude = longitude
        self.land = land
        self.ocean = ocean
        self.coastline = coastline
        self.borders = borders
        self.xlim = xlim
        self.ylim = ylim
        self.range = range_values
        self.threshold = threshold
        self.shift = shift
        self.xylim_supported = False
        self.colorbar = True
        self.title = title
        if output is None:
            self.output = Path(input).stem + '.png'
        else:
            self.output = output
        self.verbose = verbose
        self.dset = xr.open_dataset(self.input, use_cftime=True)

        if label == "" or label is None:
            self.label = self.dset[self.varname].long_name + \
                            ' [' + \
                            self.dset[self.varname].units + ']'
        else:
            self.label = label
        if verbose:
            print("input: ", self.input)
            print("proj: ", self.proj)
            print("varname: ", self.varname)
            print("cmap: ", self.cmap)
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

    def get_cmap(self):
        if self.cmap == "batlow":
            self.cmap = cm.batlow
        if self.cmap == "batlowW":
            self.cmap = cm.batlowW
        if self.cmap == "batlowK":
            self.cmap = cm.batlowK
        if self.cmap == "devon":
            self.cmap = cm.devon
        if self.cmap == "davos":
            self.cmap = cm.davos
        if self.cmap == "oslo":
            self.cmap = cm.oslo
        if self.cmap == "lapaz":
            self.cmap = cm.lapaz
        if self.cmap == "acton":
            self.cmap = cm.acton
        if self.cmap == "lajolla":
            self.cmap = cm.lajolla
        if self.cmap == "bilbao":
            self.cmap = cm.bilbao
        if self.cmap == "grayC":
            self.cmap = cm.grayC
        if self.cmap == "tokyo":
            self.cmap = cm.tokyo
        if self.cmap == "turku":
            self.cmap = cm.turku
        if self.cmap == "bamako":
            self.cmap = cm.bamako
        if self.cmap == "nuuk":
            self.cmap = cm.nuuk
        if self.cmap == "hawaii":
            self.cmap = cm.hawaii
        if self.cmap == "buda":
            self.cmap = cm.buda
        if self.cmap == "imola":
            self.cmap = cm.imola
        if self.cmap == "broc":
            self.cmap = cm.broc
        if self.cmap == "lisbon":
            self.cmap = cm.lisbon
        if self.cmap == "roma":
            self.cmap = cm.roma
        if self.cmap == "cork":
            self.cmap = cm.cork
        if self.cmap == "tofino":
            self.cmap = cm.tofino
        if self.cmap == "bam":
            self.cmap = cm.bam
        if self.cmap == "vik":
            self.cmap = cm.vik
        if self.cmap == "berlin":
            self.cmap = cm.berlin
        if self.cmap == "vanimo":
            self.cmap = cm.vanimo
        if self.cmap == "oleron":
            self.cmap = cm.oleron
        if self.cmap == "bukavu":
            self.cmap = cm.bukavu
        if self.cmap == "fes":
            self.cmap = cm.fes
        if self.cmap == "romaO":
            self.cmap = cm.romaO
        if self.cmap == "bamO":
            self.cmap = cm.bamO
        if self.cmap == "brocO":
            self.cmap = cm.brocO
        if self.cmap == "corkO":
            self.cmap = cm.corkO
        if self.cmap == "vikO":
            self.cmap = cm.vikO
        if self.cmap == "batlow_r":
            self.cmap = cm.batlow_r
        if self.cmap == "batlowW_r":
            self.cmap = cm.batlowW_r
        if self.cmap == "batlowK_r":
            self.cmap = cm.batlowK_r
        if self.cmap == "devon_r":
            self.cmap = cm.devon_r
        if self.cmap == "davos_r":
            self.cmap = cm.davos_r
        if self.cmap == "oslo_r":
            self.cmap = cm.oslo_r
        if self.cmap == "lapaz_r":
            self.cmap = cm.lapaz_r
        if self.cmap == "acton_r":
            self.cmap = cm.acton_r
        if self.cmap == "lajolla_r":
            self.cmap = cm.lajolla_r
        if self.cmap == "bilbao_r":
            self.cmap = cm.bilbao_r
        if self.cmap == "grayC_r":
            self.cmap = cm.grayC_r
        if self.cmap == "tokyo_r":
            self.cmap = cm.tokyo_r
        if self.cmap == "turku_r":
            self.cmap = cm.turku_r
        if self.cmap == "bamako_r":
            self.cmap = cm.bamako_r
        if self.cmap == "nuuk_r":
            self.cmap = cm.nuuk_r
        if self.cmap == "hawaii_r":
            self.cmap = cm.hawaii_r
        if self.cmap == "buda_r":
            self.cmap = cm.buda_r
        if self.cmap == "imola_r":
            self.cmap = cm.imola_r
        if self.cmap == "broc_r":
            self.cmap = cm.broc_r
        if self.cmap == "lisbon_r":
            self.cmap = cm.lisbon_r
        if self.cmap == "roma_r":
            self.cmap = cm.roma_r
        if self.cmap == "cork_r":
            self.cmap = cm.cork_r
        if self.cmap == "tofino_r":
            self.cmap = cm.tofino_r
        if self.cmap == "bam_r":
            self.cmap = cm.bam_r
        if self.cmap == "vik_r":
            self.cmap = cm.vik_r
        if self.cmap == "berlin_r":
            self.cmap = cm.berlin_r
        if self.cmap == "vanimo_r":
            self.cmap = cm.vanimo_r
        if self.cmap == "oleron_r":
            self.cmap = cm.oleron_r
        if self.cmap == "bukavu_r":
            self.cmap = cm.bukavu_r
        if self.cmap == "fes_r":
            self.cmap = cm.fes_r
        if self.cmap == "romaO_r":
            self.cmap = cm.romaO_r
        if self.cmap == "bamO_r":
            self.cmap = cm.bamO_r
        if self.cmap == "brocO_r":
            self.cmap = cm.brocO_r
        if self.cmap == "corkO_r":
            self.cmap = cm.corkO_r
        if self.cmap == "vikO_r":
            self.cmap = cm.vikO_r

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
                                      cbar_kwargs={
                                         'label': self.label
                                      })
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
                                                        cbar_kwargs={
                                                          'label': self.label
                                                        })
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
        help='Specify the projection on which we draw'
    )
    parser.add_argument(
        'varname',
        help='Specify which variable to plot (case sensitive)'
    )
    parser.add_argument(
        '--cmap',
        help='Specify which colormap to use for plotting'
    )
    parser.add_argument(
        '--output',
        help='output filename to store resulting image (png format)'
    )
    parser.add_argument(
        '--time',
        help='list of times to plot for multiple plots'
    )
    parser.add_argument(
        '--title',
        help='plot title'
    )
    parser.add_argument(
        '--latitude',
        help='variable name for latitude'
    )
    parser.add_argument(
        '--longitude',
        help='variable name for longitude'
    )
    parser.add_argument(
        '--land',
        help='add land on plot with alpha value [0-1]'
    )
    parser.add_argument(
        '--ocean',
        help='add oceans on plot with alpha value [0-1]'
    )
    parser.add_argument(
        '--coastline',
        help='add coastline with alpha value [0-1]'
    )
    parser.add_argument(
        '--borders',
        help='add country borders with alpha value [0-1]'
    )
    parser.add_argument(
        '--xlim',
        help='limited geographical area longitudes "x1,x2"'
    )
    parser.add_argument(
        '--ylim',
        help='limited geographical area latitudes "y1,y2"'
    )
    parser.add_argument(
        '--range',
        help='min and max values for plotting "minval,maxval"'
    )
    parser.add_argument(
        '--threshold',
        help='do not plot values below threshold'
    )
    parser.add_argument(
        '--label',
        help='set a label for colorbar'
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

    if args.time is None:
        time = []
    else:
        time = list(map(int, args.time.split(",")))
    if args.xlim is None:
        xlim = []
    else:
        xlim = list(map(float, args.xlim.split(",")))
    if args.ylim is None:
        ylim = []
    else:
        ylim = list(map(float, args.ylim.split(",")))
    if args.range is None:
        range_values = []
    else:
        range_values = list(map(float, args.range.split(",")))
    if args.latitude is None:
        latitude = "latitude"
    else:
        latitude = args.latitude
    if args.longitude is None:
        longitude = "longitude"
    else:
        longitude = args.longitude
    if args.land is None:
        land = 0
    else:
        land = float(args.land)
    if args.ocean is None:
        ocean = 0
    else:
        ocean = float(args.ocean)
    if args.coastline is None:
        coastline = 0
    else:
        coastline = float(args.coastline)
    if args.borders is None:
        borders = 0
    else:
        borders = float(args.borders)

    dset = MapPlotXr(input=args.input, proj=args.proj, varname=args.varname,
                     cmap=args.cmap, output=args.output, verbose=args.verbose,
                     time=time, title=args.title,
                     latitude=latitude, longitude=longitude, land=land,
                     ocean=ocean, coastline=coastline, borders=borders,
                     xlim=xlim, ylim=ylim, threshold=args.threshold,
                     label=args.label, shift=args.shift,
                     range_values=range_values)

    if dset.time == []:
        dset.plot()
    else:
        for t in dset.time:
            dset.plot(t)
            dset.shift = False   # only shift once
            dset.colorbar = False

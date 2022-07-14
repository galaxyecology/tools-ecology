#!/usr/bin/env python3
#
#
# usage:  netCDF_timeseries.py [-h] [--output output.png]
#                               [--save timeseries.tabular]
#                               [--config config-file]
#                               [-v]
#                               input varname
# positional arguments:
#  input            input filename with geographical coordinates (netCDF
#                   format)
#  varname          Specify which variable to extract (case sensitive)
#
# optional arguments:
#  -h, --help                 show this help message and exit
#  --output output.png        filename to store image (png format)
#  --save timeseries.tabular  filename to store timeseries (tabular format)
#  --config                   config file extract parameters
#  -v, --verbose              switch on verbose mode
#
import argparse
import ast
import warnings

import matplotlib as mpl
mpl.use('Agg')

import matplotlib.pyplot as plt   # noqa: I202,E402
from matplotlib.dates import DateFormatter   # noqa: I202,E402

import xarray as xr  # noqa: I202,E402


class TimeSeries ():
    def __init__(self, input, varname, output, save, verbose=False,
                 config_file=""):

        li = list(input.split(","))
        if len(li) > 1:
            self.input = li
        else:
            self.input = input

        self.varname = varname
        self.xylim_supported = True
        if output == "" or output is None:
            self.output = "Timeseries.png"
        else:
            self.output = output
        if save == "" or save is None:
            self.save = "Timeseries.tabular"
        else:
            self.save = save
        self.verbose = verbose
        self.time_start_value = ""
        self.time_end_value = ""
        self.lon_value = ""
        self.lat_value = ""
        self.lat_name = 'lat'
        self.lon_name = 'lon'
        self.time_name = 'time'
        self.title = ''
        self.xlabel = ''
        self.ylabel = ''
        self.format_date = ''
        if config_file != "" and config_file is not None:
            with open(config_file) as f:
                sdict = ''.join(
                    f.read().replace("\n", "").split('{')[1].split('}')[0]
                    )
                tmp = ast.literal_eval('{' + sdict.strip() + '}')
                for key in tmp:
                    if key == 'time_start_value':
                        self.time_start_value = tmp[key]
                    if key == 'time_end_value':
                        self.time_end_value = tmp[key]
                    if key == 'lon_value':
                        self.lon_value = tmp[key]
                    if key == 'lat_value':
                        self.lat_value = tmp[key]
                    if key == 'lon_name':
                        self.lon_name = tmp[key]
                    if key == 'lat_name':
                        self.lat_name = tmp[key]
                    if key == 'time_name':
                        self.time_name = tmp[key]
                    if key == 'title':
                        self.title = tmp[key]
                    if key == 'xlabel':
                        self.xlabel = tmp[key]
                    if key == 'ylabel':
                        self.ylabel = tmp[key]
                    if key == 'format_date':
                        self.format_date = tmp[key]
                        self.format_date = self.format_date.replace('X', '%')

        if type(self.input) is list:
            self.dset = xr.open_mfdataset(self.input, use_cftime=True)
        else:
            self.dset = xr.open_dataset(self.input, use_cftime=True)

        if verbose:
            print("input: ", self.input)
            print("varname: ", self.varname)
            if self.time_start_value:
                print("time_start_value: ", self.time_start_value)
            if self.time_end_value:
                print("time_end_value: ", self.time_end_value)
            print("output: ", self.output)
            if self.lon_value:
                print(self.lon_name, self.lon_value)
            if self.lat_value:
                print(self.lat_name, self.lat_value)

    def plot(self):
        if self.lon_value:
            lon_c = float(self.lon_value)
        if self.lat_value:
            lat_c = float(self.lat_value)
        if self.lat_value and self.lon_value:
            self.df = self.dset.sel({self.lat_name: lat_c,
                                     self.lon_name: lon_c},
                                    method='nearest')
        else:
            self.df = self.dset
        if self.time_start_value or self.time_end_value:
            self.df = self.df.sel({self.time_name: slice(self.time_start_value,
                                                         self.time_end_value)})
        # Saving the time series into a tabular
        self.df = self.df[self.varname].squeeze().to_dataframe().dropna()
        self.df.to_csv(self.save, sep='\t')
        # Plot the time series into png image
        fig = plt.figure(figsize=(15, 5))
        ax = plt.subplot(111)
        self.df[self.varname].plot(ax=ax)
        if self.title:
            plt.title(self.title)
        if self.xlabel:
            plt.xlabel(self.xlabel)
        if self.ylabel:
            plt.ylabel(self.ylabel)
        if self.format_date:
            ax.xaxis.set_major_formatter(DateFormatter(self.format_date))
        fig.tight_layout()
        fig.savefig(self.output)


if __name__ == '__main__':
    warnings.filterwarnings("ignore")
    parser = argparse.ArgumentParser()
    parser.add_argument(
        'input',
        help='input filename with geographical coordinates (netCDF format)'
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
        '--save',
        help='save resulting tabular file (tabular format) into filename'
    )
    parser.add_argument(
        '--config',
        help='pass timeseries parameters via a config file'
    )
    parser.add_argument(
        "-v", "--verbose",
        help="switch on verbose mode",
        action="store_true")
    args = parser.parse_args()

    dset = TimeSeries(input=args.input, varname=args.varname,
                      output=args.output, save=args.save, verbose=args.verbose,
                      config_file=args.config)
    dset.plot()

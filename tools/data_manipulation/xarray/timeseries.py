#!/usr/bin/env python3
#
#
# usage:  netCDF_timeseries.py [-h] [--output OUTPUT]
#                               [--start_time STARTTIME]
#                               [--end_time ENDTIME]
#                               [--xlim "x1"]
#                               [--ylim "y1"]
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
#  --output OUTPUT            output filename to store resulting timeseries file (csv format)
#  --start_time STARTTIME     starting time index for timeseries ("0 1 2 3")
#  --end_time ENDTIME         ending time index for timeseries ("0 1 2 3")
#  --xlim                     limited geographical point longitudes "x1"
#  --ylim                     limited geographical point latitudes "y1"
#  --config                   extraction parameters are passed via a config file
#                             (overwrite other extraction options)
#  -v, --verbose               switch on verbose mode
#
import argparse
import ast
import warnings
from pathlib import Path
import netCDF4
import xarray as xr
import pandas as pd
class TimeSeries ():
    def __init__(self, input, varname, output, verbose=False,
                 config_file=""):

        li = list(input.split(","))
        if len(li) > 1:
            self.input = li
        else:
            self.input = input

        self.varname = varname
        self.xylim_supported = True
        self.output = output
        self.verbose = verbose
        self.time_start_value = []
        self.time_start_end = []
        self.xlim = ""
        self.ylim = ""

        if config_file != "" and config_file is not None:
            with open(config_file) as f:
                sdict = ''.join(
                    f.read().replace("\n", "").split('{')[1].split('}')[0]
                    )
                tmp = ast.literal_eval('{' + sdict.strip() + '}')
                for key in tmp:
                    if key == 'time_start_value':  
                        time_start_value = tmp[key]
                        self.time_start_value = list(map(int, time_start_value.split(",")))
                    if key == 'time_end_value':  
                        time_end_value = tmp[key]
                        self.time_end_value = list(map(int, time_end_value.split(",")))
                    if key == 'xlim': 
                        self.xlim = tmp[key]
                    if key == 'ylim':
                        self.ylim = tmp[key]
                   
 
        if type(self.input) is list:
            self.dset = xr.open_mfdataset(self.input,r, use_cftime=True)
        else:
            self.dset = xr.open_dataset(self.input,r, use_cftime=True)

        if verbose:
            print("input: ", self.input)
            print("varname: ", self.varname)
            print("time_start_value: ", self.time_start_value)
            print("time_end_value: ", self.time_end_value)
            print("output: ", self.output)
            print("xlim: ", self.xlim)
            print("ylim: ", self.ylim)
            
    def plot(self):
        lon_c = float(self.xlim)
        lat_c = float(self.ylim)            
        sq_diff_lat = (lat - lat_c)**2
        sq_diff_lon = (lon - lon_c)**2
        min_index_lat = sq_diff_lat.argmin()
        min_index_lon = sq_diff_lon.argmin()
        date_range = pd.date_range(start = starting_date, end = ending_date)
        df = pd.DataFrame(0, columns = ['varname'], index = date_range)
        dt = np.arange(0, data.variables['time'].size)
        self.output = df
        for time_index in dt:
            df.iloc[time_index] = temp[time_index,min_index_lat ,min_index_lon]

        # Saving the time series into a csv
        self.output.to_csv('Timeseries.csv')
        

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
        help='output filename to store resulting csv file (csv format)'
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
                     output=args.output, verbose=args.verbose,
                     config_file=args.config)
    dset.plot()

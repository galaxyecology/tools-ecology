#!/usr/bin/env python3
#
#  Apply operations on selected variables
# - scale
# one can also select the range of time (for timeseries)
# to apply these operations over the range only
# when a range of time is selected and when scaling, one
# can choose to save the entire timeseries or
# the selected range only.
# when scaling, one can add additional filters on dimensions
# (typically used to filter over latitudes and longitudes)


import argparse
import re
import warnings
from pathlib import Path

import xarray as xr  # noqa: E402


class netCDF2netCDF ():
    def __init__(self, infile, varname, scale="",
                 output="output.netcdf",
                 write_all=False,
                 keep_attributes=True,
                 filter_list="",
                 where_config="",
                 other="",
                 sel=False,
                 drop=False,
                 verbose=False):
        self.drop = drop
        if Path(where_config).exists():
            f = open(where_config)
            self.where = f.read().replace("\n", "")
        else:
            self.where = ""
        self.other = other
        self.sel = sel
        li = list(infile.split(","))
        if len(li) > 1:
            self.infile = li
        else:
            self.infile = infile
        self.verbose = verbose
        if varname == 'None' or varname is None:
            self.varname = varname
        else:
            li = list(varname.split(","))
            self.varname = li
        self.write_all = write_all
        self.keep_attributes = keep_attributes
        if self.keep_attributes:
            xr.set_options(keep_attrs=True)
        self.filter = filter_list
        self.selection = {}
        self.method = {}
        if scale == "" or scale is None:
            self.scale = 1
        else:
            self.scale = float(scale)
        if output is None:
            self.output = "output.netcdf"
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
            print("keep_attributes: ", self.keep_attributes)
            print("sel: ", self.sel)
            print("output: ", self.output)

    def apply_selection(self):
        self.dset = self.ds
        for key in self.selection:
            if 'slice' in str(self.selection[key]):
                self.dset = self.dset.sel(
                    {key: self.selection[key]}
                    )
            else:
                self.dset = self.dset.sel(
                    {key: self.selection[key]},
                    method=self.method[key]
                    )

    def dimension_selection(self, single_filter):
        split_filter = single_filter.split('#')
        dimension_varname = split_filter[0]
        op = split_filter[1]
        if self.sel:
            ll = float(split_filter[2])
        else:
            ll = int(split_filter[2])
        if (op == 'sl'):
            if self.sel:
                rl = float(split_filter[3])
            else:
                rl = int(split_filter[3])
            self.selection[dimension_varname] = slice(ll, rl)
        elif (op == 'to'):
            self.selection[dimension_varname] = slice(None, ll)
        elif (op == 'from'):
            self.selection[dimension_varname] = slice(ll, None)
        elif (op == 'is'):
            self.selection[dimension_varname] = ll
            if self.sel:
                rl = split_filter[3]
                if 'None' in rl:
                    self.method[dimension_varname] = None
                else:
                    self.method[dimension_varname] = rl

    def filter_selection(self):
        for single_filter in self.filter:
            self.dimension_selection(single_filter)

        if self.sel:
            self.apply_selection()
        else:
            self.dset = \
                self.ds.isel(self.selection)

        if self.varname != 'None' and self.varname is not None:
            for var in self.varname:
                self.dset[var] = \
                    self.dset[var]*self.scale

    def compute(self):
        if self.dset is None:
            if type(self.infile) is list:
                self.ds = xr.open_mfdataset(self.infile)
            else:
                self.ds = xr.open_dataset(self.infile)
            if self.where != "":
                if self.drop:
                    if self.verbose:
                        print("Where with drop=True")
                    self.ds = self.ds.where(
                        self.eval_where(self.where),
                        drop=True
                        )
                elif self.other is not None and self.other != "":
                    if self.verbose:
                        print("Where with  other=", float(self.other))
                    self.ds = self.ds.where(
                        self.eval_where(self.where),
                        other=float(self.other)
                        )
                else:
                    self.ds = self.ds.where(
                        self.eval_where(self.where)
                        )
            self.filter_selection()
            if self.verbose:
                print(self.selection)

    def save(self):
        if self.varname != 'None' and \
            self.varname is not None and \
                not self.write_all:
            self.dset[self.varname].to_netcdf(self.output)
        else:
            self.dset.to_netcdf(self.output)

    def is_float(self, element) -> bool:
        try:
            float(element)
            return True
        except ValueError:
            return False

    def eval_where(self, where_condition):
        eval_cond = None
        list_names = list(set(
                        list(self.ds.keys()) +
                        list(self.ds.coords.keys()))
                        )
        wcond = where_condition
        check_cond = where_condition
        for var in list_names:
            wcond = wcond.replace(var, ' self.ds.' + var + ' ')
            check_cond = check_cond.replace(var, '')
        to_remove = "[><=&|()]"
        check_cond = re.sub(to_remove, "", check_cond).replace("!", "")
        check_cond = re.sub(' +', ' ', check_cond).strip()
        list_flt = check_cond.split(" ")
        no_convert = False
        for num in list_flt:
            if not self.is_float(num):
                no_convert = True
        if not no_convert:
            eval_cond = eval(wcond)
        return eval_cond


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
        '--where',
        help='filename with where condition to be evaluated'
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
        '--other',
        help='Value to use for locations where condition is False (float)'
    )
    parser.add_argument(
        "--write_all",
        help="write all data to netCDF",
        action="store_true")
    parser.add_argument(
        "--keep_attributes",
        help="Keep all attributes",
        action="store_true")
    parser.add_argument(
        "-v", "--verbose",
        help="switch on verbose mode",
        action="store_true")
    parser.add_argument(
        "--selection",
        help="select by values",
        action="store_true")
    parser.add_argument(
        "--drop",
        help="drop values where condition is not met",
        action="store_true")
    args = parser.parse_args()

    print("args.selection", args.selection)
    dset = netCDF2netCDF(infile=args.input, varname=args.varname,
                         scale=args.scale, output=args.output,
                         write_all=args.write_all,
                         sel=args.selection,
                         keep_attributes=args.keep_attributes,
                         filter_list=args.filter,
                         where_config=args.where,
                         drop=args.drop, other=args.other,
                         verbose=args.verbose)
    dset.compute()
    dset.save()

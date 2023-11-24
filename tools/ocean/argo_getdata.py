# author: Marie Jossé

# Python script

#############################
#      Argo data access     #
#############################

# Packages : argopy


# Load arguments
import argparse
import sys

import argopy

command_line_args = sys.argv[1:]


parser = argparse.ArgumentParser(description="Retrieve argo Data")
# Add arguments

parser.add_argument("--user", type=str,
                    help="User mode : standard, expert or research")
parser.add_argument("--cardinal_1", type=float, help="Longitude min")
parser.add_argument("--cardinal_2", type=float, help="Longitude max")
parser.add_argument("--cardinal_3", type=float, help="Latitude min")
parser.add_argument("--cardinal_4", type=float, help="Latitude max")
parser.add_argument("--pressure_1", type=float, help="Pressure min")
parser.add_argument("--pressure_2", type=float, help="Pressure max")
parser.add_argument("--date_1", type=str, help="Starting date")
parser.add_argument("--date_2", type=str, help="Ending date.")
parser.add_argument("--wmo", type=str, help="WMO: argo's identifier")
parser.add_argument("--profile", type=str, help="Number of profiles")
parser.add_argument("--params", type=str, help="List of bgc parameters")
parser.add_argument("--measured", type=str, help="List of bgc parameters")
parser.add_argument("--output_argo", type=str, help="Output data from argo")

args = parser.parse_args(command_line_args)


# Parse the command line arguments

print(args)
# Import data

user = args.user
cardinal_1 = args.cardinal_1
cardinal_2 = args.cardinal_2
cardinal_3 = args.cardinal_3
cardinal_4 = args.cardinal_4
pressure_1 = args.pressure_1
pressure_2 = args.pressure_2
date_1 = args.date_1
date_2 = args.date_2
wmo = args.wmo
if wmo is not None:
    wmo = list(map(int, wmo.split(",")))
profile = args.profile
if profile is not None:
    profile = list(map(int, profile.split(",")))
params = args.params
if params is not None:
    params = list(map(int, params.split(",")))
    if len(params) == 83:
        params = "all"
measured = args.measured
if measured is not None:
    measured = measured.split(",")

# Let’s import the argopy data fetcher:

######################
#       User mode    #
######################
# By default,
# all argopy data fetchers are set to work with a standard user mode.
# To change that

argopy.set_options(mode=user)

######################
# Fetching Argo data #
######################
# Data selection #

# To access Argo data with a DataFetcher,
# you need to define how to select your data of interest.
# argopy provides 3 different data selection methods:

argo_data = argopy.DataFetcher()

# 🗺 For a space/time domain #

if (cardinal_1 is not None):
    mode = "region"
    argo_data = argo_data.region([cardinal_1, cardinal_2,
                                  cardinal_3, cardinal_4,
                                  pressure_1, pressure_2,
                                  date_1, date_2])

# ⚓ For one or more profiles #
# Use the fetcher access point argopy.DataFetcher.profile()
# to specify the float WMO platform number
# and the profile cycle number(s) to retrieve profiles for.
elif (wmo is not None and profile is not None):
    argo_data = argo_data.profile(wmo, profile)
    # can also be argo_data = argo_data.profile(6902755, [3, 12])
    mode = "profile"

# 🤖 For one or more floats #
# If you know the Argo float unique identifier number called a WMO number
# you can use the fetcher access point DataFetcher.float()
# to specify one or more float WMO platform numbers to select.
else:
    argo_data = argo_data.float(wmo)
    # can also be argo_data = argo_data.float([6902746, 6902755])
    mode = "float"
    argo_data.data

# Data sources #
# Let’s start with standard import:
# argopy.reset_options()
# Specify data source erddap, gdac or argovis

#if (ftp != "") :
    #argopy.set_options(src = "gdac", ftp = ftp)
#else :
    #argopy.set_options(src = "erddap")

# With remote, online data sources,
# it may happens that the data server is experiencing down time.
print(argopy.status())

# Dataset #
# Argo data are distributed as a single dataset.
# It is referenced at https://doi.org/10.17882/42182.
# But they are several Argo missions with specific files and parameters
# that need special handling by argopy, namely:
#   - the core Argo Mission: from floats that measure temperature,
#     salinity, pressure down to 2000m,
#   - the Deep Argo Mission: from floats that measure temperature,
#     salinity, pressure down to 6000m,
#   - and the BGC-Argo Mission: from floats that measure temperature,
#     salinity, pressure and oxygen, pH, nitrate, chlorophyll,
#     backscatter, irradiance down to 2000m.
# You can choose between phy or bgc
if (params is None):
    argopy.set_options(dataset="phy")
else:
    argopy.set_options(dataset="bgc")
    if (measured is not None):
        argo_data = argopy.DataFetcher(params=params, measured=measured)
        if (mode == "region"):
            argo_data = argo_data.region([cardinal_1, cardinal_2,
                                          cardinal_3, cardinal_4,
                                          pressure_1, pressure_2,
                                          date_1, date_2])
        elif (mode == "profile"):
            argo_data = argo_data.profile(wmo, profile)
        else:
            argo_data = argo_data.float(wmo)
    else:
        argo_data = argopy.DataFetcher(params=params, measured=None)
        if (mode == "region"):
            argo_data = argo_data.region([cardinal_1, cardinal_2,
                                          cardinal_3, cardinal_4,
                                          pressure_1, pressure_2,
                                          date_1, date_2])
        elif (mode == "profile"):
            argo_data = argo_data.profile(wmo, profile)
        else:
            argo_data = argo_data.float(wmo)

# Data fetching #
# To fetch (i.e. access, download, format) Argo data,
# argopy provides the DataFetcher class.
# Several DataFetcher arguments exist to help you select the dataset,
# the data source and the user mode the most suited for your applications;
# and also to improve performances.

# You define the selection of data you want to fetch
# with one of the DataFetcher methods: region, float or profile.
# 2 lines to download Argo data: import and fetch !

argo_data = argo_data.load().data
argo_data.to_netcdf("argo_data.nc")

# argo_metadata = argo_data.to_index()

print(argo_data)

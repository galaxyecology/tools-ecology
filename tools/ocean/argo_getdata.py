# author: Marie Jossé

# Python script

###############################
##      Argo data access     ##
###############################

##### Packages : argopy


##### Load arguments

import sys
import argopy
from argopy import DataFetcher as ArgoDataFetcher

args = sys.argv

##### Import data

if len(args) < 3:
	raise ValueError("This tool needs at least 3 arguments")
else:
	user = args[1]
	if (args[2] != ""):
		cardinal_1 = float(args[2])
		cardinal_2 = float(args[3])
		cardinal_3 = float(args[4])
		cardinal_4 = float(args[5])
		pressure_1 = float(args[6])
		pressure_2 = float(args[7])
	else:
		cardinal_1 = args[2]
		cardinal_2 = args[3]
		cardinal_3 = args[4]
		cardinal_4 = args[5]
		pressure_1 = args[6]
		pressure_2 = args[7]
	date_1 = args[8]
	date_2 = args[9]
	wmo = args[10]
	if (wmo != "") :
		wmo = wmo.split(",")
		wmo = list(map(int, wmo))
	profile = args[11]
	if (profile != "") :
		profile = profile.split(",")
		profile = list(map(int, profile))
#    source = args[13]
#	ftp = args[12]
#    dataset = args[15]
	params = args[12]
	if (params != "") :
		params = params.split(",")
		if (len(params) == 83) :
			params = "all"
		measured = args[13]
		measured = measured.split(",")

# Let’s import the argopy data fetcher:

##########################
###       User mode    ###
##########################
# By default, all argopy data fetchers are set to work with a standard user mode.
# To change that 

argopy.set_options(mode = user)

##########################
### Fetching Argo data ###
##########################
## Data selection ##

# To access Argo data with a DataFetcher, you need to define how to select your data of interest.
# argopy provides 3 different data selection methods:

argo_data = argopy.DataFetcher()

# 🗺 For a space/time domain #

if (cardinal_1 != "") :
	argo_data = argo_data.region([cardinal_1, cardinal_2, cardinal_3, cardinal_4, pressure_1, pressure_2, date_1, date_2])
	mode = "region"
	argo_data.data

# ⚓ For one or more profiles #
# Use the fetcher access point argopy.DataFetcher.profile() to specify the float WMO platform number and the profile cycle number(s) to retrieve profiles for.
elif (wmo != "" and profile != "") :
	argo_data = argo_data.profile(wmo, profile)
	# can also be argo_data = argo_data.profile(6902755, [3, 12])
	mode = "profile"
	argo_data.data

# 🤖 For one or more floats #
# If you know the Argo float unique identifier number called a WMO number you can use the fetcher access point DataFetcher.float() to specify one or more float WMO platform numbers to select.
else :
	argo_data = argo_data.float(wmo)
	# can also be argo_data = argo_data.float([6902746, 6902755])
	mode = "float"
	argo_data.data


## Data sources ##
# Let’s start with standard import:
# argopy.reset_options()
# Specify data source erddap, gdac or argovis

#if (ftp != "") :
#	argopy.set_options(src = "gdac", ftp = ftp)
#else : 
#	argopy.set_options(src = "erddap")

# With remote, online data sources, it may happens that the data server is experiencing down time.
print(argopy.status())

## Dataset ##
# Argo data are distributed as a single dataset. It is referenced at https://doi.org/10.17882/42182.
# But they are several Argo missions with specific files and parameters that need special handling by argopy, namely:
#   - the core Argo Mission: from floats that measure temperature, salinity, pressure down to 2000m,
#   - the Deep Argo Mission: from floats that measure temperature, salinity, pressure down to 6000m,
#   - and the BGC-Argo Mission: from floats that measure temperature, salinity, pressure and oxygen, pH, nitrate, chlorophyll, backscatter, irradiance down to 2000m.
# You can choose between phy or bgc
if (params == "") :
	argopy.set_options(dataset = "phy")
else :
	argopy.set_options(dataset = "bgc")
	if (measured != ["None"]) :
		argo_data = argopy.DataFetcher(params = params, measured = measured)
		if (mode == "region") :
			argo_data = argo_data.region([cardinal_1, cardinal_2, cardinal_3, cardinal_4, pressure_1, pressure_2, date_1, date_2])
		elif (mode == "profile") :
			argo_data = argo_data.profile(wmo, profile)
		else :
			argo_data = argo_data.float(wmo)
	else:
		argo_data = argopy.DataFetcher(params = params, measured = None)
		if (mode == "region") :
			argo_data = argo_data.region([cardinal_1, cardinal_2, cardinal_3, cardinal_4, pressure_1, pressure_2, date_1, date_2])
		elif (mode == "profile") :
			argo_data = argo_data.profile(wmo, profile)
		else :
			argo_data = argo_data.float(wmo)
	
## Data fetching ##
# To fetch (i.e. access, download, format) Argo data, argopy provides the DataFetcher class. 
# Several DataFetcher arguments exist to help you select the dataset, the data source and the user mode the most suited for your applications; and also to improve performances.

# You define the selection of data you want to fetch with one of the DataFetcher methods: region, float or profile.
# 2 lines to download Argo data: import and fetch !

argo_data = argo_data.load().data
argo_data.to_netcdf("argo_data.nc")

#argo_metadata = argo_data.to_index()

#print(argo_data)

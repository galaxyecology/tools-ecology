# PURPOSE
Extracts time series for a given variable in a .csv format file. This file can be used to plot the timeseries easily.

## Executable Python Code
```
from netCDF4 import Dataset
import numpy as np
import pandas as pd 
data = Dataset(r'D:\GeoDeltaLabs Projects\Handling netCDF files with Python\Temparature Data\1961.nc', 'r')
lat = data.variables['lat'][:]
lon = data.variables['lon'][:]
lat_katmandu =  27.697817
lon_katmandu =  85.329806
sq_diff_lat = (lat - lat_katmandu)**2
sq_diff_lon = (lon - lon_katmandu)**2
min_index_lat = sq_diff_lat.argmin()
min_index_lon = sq_diff_lon.argmin()
temp = data.variables['tave']
starting_date = data.variables['time'].units[14:24]
ending_date = data.variables['time'].units[14:18] + '-12-31'
date_range = pd.date_range(start = starting_date, end = ending_date)
df = pd.DataFrame(0, columns = ['Temparature'], index = date_range)
dt = np.arange(0, data.variables['time'].size)
for time_index in dt:
    df.iloc[time_index] = temp[time_index,min_index_lat ,min_index_lon]
df.to_csv('temparature_Katmandu.csv')
```


Link to website (data) <https://www.chikyu.ac.jp/precip/english/>
Zenodo Link for Data   <https://zenodo.org/record/6796362/files/Aphrodites_daily_mean_temperature_monsoon_Asia_1961.nc?download=1>`

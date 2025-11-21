import os
import sys
from datetime import datetime

import cdsapi

import pandas as pd

# Read arguments
# get-era5.py INITIAL_DATE FINAL_DATE XMIN XMAX YMIN YMAX

SDATE = sys.argv[1]
EDATE = sys.argv[2]
WEST = sys.argv[3]
EAST = sys.argv[4]
SOUTH = sys.argv[5]
NORTH = sys.argv[6]

start_date = datetime.strptime(SDATE, "%Y-%m-%d")
end_date = datetime.strptime(EDATE, "%Y-%m-%d")

date_list = list(
    pd.date_range(start_date, end_date, freq='D')
    .strftime('%Y-%m-%d')
)

VARIABLES = ['10m_u_component_of_wind',
                '10m_v_component_of_wind',
                '2m_dewpoint_temperature',
                '2m_temperature',
                'mean_sea_level_pressure',
                'surface_solar_radiation_downwards',
                'surface_thermal_radiation_downwards',
                'total_precipitation']

OUTPUT_FILENAME = 'ccds_era5_af_data.zip'

c = cdsapi.Client(
    url="https://cds.climate.copernicus.eu/api",
    key=os.environ["CDS_TOKEN"]
)

fl = c.retrieve(
    'reanalysis-era5-single-levels',
    {
        'product_type': 'reanalysis',
        'format': 'netcdf',
        'variable': VARIABLES,
        'date': date_list,
        'time': [
            '00:00', '01:00', '02:00',
            '03:00', '04:00', '05:00',
            '06:00', '07:00', '08:00',
            '09:00', '10:00', '11:00',
            '12:00', '13:00', '14:00',
            '15:00', '16:00', '17:00',
            '18:00', '19:00', '20:00',
            '21:00', '22:00', '23:00',
        ],
        'area': [NORTH, WEST, SOUTH, EAST],
        'data_format': "netcdf4",
        "download_format": "zip"
    },
    OUTPUT_FILENAME)

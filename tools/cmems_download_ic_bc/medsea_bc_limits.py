# This python script reads a file with a subset of the CMEMS MedSea data for
# the selected region to check the location of the boundaries of the system:
# The downloaded 3D should be one pixel beyond the limits of the simulation.
# Data for boundary, just the two pixels bracketing the edge of the
# simulation.

import argparse

import netCDF4


def arguments():
    parser = argparse.ArgumentParser(description='''
   Find model boundaries in Copernicus products
    ''', formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument('--xmin', '-xm',
                        type=str,
                        required=True,
                        help='xmin in model')
    parser.add_argument('--xmax', '-xM',
                        type=str,
                        required=True,
                        help='xmax in model')
    parser.add_argument('--ymin', '-ym',
                        type=str,
                        required=True,
                        help='ymin in model')
    parser.add_argument('--ymax', '-yM',
                        type=str,
                        required=True,
                        help='ymax in model')
    parser.add_argument('--infile', '-i',
                        type=str,
                        required=True,
                        help='Copernicus input file')
    return parser.parse_args()


args = arguments()

XMIN = float(args.xmin)
XMAX = float(args.xmax)
YMIN = float(args.ymin)
YMAX = float(args.ymax)
infile = args.infile

outFile = 'copernicus_lims.inc'

# Now, get the grid of the copernicus product and check the limits:

SD = netCDF4.Dataset(infile)
lon = SD['longitude'][:]
lat = SD['latitude'][:]

# Domain limits

Ifound = False
io = 0
while not Ifound:
    Ifound = lon[io] < XMIN and lon[io+1] >= XMIN
    if not Ifound:
        io += 1

Ifound = False
ie = len(lon)-1
while not Ifound:
    Ifound = lon[ie] > XMAX and lon[ie-1] <= XMAX
    if not Ifound:
        ie -= 1

Jfound = False
jo = 0
while not Jfound:
    Jfound = lat[jo] < YMIN and lat[jo+1] >= YMIN
    if not Jfound:
        jo += 1

Jfound = False
je = len(lat)-1
while not Jfound:
    Jfound = lat[je] > YMAX and lat[je-1] <= YMAX
    if not Jfound:
        je -= 1

copernicus_xmin = lon[io]
copernicus_xmax = lon[ie]
copernicus_ymin = lat[jo]
copernicus_ymax = lat[je]

copernicus_south_y1 = lat[jo]
copernicus_south_y2 = lat[jo+1]
copernicus_north_y1 = lat[je-1]
copernicus_north_y2 = lat[je]

copernicus_west_x1 = lon[io]
copernicus_west_x2 = lon[io+1]
copernicus_east_x1 = lon[ie-1]
copernicus_east_x2 = lon[ie]

with open(outFile, 'w') as f:
    f.write('DOMAIN_XMIN=%f\n' % (XMIN))
    f.write('DOMAIN_XMAX=%f\n' % (XMAX))
    f.write('DOMAIN_YMIN=%f\n' % (YMIN))
    f.write('DOMAIN_YMAX=%f\n' % (YMAX))
    f.write('COPERNICUS_XMIN=%f\n' % (copernicus_xmin))
    f.write('COPERNICUS_XMAX=%f\n' % (copernicus_xmax))
    f.write('COPERNICUS_YMIN=%f\n' % (copernicus_ymin))
    f.write('COPERNICUS_YMAX=%f\n' % (copernicus_ymax))
    f.write('COPERNICUS_SOUTH_Y1=%f\n' % (copernicus_south_y1))
    f.write('COPERNICUS_SOUTH_Y2=%f\n' % (copernicus_south_y2))
    f.write('COPERNICUS_NORTH_Y1=%f\n' % (copernicus_north_y1))
    f.write('COPERNICUS_NORTH_Y2=%f\n' % (copernicus_north_y2))
    f.write('COPERNICUS_WEST_X1=%f\n' % (copernicus_west_x1))
    f.write('COPERNICUS_WEST_X2=%f\n' % (copernicus_west_x2))
    f.write('COPERNICUS_EAST_X1=%f\n' % (copernicus_east_x1))
    f.write('COPERNICUS_EAST_X2=%f\n' % (copernicus_east_x2))

#Julia script

###############################
##    DIVAndrun analsysis    ##
###############################
import Pkg; 
#Pkg.add("julia")  
using Pkg
Pkg.status()

### Import packages
# using NCDatasets
# using PhysOcean
# using DataStructures
using DIVAnd
using Dates
# using Statistics
# using Random

using Printf
# Pkg.add("PyCall")
# using PyCall
# PyCall.pyimport("matplotlib.pyplot")
### Import data
# Récupération des arguments de la ligne de commande
args = ARGS

# Importation des données
if length(args) < 4
    error("This tool needs at least 4 arguments")
else
    netcdf_data = args[1]
    longmin = parse(Float64, args[2])
    longmax = parse(Float64, args[3])
    latmin = parse(Float64, args[4])
    latmax = parse(Float64, args[5])
    startdate = args[6] # yyyy,mm,dd
    enddate = args[7]
    varname = args[8]
    selmin = parse(Int, args[9])
    selmax = parse(Int, args[10])
end

## This script will create a climatology:
# 1. ODV data reading.
# 2. Extraction of bathymetry and creation of mask
# 3. Data download from other sources and duplicate removal.
# 4. Quality control.
# 5. Parameter optimisation.
# 6. Spatio-temporal interpolation with DIVAnd.


### Configuration
# Define the horizontal, vertical (depth levels) and temporal resolutions.
# Select the variable of interest

dx, dy = 0.125, 0.125
lonr = longmin:dx:longmax
latr = latmin:dy:latmax

# Extracting year, month, and day
startyear = startdate[1:4]   # extract first four characters (year)
startmonth = startdate[5:6]  # extract characters 5 and 6 (month)
startday = startdate[7:8]    # extract last two characters (day)
# Converting to integers
startyear = parse(Int, startyear)
startmonth = parse(Int, startmonth)
startday = parse(Int, startday)

endyear = enddate[1:4]   # extract first four characters (year)
endmonth = enddate[5:6]  # extract characters 5 and 6 (month)
endday = enddate[7:8]    # extract last two characters (day)
endyear = parse(Int, endyear)
endmonth = parse(Int, endmonth)
endday = parse(Int, endday)

timerange = [Date(startyear, startmonth, startday),Date(endyear, endmonth, endday)];

depthr = [0.,5., 10., 15., 20., 25., 30., 40., 50., 66, 
    75, 85, 100, 112, 125, 135, 150, 175, 200, 225, 250, 
    275, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 
    800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1250, 
    1300, 1350, 1400, 1450, 1500, 1600, 1750, 1850, 2000];
depthr = [0.,10.,20.];

varname = varname
yearlist = [1900:2023];
monthlist = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]];

# We create here the variable TS (for "tDataset(netcdf_data,"r")ime selector"), which allows us to work with the observations corresponding to each period of interest.

TS = DIVAnd.TimeSelectorYearListMonthList(yearlist,monthlist);
@show TS;

figdir = "outputs/"
if ~(isdir(figdir))
    mkdir(figdir)
else
    @info("Figure directory already exists")
end
### 1. Read your ODV file
# Adapt the datadir and datafile values.
# The example is based on a sub-setting of the Mediterranean Sea aggregated dataset.
# The dataset has been extracted around the Adriatic Sea and exported to a netCDF using Ocean Data 
datadir = "../data"

datafile = netcdf_data

# Then you can read the full file:
@time obsval,obslon,obslat,obsdepth,obstime,obsid = NCODV.load(Float64, datafile, 
    "Water body $(varname)");

#figure("Data")
#ax = subplot(1,1,1)
#plot(obslon, obslat, "ko", markersize=.1, markerfacecolor="k")
#aspectratio = 1/cos(mean(latr) * pi/180)
#ax.tick_params("both",labelsize=6)
#gca().set_aspect(aspectratio)
#figname = "Data.png"
#plt.savefig(joinpath(figdir, figname), dpi=600, bbox_inches="tight");
#plt.close_figs()

# Check the extremal values of the observations
checkobs((obslon,obslat,obsdepth,obstime),obsval,obsid)

### 2. Extract the bathymetry

# It is used to delimit the domain where the interpolation is performed.
## 2.1 Choice of bathymetry

# Modify bathname according to the resolution required.

download("https://dox.ulg.ac.be/index.php/s/U0pqyXhcQrXjEUX/download", "gebco_30sec_8.nc")
bathname="gebco_30sec_8.nc"

@time bx,by,b = load_bath(bathname,true,lonr,latr);
     
#figure("Data-Bathymetry")
#ax = subplot(1,1,1)
#pcolor(bx, by, permutedims(b, [2,1]));
#colorbar(orientation="vertical", shrink=0.8).ax.tick_params(labelsize=8)
#contour(bx, by, permutedims(b, [2,1]), [0, 0.1], colors="k", linewidths=.5)
#gca().set_aspect(aspectratio)
#ax.tick_params("both",labelsize=6)
#figname = "Bathymetry.png"
#plt.savefig(joinpath(figdir, figname), dpi=600, bbox_inches="tight");
#plt.close_figs()

## 2.2 Create mask
# False for sea
# True for land

mask = falses(size(b,1),size(b,2),length(depthr))
for k = 1:length(depthr)
    for j = 1:size(b,2)
        for i = 1:size(b,1)
            mask[i,j,k] = b[i,j] >= depthr[k]
        end
    end
end
@show size(mask)


#figure("Data-Mask")
#ax = subplot(1,1,1)
#gca().set_aspect(aspectratio)
#ax.tick_params("both",labelsize=6)
#ax.pcolor(bx, by, transpose(mask[:,:,1]), cmap=plt.cm.binary_r)
#ax.set_title("Land-sea mask")
#figname = "Mask.png"
#plt.savefig(joinpath(figdir, figname), dpi=600, bbox_inches="tight");
#plt.close_figs()

### 3. Quality control
# We check the salinity value.
# Adapt the criteria to your region and variable.

sel = (obsval .<= selmax) .& (obsval .>= selmin);

obsval = obsval[sel]
obslon = obslon[sel]
obslat = obslat[sel]
obsdepth = obsdepth[sel]
obstime = obstime[sel]
obsid = obsid[sel];


### 4. Analysis parameters
# Correlation lengths and noise-to-signal ratio

# We will use the function diva3D for the calculations.
# With this function, the correlation length has to be defined in meters, not in degrees.

sz = (length(lonr),length(latr),length(depthr));
lenx = fill(100_000.,sz)   # 100 km
leny = fill(100_000.,sz)   # 100 km
lenz = fill(25.,sz);      # 25 m 
len = (lenx, leny, lenz);
epsilon2 = 0.1;
#epsilon2 = epsilon2 * rdiag;

### Output file name
outputdir = "outputs_netcdf/"
if !isdir(outputdir)
    mkpath(outputdir)
end
filename = joinpath(outputdir, "Water_body_$(replace(varname," "=>"_")).nc")

### 7. Analysis
# Remove the result file before running the analysis, otherwise you'll get the message
if isfile(filename)
    rm(filename) # delete the previous analysis
    @info "Removing file $filename"
end

## 7.1 Plotting function
# Define a plotting function that will be applied for each time index and depth level.
# All the figures will be saved in a selected directory.
     
function plotres(timeindex,sel,fit,erri)
    tmp = copy(fit)
    nx,ny,nz = size(tmp)
    for i in 1:nz
        figure("Additional-Data")
        ax = subplot(1,1,1)
        ax.tick_params("both",labelsize=6)
        ylim(39.0, 46.0);
        xlim(11.5, 20.0);
        title("Depth: (timeindex)", fontsize=6)
        pcolor(lonr.-dx/2.,latr.-dy/2, permutedims(tmp[:,:,i], [2,1]);
               vmin = 33, vmax = 40)
        colorbar(extend="both", orientation="vertical", shrink=0.8).ax.tick_params(labelsize=8)

        contourf(bx,by,permutedims(b,[2,1]), levels = [-1e5,0],colors = [[.5,.5,.5]])
        aspectratio = 1/cos(mean(latr) * pi/180)
        gca().set_aspect(aspectratio)
        
        figname = varname * @sprintf("_%02d",i) * @sprintf("_%03d.png",timeindex)
        plt.savefig(joinpath(figdir, figname), dpi=600, bbox_inches="tight");
        plt.close_figs()
    end
end

## 7.2 Create the gridded fields using diva3d
# Here only the noise-to-signal ratio is estimated.
# Set fitcorrlen to true to also optimise the correlation length.

@time dbinfo = diva3d((lonr,latr,depthr,TS),
    (obslon,obslat,obsdepth,obstime), obsval,
    len, epsilon2,
    filename,varname,
    bathname=bathname,
    fitcorrlen = false,
    niter_e = 2,
    surfextend = true
    );

# Save the observation metadata in the NetCDF file.

DIVAnd.saveobs(filename,(obslon,obslat,obsdepth,obstime),obsid);
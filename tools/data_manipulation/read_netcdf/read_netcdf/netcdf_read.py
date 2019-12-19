#!/usr/bin/env python
#Alan Amosse then Yvan Le Bras

"""
A script for using regex substitutions on columns.
"""

import netCDF4
from netCDF4 import Dataset
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from pylab import *
import sys
import os
from scipy import spatial
from math import radians, cos, sin, asin, sqrt
import itertools

#####################
#####################
 
def checklist(dim_list, dim_name, filtre, threshold):
    if not dim_list:
        error="Error "+str(dim_name)+" has no value "+str(filtre)+" "+str(threshold)
        sys.exit(error)


#Return dist in km between two coord
#Thx to : https://stackoverflow.com/questions/4913349/haversine-formula-in-python-bearing-and-distance-between-two-gps-points
def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])

    # haversine formula 
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    r = 6371 # Radius of earth in kilometers. Use 3956 for miles
    return c * r


#Comparison functions, return a list of indexes for the user conditions 
def is_strict_inf(filename, dim_name, threshold):
    list_dim=[]
    for i in range(0,filename.variables[dim_name].size):
        if filename.variables[dim_name][i] < threshold:
            list_dim.append(i)
    checklist(list_dim,dim_name,"<",threshold)
    return list_dim

def is_equal_inf(filename, dim_name, threshold):
    list_dim=[]
    for i in range(0,filename.variables[dim_name].size):
        if filename.variables[dim_name][i] <= threshold:
            list_dim.append(i)
    checklist(list_dim,dim_name,"<=",threshold)
    return list_dim

def is_equal_sup(filename, dim_name, threshold):
    list_dim=[]
    for i in range(0,filename.variables[dim_name].size):
        if filename.variables[dim_name][i] >= threshold:
            list_dim.append(i)
    checklist(list_dim,dim_name,">=",threshold)
    return list_dim

def is_strict_sup(filename, dim_name, threshold):
    list_dim=[]
    for i in range(0,filename.variables[dim_name].size):
        if filename.variables[dim_name][i] > threshold:
            list_dim.append(i)
    checklist(list_dim,dim_name,">",threshold)
    return list_dim

def find_nearest(array,value):
    index = (np.abs(array-value)).argmin()
    return index

def is_equal(filename, dim_name, value):
    try:
        index=filename.variables[dim_name][:].tolist().index(value)
    except:
        index=find_nearest(filename.variables[dim_name][:],value)
    return index

def is_between_include(filename, dim_name, threshold1, threshold2):
    list_dim=[]
    for i in range(0,filename.variables[dim_name].size):
        if filename.variables[dim_name][i] >= threshold1 and filename.variables[dim_name][i] <= threshold2:
            list_dim.append(i)
    checklist(list_dim,dim_name,">=",threshold1)
    checklist(list_dim,dim_name,"=<",threshold2)
    return list_dim

def is_between_exclude(filename, dim_name, threshold1, threshold2):
    list_dim=[]
    for i in range(0,filename.variables[dim_name].size):
        if filename.variables[dim_name][i] > threshold1 and filename.variables[dim_name][i] < threshold2:
            list_dim.append(i)
    checklist(list_dim,dim_name,">",threshold1)
    checklist(list_dim,dim_name,"<",threshold2)
    return list_dim

#######################
#######################

#Get args
#Get Input file
inputfile=Dataset(sys.argv[1])
var_file_tab=sys.argv[2]
var=sys.argv[3] #Var chosen by user

Coord_bool=False


######################
######################
#len_threshold=1000000
len_threshold=7000
x_percent=0.75
threshold_latlon=100


#Check if coord is passed as parameter
arg_n=len(sys.argv)-1
if(((arg_n-3)%3)!=0):
    Coord_bool=True #Useful to get closest coord
    arg_n=arg_n-4 #Number of arg minus lat & lon
    name_dim_lat=str(sys.argv[-4])
    name_dim_lon=str(sys.argv[-2])
    value_dim_lat=float(sys.argv[-3])
    value_dim_lon=float(sys.argv[-1])

    #Get all lat & lon
    #try:
    if True:
        latitude=np.ma.MaskedArray(inputfile.variables[name_dim_lat])
        longitude=np.ma.MaskedArray(inputfile.variables[name_dim_lon])
        lat=latitude;lon=longitude #Usefull to keep the originals lat/lon vect before potentially resize it bellow.
        len_all_coord=len(lat)*len(lon)
        
        #print("len all coord "+str(len_all_coord)+" threshold "+str(len_threshold))

        #To avoid case when all_coord is to big and need to much memory
        #If the vector is too big, reduce it to its third in a loop until its < to the threshold
        while len_all_coord > len_threshold:
            
            if len(lat)<threshold_latlon: #If lat and lon are very different and lon is >> than lat. This way only lon is reduce and not lat.
                x_percent_len_lat=99999999
            else:
                x_percent_len_lat=int(x_percent*len(lat))

            if len(lon)<threshold_latlon: #If lat and lon are very different and lat is >> than lon. This way only lat is reduce and not lon.
                x_percent_len_lon=99999999
            else:
                x_percent_len_lon=int(x_percent*len(lon))

            #print("len(lat) :"+str(len(lat))+" x_percent_len_lat "+str(x_percent_len_lat))
            #print("len(lon) :"+str(len(lon))+" x_percent_len_lon "+str(x_percent_len_lon))

 
            pos_lat_user=find_nearest(lat,value_dim_lat)
            pos_lon_user=find_nearest(lon,value_dim_lon)

              
            #This part is to avoid having a vector that start bellow 0
            lat_reduced=int(pos_lat_user-x_percent_len_lat/2-1)
            if lat_reduced<0:
                lat_reduced=0
            lon_reduced=int(pos_lon_user-x_percent_len_lon/2-1)
            if lon_reduced<0:
                lon_reduced=0
            #Opposite here to avoid having vector with len > to len(vector)
            lat_extended=int(pos_lat_user+x_percent_len_lat/2-1)
            if lat_extended>len(lat):
                lat_extended=len(lat)
            lon_extended=int(pos_lon_user+x_percent_len_lon/2-1)
            if lon_extended>len(lon):
                lon_extended=len(lon)

            lat=lat[lat_reduced:lat_extended] #add a test to check if pos_lat_user-x_percent_len_lat/2-1 >0
            lon=lon[lon_reduced:lon_extended]
            #print("latreduced : "+str(lat_reduced)+" latextended "+str(lat_extended))
            #print("lonreduced : "+str(lon_reduced)+" lonextended "+str(lon_extended))
            #print("lat : "+str(lat))
            #print("lon : "+str(lon))
            len_all_coord=len(lat)*len(lon)

            #print ("len_all_coord : "+str(len_all_coord)+". len_lat : "+str(len(lat))+" .len_lon : "+str(len(lon)))

    else:
    #except:
        sys.exit("Latitude & Longitude not found") 

    #Set all lat-lon pair avaible in list_coord
    list_coord_dispo=[]
    for i in lat:
        for j in lon:
            list_coord_dispo.append(i);list_coord_dispo.append(j)

    #Reshape
    all_coord=np.reshape(list_coord_dispo,(lat.size*lon.size,2))
    #np.set_printoptions(threshold='nan')#to print full vec
    #print(str(all_coord))
    noval=True



#########################
#########################


#Get the file of variables and number of dims : var.tab
var_file=open(var_file_tab,"r") #read
lines=var_file.readlines() #line
dim_names=[]
for line in lines: #for every lines
    words=line.split()
    if (words[0]==var): #When line match user input var
        varndim=int(words[1])  #Get number of dim for the var
        for dim in range(2,varndim*2+2,2): #Get dim names
            dim_names.append(words[dim])
        #print ("Chosen var : "+sys.argv[3]+". Number of dimensions : "+str(varndim)+". Dimensions : "+str(dim_names)) #Standard msg
        

########################
########################


#Use a dictionary to save every lists of indexes
my_dic={} ##d["string{0}".format(x)]

for i in range(4,arg_n,3):
    #print("\nDimension name : "+sys.argv[i]+" action : "+sys.argv[i+1]+" .Value : "+sys.argv[i+2]+"\n") #Standard msg

    #Check if the dim selected for filtering is present in the var dimensions.
    if (sys.argv[i] not in dim_names):
        print("Warning ! "+sys.argv[i]+" is not a dimension of "+var+".\nThis filter will be skipped\nCheck in the file \"variables\" the dimensions available.\n\n")
        pass

    my_dic["string{0}".format(i)]="list_index_dim"
    my_dic_index="list_index_dim"+str(sys.argv[i])   #Possible improvement: Check if lon/lat are not parsed again

    #Apply every user filter. Call function and return list of index wich validate condition for every dim.
    if (sys.argv[i+1]=="l"): #<
        my_dic[my_dic_index]=is_strict_inf(inputfile, sys.argv[i], float(sys.argv[i+2]))
    if (sys.argv[i+1]=="le"): #<=
        my_dic[my_dic_index]=is_equal_inf(inputfile, sys.argv[i], float(sys.argv[i+2]))
    if (sys.argv[i+1]=="g"): #>
        my_dic[my_dic_index]=is_strict_sup(inputfile, sys.argv[i], float(sys.argv[i+2]))
    if (sys.argv[i+1]=="ge"): #>=
        my_dic[my_dic_index]=is_equal_sup(inputfile, sys.argv[i], float(sys.argv[i+2]))
    if (sys.argv[i+1]=="e"): #==
        my_dic[my_dic_index]=is_equal(inputfile, sys.argv[i], float(sys.argv[i+2]))
    if (sys.argv[i+1]==":"): #all
        my_dic[my_dic_index]=np.arange(inputfile.variables[sys.argv[i]].size)
    if (sys.argv[i+1]=="be"): #between_exclude
        #Get the 2 thresholds from the arg which looks like "threshold1-threshold2"
        threshold1=sys.argv[i+2].split("-")[0] 
        threshold2=sys.argv[i+2].split("-")[1] 
        my_dic[my_dic_index]=is_between_exclude(inputfile, sys.argv[i], float(threshold1), float(threshold2))
    if (sys.argv[i+1]=="bi"): #between_include
        #Get the 2 thresholds from the arg which looks like "threshold1-threshold2"
        threshold1=sys.argv[i+2].split("-")[0]
        threshold2=sys.argv[i+2].split("-")[1]
        my_dic[my_dic_index]=is_between_include(inputfile, sys.argv[i], float(threshold1), float(threshold2))

#####################
#####################


#If precise coord given.
if Coord_bool:
    while noval: #While no closest coord with valid values is found
        #Return closest coord avaible
        tree=spatial.KDTree(all_coord)
        closest_coord=(tree.query([(value_dim_lat,value_dim_lon)]))
        cc_index=closest_coord[1]

        closest_lat=float(all_coord[closest_coord[1]][0][0])
        closest_lon=float(all_coord[closest_coord[1]][0][1])

        #Get coord index into dictionary
        my_dic_index="list_index_dim"+str(name_dim_lat)
        my_dic[my_dic_index]=latitude.tolist().index(closest_lat)

        my_dic_index="list_index_dim"+str(name_dim_lon)
        my_dic[my_dic_index]=longitude.tolist().index(closest_lon)


        #All dictionary are saved in the string exec2 which will be exec(). Value got are in vec2
        exec2="vec2=inputfile.variables['"+var+"']["
        first=True
        for i in dim_names: #Every dim are in the right order
            if not first:
                exec2=exec2+","
            dimension_indexes="my_dic[\"list_index_dim"+i+"\"]" #new dim, custom name dic
            try:  #If some error or no specific user choices; every indexes are used for the selected dim.
                exec(dimension_indexes)
            except:
                dimension_indexes=":"
            exec2=exec2+dimension_indexes #Concatenate dim
            first=False #Not the first element now
        exec2=exec2+"]"
        #print exec2 #To check integrity of the string
        exec(exec2) #Execution, value are in vec2.
        #print vec2 #Get the value, standard output

        #Check integrity of vec2. We don't want  NA values
        i=0 
        #Check every value, if at least one non NA is found vec2 and the current closest coords are validated
        vecsize=vec2.size
        #print (str(vecsize))
        if vecsize>1:
            while i<vecsize:
                #print (str(vec2))
                if vec2[i]!="nan":
                    break
                else: 
                    i=i+1
        else:
            if vec2!="nan":
                break
            else: 
                i=i+1
   
        if i<vecsize: #There is at least 1 nonNA value
            noval=False
        else: #If only NA : pop the closest coord and search in the second closest coord in the next loop.
            all_coord=np.delete(all_coord,cc_index,0)


#Same as before, dictionary use in exec2. exec(exec2) give vec2 and the values wanted.
else:
    exec2="vec2=inputfile.variables['"+str(sys.argv[3])+"']["
    first=True
    for i in dim_names: #Respect order
        if not first:
            exec2=exec2+","
        dimension_indexes="my_dic[\"list_index_dim"+i+"\"]"
        try:  #Avoid error and exit
            exec(dimension_indexes)
        except:
            dimension_indexes=":"
        exec2=exec2+dimension_indexes
        first=False
    exec2=exec2+"]"
    exec(exec2)
   

########################
########################


#This part create the header of every value. 
#Values of every dim from the var is saved in a list : b[].
#All the lists b are saved in the unique list a[]
#All the combinations of the dim values inside a[] are the headers of the vec2 values 

#Also write dim_name into a file to make clear header.
fo=open("header_names",'w')

a=[]
for i in dim_names:
    try: #If it doesn't work here its because my_dic= : so there is no size. Except will direcly take size of the dim.
        size_dim=inputfile[i][my_dic['list_index_dim'+i]].size
    except:
        size_dim=inputfile[i].size 
        my_dic['list_index_dim'+i]=range(size_dim)

    #print (i,size_dim) #Standard msg
    b=[]
    #Check size is useful since b.append(inputfile[i][my_dic['list_index_dim'+i][0]])  won't work
    if size_dim>1:
        for s in range(0,size_dim):
            b.append(inputfile[i][my_dic['list_index_dim'+i][s]])
            #print (i,inputfile[i][my_dic['list_index_dim'+i][s]])
    else:
        b.append(inputfile[i][my_dic['list_index_dim'+i]])
        #print (i,inputfile[i][my_dic['list_index_dim'+i]])

    a.append(b) 
    fo.write(i+"\t")
if Coord_bool: 
    fo.write("input_lat\t"+"input_lon\t")
fo.write(var+"\n")
fo.close()


######################
######################


#Write header in file
fo=open("header",'w')
for combination in itertools.product(*a):
    if Coord_bool:
        fo.write(str(combination)+"_"+str(value_dim_lat)+"_"+str(value_dim_lon)+"\t")
    else:
        fo.write(str(combination)+"\t")
fo.write("\n")
fo.close()


#Write vec2 in a tabular formated file
fo=open("sortie.tabular",'w')
#print(str(vec2))
try:
    vec2.tofile(fo,sep="\t",format="%s")
except:
    vec3=np.ma.filled(vec2,np.nan)
    vec3.tofile(fo,sep="\t",format="%s")
fo.close()


######################
######################


#Final sweet msg
print (var+" values successffuly extracted from "+sys.argv[1]+" !")

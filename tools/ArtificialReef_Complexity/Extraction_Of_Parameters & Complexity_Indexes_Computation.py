#!/usr/bin/env python
# coding: utf-8

### Loading the libraries

import matplotlib.pyplot as plt
import os
import numpy as np
from numpy import savetxt
import pandas as pd
import trimesh
from scipy.spatial import ConvexHull
from scipy.stats import entropy
import math


### Path directory & folders

data_mesh_path = "./data_STL"
data_point_clouds_path = "./data_point_clouds"
data_normals_path = "./data_normals"
output_path = "./output"


# * Create a list of your mesh `list_mesh`

# In[ ]:


list_mesh = os.listdir(data_mesh_path)


## I. Extraction of the parameters of the 3D CAD models

### I.1 Area and Volume of Mesh and its ConvexHull


AreaVol_parameters = []


for i in range(len(list_mesh)):
  
  # Load mesh files
  print("--- {} processing {} ---".format(pd.Timestamp.now(), list_mesh[i]))
  mesh = trimesh.load(os.path.join(data_mesh_path, list_mesh[i]), updateNormals=True, readcolor=False,
                        clean=True, silent=False)

  ### COMPUTE THE CONVEXHULL 

  # Extract coordinate of vertices to compute the convexhull
  points = mesh.vertices
  hull = ConvexHull(points)

  # remove unused vertices from the convex hull
  used_vertices = np.unique(hull.simplices.flatten())
  vertices = points[used_vertices]
  faces = np.searchsorted(used_vertices, hull.simplices)
  convex_mesh = trimesh.Trimesh(vertices=vertices, faces=faces)
  convex_mesh.fix_normals() 
    
  ### COMPUTE AREA & VOLUME of the MESH & CONVEXHULL

  mesh_area = mesh.area 
  mesh_volume = mesh.volume
  convex_mesh_area = convex_mesh.area
  convex_mesh_volume = convex_mesh.volume

  ### APPEND `AreaVol_parameters` TO LIST
  file_names = [os.path.splitext(file)[0] for file in list_mesh]
  AreaVol_parameters.append((file_names[i], mesh_area, mesh_volume, 
                     convex_mesh_area, convex_mesh_volume))


# ### I.2 Extraction of Point clouds and normals from the Mesh


normals_parameters = []



for i in range(len(list_mesh)):
 print("--- {} processing {} ---".format(pd.Timestamp.now(), list_mesh[i]))
 mesh = trimesh.load(os.path.join(data_mesh_path, list_mesh[i]), updateNormals=True, readcolor=False,
                        clean=True, silent=False)

 
 #### POINT CLOUDS EXTRACTION

 density = 0.01  # points per square centimeter
 point_count = int(mesh.area * density)
 point_cloud, index = trimesh.sample.sample_surface(mesh, count=point_count)
 point_cloud = point_cloud.view(np.ndarray) 
 savetxt(os.path.join(data_point_clouds_path, file_names[i] + ".txt"), point_cloud, delimiter = ",")   

 
 #### NORMALS EXTRACTION

 ## Compute the normal and its amount
 x_y_z_cos = mesh.face_normals[index]
 normal = np.sum(x_y_z_cos, axis=1) 
 nb_normal = len(normal)

 # Summing up the different type of normal orientation & save this data for further indexes computation
 sum_different_normal = pd.DataFrame(normal).apply(pd.value_counts).fillna(0).T
 file_names = [os.path.splitext(file)[0] for file in list_mesh]
 sum_different_normal.to_csv(os.path.join(data_normals_path, file_names[i] + ".csv"), index=False)
    
 # Count of the different normal vectors 
 nb_different_normal = sum_different_normal.astype(bool).sum(axis=1)
 
 ### APPEND `normals_parameters` TO LIST
 file_names = [os.path.splitext(file)[0] for file in list_mesh]
 normals_parameters.append((file_names[i],
                    nb_normal, nb_different_normal.iloc[0]))




df_parameters = pd.concat([pd.DataFrame(AreaVol_parameters).iloc[:, 1:], pd.DataFrame(normals_parameters).iloc[:, 1:]], axis=1)



df_parameters.index = file_names
df_parameters.columns = ["mesh_area", "mesh_volume", 
                        "convex_mesh_area", "convex_mesh_volume",
                        "nb_normal","nb_different_normal"]



df_parameters



df_parameters.to_csv(os.path.join(output_path, "df_parameters.csv"))


# # II. Computation of geometrical and informational complexity indexes

##### Compute Packing and Convexity from the volume and area of the mesh and its convexhull

# In[ ]:


Pt = 1-(df_parameters['convex_mesh_area']/df_parameters['mesh_area'])
Pt = Pt.tolist() 

C = ((df_parameters['convex_mesh_volume']-df_parameters['mesh_volume'])/df_parameters['convex_mesh_volume'])
C = C.tolist()


# ## II.2 Informational complexity

# In this section:
# * computation of Orientation Richness (R): measure the proportion of the different orientation of the normals.
# * computation of Orientation Diversity (H<sub>t</sub>): measure the diversity of the orientation of the normals.
# * computation of Orientation Evenness (J): measure the evenness of the orientation of the normals.

# #### Orientation richness (R)

# In[ ]:


R = df_parameters['nb_different_normal']/df_parameters['nb_normal']
R = R.tolist()


# #### Orientation diversity (H<sub>t</sub>) & Orientation evenness (J)

list_normals = os.listdir(data_normals_path)
Ht = []
J = []


# ##### Launch the loop

# In[ ]:


for i in range(len(list_normals)):
  
 print("--- {} processing {} ---".format(pd.Timestamp.now(), list_normals[i]))

 normals = pd.read_csv(os.path.join(data_normals_path, list_normals[i]))
    
 Shannon = float(np.asarray(entropy(normals,axis=1)))

 Pielou = Shannon / np.log(len(normals.axes[1]))
    
 Shannon_log = np.log(1+Shannon)
    
 Ht.append(Shannon_log)
 J.append(Pielou)


# ## III. Save the data computed


# In[ ]:


df_complexity_indexes = pd.DataFrame({'R': R, 'Ht': Ht, 'J': J, 'Pt': Pt, 'C': C}).round(3)
df_complexity_indexes.index = file_names


# In[ ]:


df_complexity_indexes


# In[ ]:


df_complexity_indexes.to_csv(os.path.join(output_path,"df_complexity_indexes.csv"), index=True)


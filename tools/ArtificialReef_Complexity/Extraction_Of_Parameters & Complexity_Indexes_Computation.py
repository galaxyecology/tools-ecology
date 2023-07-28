#!/usr/bin/env python
# coding: utf-8

# # **Extraction Of Parameters & Complexity Indexes Computation**

# This script aims at extracting parmaters and elements from 3D CAD models to compute complexity indexes. It is based on **STL** files. 
# 
# **Section I : Extraction of the parameters of the 3D CAD models**
# * Computation of the convexhull of the mesh, and their respective area and volume
# * Extraction of point clouds and normals to be saved as txt files
# * Computation of the sum of normals and the number of different normals for each 3D CAD models
# 
# **Section II : Computation of geometrical and informational complexity indexes**
# * Computation of Packing (P<sub>t</sub>) and Convexity (C) (fractal dimension (D<sub>t</sub>) is computed on R, script available at: https://github.com/ELI-RIERA/ArtificialReef_Complexity)
# * Computation of Richness (R), Diversity (H<sub>t</sub>) and Evenness (J) on normals 
# 
# For more details refer to the paper available at:

# ### Loading the libraries

# In[ ]:

import matplotlib.pyplot as plt
import os
import numpy as np
from numpy import savetxt
import pandas as pd
import trimesh
from scipy.spatial import ConvexHull
from scipy.stats import entropy
import math


# ### Download STL file from Zenodo
# 
# As an example to test the script, 3D CAD mesh models (STL files) are available on Zenodo:
# 
# [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8048071.svg)](https://doi.org/10.5281/zenodo.8048071)
# 
# Otherwise use your own mesh at the format STL 

# ### Path directory & folders
# 
# Mesh (STL files):
# * Create a folder where you store your mesh (STL files) `data_mesh` or the one that you downloaded from Zenodo
# * Determine the path of your mesh `data_mesh_path`
# 
# Point clouds and normals (txt files):
# * Create two folders where you'll store the point clouds `data_point_clouds` and the normals `data_normals`
# * Determine the path directory of these folders
# 
# outputs:
# * Create a folder `output` where you'll store the output from the difference computation process
# * Determine the path of the output folder `output_path`

# In[ ]:


data_mesh_path = "./data_STL"
data_point_clouds_path = "./data_point_clouds"
data_normals_path = "./data_normals"
output_path = "./output"


# * Create a list of your mesh `list_mesh`

# In[ ]:


list_mesh = os.listdir(data_mesh_path)


# ## I. Extraction of the parameters of the 3D CAD models

# ### I.1 Area and Volume of Mesh and its ConvexHull

# In this section: 
# * Computation of the convexhull of each mesh of 3D CAD models. The convexhull of a shape is the smallest convex set that contains it.
# * Computation of the area and volume of all mesh and its respective convexhull that you save in a dataframe called `AreaVol_parameters`
# 

# ##### Prepare the loop for the computation 
# 
# * Create an empty object `AreaVol_parameters` where area and volume parameters will be stored durign the loop

# In[ ]:


AreaVol_parameters = []


# ##### Launch the loop

# In[ ]:


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

# In this section: 
# * extraction of point clouds from the mesh that you save as `.txt` file for fractal dimension computation
# * extraction of normals associated to the point clouds 
# * computation the number of normals and the number of different normals that you save in a dataframe `normals_parameters`
# 

# ##### Prepare the loop for the computation
# 
# * Create an empty object `normals_parameters` where normals will be stored during the loop

# In[ ]:


normals_parameters = []


# ##### Launch the loop

# In[ ]:


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


# ### I.3. Save the paramaters data
# 
# * Concatenate `normals_parameters` and `AreaVol_parameters` into a panda dataframe `df_parameters`
# * Rename the rows and columns
# * Verify your dataframe
# * Save it to the `output_path`

# In[ ]:


df_parameters = pd.concat([pd.DataFrame(AreaVol_parameters).iloc[:, 1:], pd.DataFrame(normals_parameters).iloc[:, 1:]], axis=1)


# In[ ]:


df_parameters.index = file_names
df_parameters.columns = ["mesh_area", "mesh_volume", 
                        "convex_mesh_area", "convex_mesh_volume",
                        "nb_normal","nb_different_normal"]


# In[ ]:


df_parameters


# In[ ]:


df_parameters.to_csv(os.path.join(output_path, "df_parameters.csv"))


# # II. Computation of geometrical and informational complexity indexes

# ## II.1 Geometrical complexity : Packing (P) & Convexity (C)

# In this section:
# * computation of Packing (P<sub>t</sub>): measure of the degree of space between different parts of an object.
# 
# \begin{equation*}
# {P}=\frac{{A}_{ar}}{{A}_{ch}} \Rightarrow {P}_{t}=1-\frac{{A}_{ch}}{{A}_{ar}} 
# \end{equation*}  
# 
# * computation of Convexity (C): measure of the degree of space available between different parts of an object.
# 
# \begin{equation*}
# {C}=\frac{{V}_{av}}{{A}_{ch}}
# \end{equation*} 
# 
# * (computation of Fractal Dimension (D<sub>t</sub>): R script available on github (https://github.com/ELI-RIERA/ArtificialReef_Complexity)
# 
# \begin{equation*}
# D=\lim_{\varepsilon\to0}\frac{\log{N}({\varepsilon})}{\log{\frac{1}{\varepsilon}}} \Rightarrow D_{t} = 1-(3-D)
# \end{equation*}
# 

# ##### Compute Packing and Convexity from the volume and area of the mesh and its convexhull

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

# ##### Prepare the loop for the computation of H<sub>t</sub> and J
# 
# * Create a list from the normals files computed previously
# * Create an empty objects **H<sub>t</sub>** & **J** where indexes will be stored durign the loop
# 
# 
# \begin{equation*}
# H=-\sum_{i=1}^{S} {p}_{i} . log\left( {p}_{i} \right)\Rightarrow {H}_{t}=log\left( 1+H \right)
# \end{equation*}
# 
# \begin{equation*}
# J=\frac{H}{\log{\left( S \right)}}
# \end{equation*}
# 

# In[ ]:


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
# 
# * Concatenate your indexes **R, H<sub>t</sub>, J, P<sub>t</sub>, C** into a panda dataframe
# * Rename the rows and columns
# * Verify your dataframe
# * Save it to the output path

# In[ ]:


df_complexity_indexes = pd.DataFrame({'R': R, 'Ht': Ht, 'J': J, 'Pt': Pt, 'C': C}).round(3)
df_complexity_indexes.index = file_names


# In[ ]:


df_complexity_indexes


# In[ ]:


df_complexity_indexes.to_csv(os.path.join(output_path,"df_complexity_indexes.csv"), index=True)


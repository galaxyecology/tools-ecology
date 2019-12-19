# Netcdf reading and extraction
## Netcdf info
The first tool "netcdf info" use the C library netcdf (v4.5.0) to open and get general informations, variables names and attributes.
Variables that can be extracted and dimensions availables are printed in tabular files.

The tool use the bioconda tool netcdf-metadata-info : https://github.com/Alanamosse/Netcdf-Metadata-Info

The tool also print a general information file. It's the result of the

```
ncdump -h inputfile
```
command.


## Netcdf read
The second tool "netcdf read" use the Python module Netcdf4 (1.3.1) which is an interface of the C library.

The xml netcdf_read allows to choose a variable to extract and to add filter on dimensions.
The option "Search values for custom coordinates" use the scipy spatial function to get the closest coordinates with non-NA values.

User can either give coordinates by standard input, or the tool can use a standardised tabular file with multiple coordinates. 
The output is one tabular per coordinate with the variable and its dimensions values (eg : latitude, longitude, time...).

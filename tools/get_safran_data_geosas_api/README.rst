Safran Data Extraction Scripts
==============================

Overview
--------

This repository contains scripts for extracting SAFRAN meteorological data via the GEOSAS API.

Scripts
-------

``get_safran_data_geosas_api_MG.xml``
  XML configuration file that defines parameters and requests for SAFRAN data extraction
  through the GEOSAS API.

  **Purpose:** Configure API connection and data query parameters for SAFRAN datasets.

``get_safran_data_geosas_api_MG.R``
  R script that retrieves SAFRAN meteorological data using the GEOSAS API.

  **Purpose:** Execute data extraction and process SAFRAN climate/weather variables.

Requirements
------------

- R environment with appropriate packages
- GEOSAS API access
- Network access to GEOSAS services

Usage
-----

1. Configure parameters in the Galaxy tool interface.
2. Select the target sites and date range.
3. Run the tool to extract SAFRAN climate data.

Author
------

M. Guimont
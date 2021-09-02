# HI_Model

Input data, R scripts, and manuscript files for LUCAS model projections of ecosystem carbon balance in the Hawaiian Islands under different scenarios of climate and land use change. 

LUCAS model output data and metadata are freely available in machine-readable format from the USGS ScienceBase repository: https://doi.org/10.5066/P9AWLFKZ

Manuscript accepted for publication in *Environmental Research Letters* on 2021-09-02, available at https://doi.org/10.1088/1748-9326/ac2347

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5198072.svg)](https://doi.org/10.5281/zenodo.5198072)

## Directory Structure

* ___Carbon_IBIS___ - IBIS DGVM output data and R code used to create a State Attribute Values table (carbon stocks by Moisture Zone and State Class) and a Flow Pathways table (carbon fluxes to and from stocks) using IBIS data as input. 

* ___Climate___ - raster TIFF files of current climate and projected future climate (mid- and end of century) based on statistically downscaled CMIP5 GCM data. 

* ___Fire___ - a spatial database of fire occurence and fire extent in shapefile format for the years 1999-2019, as well as R code used to calculate annual burned area by island, ignition probabilities, and fire size distribution. 

* ___FlowMultipliers___ - R code to calculate growth multipliers based on projected climate change using an empirical NPP model, decomposition multipliers based on projected climate change using a Q<sub>10</sub> temperature coefficient, and growth multipliers based on projected changes in atmospheric CO<sub>2</sub> concentration using different levels of a CO<sub>2</sub> fertilization effect. 

* ___InputData___ - input data used to parameterize the model, including Flow Pathways, State Attribute Values, Flow Multipliers, Transition Targets, Transition Pathways, and Spatial Multipliers for growth, fire, and transitions. 

* ___Manuscript___ - R code to create figures, R Markdown code for manuscript files and journal correspondence, and a bibTeX file for references. 

* ___Transitions___ - historical land cover change data, population projections, and R code used to create Transition Pathways and Transition Targets for each of the two land-use change scenarios. 



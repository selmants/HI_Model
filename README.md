# HI_Model

Input data, R scripts, and manuscript files for LUCAS model projections of ecosystem carbon balance in the Hawaiian Islands under different scenarios of climate and land use change. 

LUCAS model summarized output data is available at https://doi.org/10.5066/P9AWLFKZ.
 

The manuscript was submitted for publication in the IOP journal *Environmental Research Letters*.  

## Directory Structure

* __CarbonStockFlow_IBIS__ contains IBIS DGVM output data and R code used to create State Attribute Values table (carbon stocks by Moisture Zone and State Class) and Flow Pathways table (carbon fluxes to and from stocks) using IBIS data as input. 

* __Climate__ contains raster TIFF files of current climate and projected future climate (mid- and end of century) based on statistically downscaled CMIP5 GCM data. 

* __Fire__ contains a spatial database of fire occurence and fire extent in shapefile format for the years 1999-2018, as well as R code used to calculate annual burned area by island, ignition probabilities, and fire size distribution. 

* __FlowMultipliers__ contains R code to calculate growth multipliers based on projected changes in temperature and rainfall, decomposition multipliers based on projected changes in temperature temperature using a Q10 function, and growth multipliers based on projected changes in atmospheric CO<sub>2</sub> concentration using different levels of a CO<sub>2</sub> fertilization effect. 

* __Transitions__ contains R code and data used to create Transition Pathways and Transition Targets for each land-use change scenario. 

* __InputData__ contains input data used to parameterize the model, including Flow Pathways, State Attribute Values, Flow Multipliers, Transition Targets, Transition Pathways, and Spatial Multipliers for growth, fire, and transitions. 

* __Manuscript__ contains R code used to create figures and the R Markdown manuscript file. 
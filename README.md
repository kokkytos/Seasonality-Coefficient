# VIIRS Night Lights Seasonality Coefficient

> **Project name**: "Estimating Seasonal Population via Night-Time Satellite Images" (SEPT). 

> A project funded by [Greek General secretariat and Technology & Hellenic Foundation For Research and Innovation](http://www.gsrt.gr/).

> [Spatial Analysis, GIS and Thematic Mapping Laboratory
Department of Planning and Regional Development
University of Thessaly, School of Engineering](http://www.gislab.gr/)


The current R project includes the code for calculating the Seasonality Coefficient based on night lights as described in Stathakis & Baltas (2018) <sup>[1](#myfootnote1)</sup>.

The calculation of Seasonality Coefficient for [NUTS 3](https://ec.europa.eu/eurostat/cache/GISCO/distribution/v2/nuts/download/ref-nuts-2016-01m.shp.zip) and [LAU](https://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/LAU-2018-01M-SH.zip) is provided.

The results are exported to the *output* directory .


Snpp-VIIRS monthly night lights <sup>[2](#myfootnote2)</sup> are used for 2014, 2015, 2016. 
Preprocessing has been done on the basis of the DMSP / OLS night lights and the methodology described in the relevant repository  [repository](https://github.com/kokkytos/viirs-global-mosaic).

The complete environment in which the current R code was executed is described in the file [sessionInfo.txt](sessionInfo.txt) 


<a name="myfootnote1"><sup>[1]</sup></a>  Stathakis, D., & Baltas, P. (2018). Seasonal population estimates based on night-time lights.
Computers, Environment and Urban Systems, 68, 133–141. 

<a name="myfootnote2"><sup>[2]</sup></a>  NOAA-National Geophysical Data Center, Version 1 VIIRS Day/Night Band Nighttime Lights, Earth Observation Group, NOAA/NCEI. URL https://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html (accessed 05.05.2019).


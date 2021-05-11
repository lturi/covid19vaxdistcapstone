# covid19vaxdistcapstone

This is the data cleaning repository for M. Eng Health Systems Engineering project Visualizing Equity in COVID-19 Vaccine Distribution by Leigh Friedman, Neha Mandhyani, and Liz Turi

# Project folders

* data/ -- this is all of the raw data collected for the project, and script output, files in here are static files (census for all US, census for just PA, and toxpi model definition headers)
* data/[dated directories] - raw data pulled from data.pa.gov
* data/output -- data prepared for ArcGIS import
* data/output/[dated directories] -- toxpi images charts from R
* R_scripts/ -- data cleansing scripts 

# Tools used

* -QGIS - for GIS modeling - https://qgis.org/en/site/forusers/download.html-
* Toxpi - to generate a variant on the Pandemic Vulnerability Index http://toxpi.org/
* ArcGIS 

# Pandemic Vulnterability Index

We use the methods and descriptions as defined here: https://covid19pvi.niehs.nih.gov/

* In our repo, we add logic to include Hispanic census data to the Population Demographics category based on NIEHS documentation and other research indicating Hispanic ethnicity is an additional risk factor. 

# Capstone project

Our project looks at developing a dashboard that provides intersectional insight into COVID-19 vulnerability factors and vaccination rates of specific populations noted to be considered highly vulnerable.

# Data sources

All census data are sourced from https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-detail.html
All vaccination rate data are sourced from https://data.pa.gov , specific sources are documented in the relevant R script.

# FAQ

* Why does our PVI map vary from the CDC/NIEHS map?
** As noted above, the NIEHS model does not include the Hispanic population as a group under Population Demographics. We felt that it was important to include 
for the purposes of our work given the nature of COVID risk posed to Hispanic communities, and to help policy makers visualize how at risk populations affect 
overall vulnerability. (For an example of differences in case rates between vulnerable groups in MA, https://elt416.wixsite.com/qpiportfolio/statistical-process-control and  https://elt416.wixsite.com/qpiportfolio/pophealth)

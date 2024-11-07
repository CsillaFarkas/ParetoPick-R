ParetoPick-R is part of the post processing in the [OPTAIN Project](https://www.optain.eu/). It shall facilitate the analysis of the Pareto front across objectives and support decision making for measure implementation.
It provides a dashboard for the user to supply their own data, visualise it and alter a range of parameters. 
The code allows the user to select variables to be analysed in a correlation analysis and a cluster algorithm. 

* Variables considered by the cluster algorithm, the first 3 are produced seperately for each measure (in a call to convert_optain)
  1. **share_con** - ratio between area covered by measure and area available for measure implementation (per measure type) 
  2. **channel_frac** - fraction of water from measure hru draining straight into the channel (per measure type) 
  3. **moran** - Moran's I (per measure type) 
  4. **linE** - ratio of structural to management options 
  5. **lu_share** - share of area used for "land use" measures (buffer, grassslope and hedge) in area available for measure implementation


The cluster algorithm relies on Principal Component Analysis (PCA) and kmeans/kmedoid. While the variables that can be considered in 
the algorithm are fixed, several settings such as outlier treatment and the number of tested principal components, can be set by the user. 
ParetoPick-R also integrates an Analytical Hierarchy Process (AHP) allowing the user to determine weights for the objectives based on pair-wise comparisons. The results of the clustering and the AHP can be combined and the app provides a range of methods for visualising the results.

Please note that the Python code used in this app is part of another project and was written by [S. White](https://github.com/SydneyEWhite).

# Folder Structure
the project consists of six folders

```
.
├── app
│   ├── ui.R
│   ├── server.R
│   ├── global.R
│   └── convert_optain.R
├── python_files
│   ├── kmeans.exe
│   ├── kmedoid.exe
│   ├── correlation_matrix.exe
│   └── _internal
├── input
│   └── config.ini
├── data
├── data for container
│   └── config.ini (for hard reset)
├── output
```
### Files created and used in the process
(stored in input folder)
* var_corr_par.csv (created in convert_optain.R, contains all variables considered in the clustering)
* hru_in_optima.RDS (created in convert_optain.R based on measure_location.csv, connection between activated HRUs and optima)
* nswrm_priorities.csv (created in covert_optain.R based on measure_location.csv)
* object_names.RDS
* pca_content.RDS
* all_var.RDS
* units.RDS


### Required input files 
(app pulls these into the data folder)
1. pareto_genomes.txt
2. pareto_fitness.txt
3. hru.con
4. measure_location.csv
5. hru shapefile consisting of: hru.shp, hru.dbf, hru.prj, hru.shx
6. basin shapefile consisting of: basin.shp, basin.dbf, basin.prj, basin.shx
7. sq_fitness.txt
8. rout_unit.con

### Elements touched in config.ini (adapt when final version of Python project available)
* col_correlation_matrix
* var1 to var4
* var_1_label to var_4_label
* qualitative_clustering_columns
* fixed_clusters_boolean
* fixed_clusters
* min_clusters & max_clusters
* handle_outliers_boolean
* deviations_min & deviations_max
* count_min & count_max
* outlier_to_cluster_ratio
* min_components & max_components

# Assumptions

* General to do and open questions
  * as "land use" measures currently only hedge, buffer and grassslope considered - if more needed global.R, functions.R (write_corr) and convert_optain require adapting
  * new nswrm_priorities() function currently considers: pond, constr_wetland, wetland, hedge, buffer, grassslope, lowtillcc, lowtill, droughtplt
  * do the current default settings produce reasonable cluster outputs across all catchments? the default might need outlier testing 
  * stratified variables such as lowflow in the Schoeps do not work in the tool, the sliders cannot be moved
  * users should not produce optimisation outputs with values below 0 and if possible no values smaller than 1, the app allows a rescale to roughly balance the values between 1 and 100 but it is turned off
  * cannot take percentage of 0 so those changes are not visible in Visualisation tab
  * share to GitLab
  * write a comprehensive Readme walking the user through the use of the tool including an explanation of the data format input requirements


* AHP
  * the initial state of the pairwise comparison as "Equal" amplifies the mathematical definition of inconsistency, therefore only when at least three sliders are NOT set to "Equal" is inconsistency considered at all
  * normalising the dataframe prior to weighting relies on scaling to between 0 and 1

# To do
## Content
* Data Prep tab:
  * explain the altered scale and minus removal
  * allow user to change labels reflecting for altered scale

* Visualising tab:
  * add one plot of map with measure frequency, percentage of implemented across current selection

* Configure tab
  * outlier testing in default run?
  * check for missing files
  * automate widest range function to config writing for it to work in python
   
* AHP and Cluster Analysis tabs
  * four checkboxes with default selection being pareto front, show axis stuff only when pareto front is selected
  * better graphical representation of decison space across solutions:
  * plot PCA variables against objectives, as one of four checkboxes, show new set of drop down menus when selected
 

## Workflow
* General
  * convert_optain requires some failsafe controls for empty values

* Visualisation tab
  * catch empty selectiona and replace "replacement has lenght zero" with "no optima fulfill these conditions", fix reload when selection changes
  * expand scatterplot to more frame space and capture longer numbers
 
Correlation tab
  * only strike through the variable that has been removed
  * new button to jump to correlation tab

* Analysis tab
  * HTML download for individual measure implementation maps, not possible across all

* AHP
  * add checkbox to allow showing all cards at once
  * show only one card at once, remove all others when one is clicked
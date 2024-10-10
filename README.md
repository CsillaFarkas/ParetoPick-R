ParetoPick-R is part of the post processing in the [OPTAIN Project](https://www.optain.eu/). It shall facilitate the analysis of the Pareto front across objectives and support decision making for measure implementation.
It provides a dashboard for the user to supply their own data, visualise it and alter a range of parameters. 
The code allows the user to select variables to be analysed in a correlation analysis and a cluster algorithm. 

* Variables considered by the cluster algorithm, the first 3 are produced seperately for each measure (in a call to convert_optain)
  1. **share_tot** - catchment area covered by measure (per measure type) 
  2. **share_con** - ratio between area covered by measure and area available for measure implementation (per measure type) 
  3. **channel_frac** - fraction of water from measure hru draining straight into the channel (per measure type) 
  4. **moran** - Moran's I 
  5. **linE** - ratio of structural to management options 


The cluster algorithm relies on Principal Component Analysis (PCA) and kmeans/kmedoid. While the variables that can be considered in 
the algorithm are fixed, several settings such as outlier treatment and the number of tested principal components, can be set by the user. 
ParetoPick-R also integrates an Analytical Hierarchy Process (AHP) allowing the user to determine weights for the objectives based on pair-wise comparisons. The results of the clustering and the AHP can be combined and the app provides a range of methods for visualising the results.

Please note that the Python code used in this app is part of another project and was written by [S. White](https://github.com/SydneyEWhite).

# Folder Structure
the project consists of six folders:
1. app folder: contains all required scripts (ui.R, global.R, functions.R, server.R, convert_optain.R)
2. output folder (empty in beginning)
3. data folder (empty in beginning): folder the user input is written to
4. input folder (containing nswrm_priorities.csv and config.ini): folder the datafiles are written/read to/from 
5. data for container (containing config.ini and nswrm_priorities.csv for hard reset)
6. python_files: contains python executables 

### Files provided in the package
* config.ini 
* nswrm_priorities.csv 

### Files created and used in the process
(stored in input folder)
* var_corr_par.csv (created in convert_optain.R, contains all variables considered in the clustering)
* hru_in_optima.RDS (created in convert_optain.R based on measure_location.csv, connection between activated HRUs and optima)
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
* General
  * do the current default settings produce reasonable cluster outputs across all catchments?
  * dynamic priority allocaiton according to measure_location$nswrm - see page 35 in deliverable 5.1 (esp. pond vs wetland)
  * users should not produce optimisation outputs with values below 0, add a check and change pareto_fitness to all above 0, would mean overhaul of range_controlled()
  * remove minus sign from sliders, where needed add imagery to explain good and bad

* AHP
  * the initial state of the pairwise comparison as "Equal" amplifies the mathematical definition of inconsistency, therefore only when at least three sliders are NOT set to "Equal" is inconsistency considered at all
  * normalising the dataframe prior to weighting relies on scaling to between 0 and 1. Should the individual solutions be spaced equally to get more effect through weighting?

# To do
## Content
* Visualising tab:
  * right color status quo
  * add new sliders for measures - along share of area possible for this measure (similar to share_con), this probably requires a spinner in beginning
  * catch R errors triggered by empty data frames better

* Configure tab
  * finish option for limiting range and check for missing files
 
* Correlation tab
  * output largest accepted correlation in bold over table/could also be part of default setting
  * (python: deviations_step is currently the default value) 

* Analysis tab
  * this tab requires another name

  
* AHP and Analysis tabs
  * option for reverse plotting to improve clarity in the presence of negative scales (if possible only if X and Y Axis are <0)
  * better graphical representation of decison space across solutions:
    * share_con/linE - on y axis
    * frequency maps - produce during python call or based on button with Micha's R script and put in output
    * barplot of implemented measures per optimum
    * plot PCA variables against objectives
    * add line plot for solutions to improve tradeoff representation
    * find a way to select measures and analyse their density/combinations/areas in field
    * representation of "most important" measures that are part of all pareto-optimal solutions
    * anonymous catchments something something



## Workflow
* General
  * declutter app by removal of items that can instead be hovered (what could be moved there? --- unit, especially also in visualisation tab)
  * AHP needs another visualisation - each pair in different tab - only show results if inconsistency acceptable
  * convert_optain requires some failsafe controls for empty values

* Visualising tab:
  * status quo plotted in the right color
  * tables are not super clear - add unit and show when hovered
 
* Analysis tab
  * HTML download for measure implementation maps
  * no replotting when table is touched, that is so annoying

* AHP
  * more reasonable representation of variables across different scales - we will need to test that with different datasets (currently just multiplied by 1000 if <0.005)
  * keep ticks and remove labels on top (probably obsolete with restructure)
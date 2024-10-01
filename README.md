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
  * a restructure of the correlation/clustering approach (see below) would require default settings that can produce reasonable cluster outputs across all catchments

* AHP
  * the initial state of the pairwise comparison as "Equal" amplifies the mathematical definition of inconsistency, therefore only when at least three sliders are NOT set to "Equal" is inconsistency considered at all
  * normalising the dataframe prior to weighting it relies on scaling to between 0 and 1 using the old dataset as anchor

# To do
## Content
* Visualising tab:
  * add percentage change from full range to table (in color)
  * find a way to visualise differences in objective spread
  * better labels for parallel axis plot (replace "worst", "medium" etc.)

* Correlation tab
  * output largest accepted correlation in bold over table/could also be part of default setting
  * deviations_step is currently the default value 

* Cluster tab
  * add possibility to limit range before clustering (where?), this is also useful for new default clustering

* Analysis tab
  * this tab requires another name
  * location of zoom in basin

* AHP and Analysis tabs
  * sliders for measures (unclear how best represented though)
  * option for reverse plotting to improve clarity in the presence of negative scales
  * better graphical representation of decison space across solutions:
    * frequency maps - produce during python call or based on button with Micha's R script and put in output
    * barplot of implemented measures per optimum
    * plot PCA variables against objectives
    * add line plot for solutions to improve tradeoff representation
    * find a way to select measures and analyse their density/combinations/areas in field
    * is there a way to introduce sliders for measures like Dennis suggested?


## Workflow
* General
  * full list of measures across all catchments --> nswrm_priorities.csv, this should be available through SWATmeasR.R
  * restructure - two-way representation of one simple and one complex workflow of correlation/clustering
    * hide both tabs and allow user to skip detailed steps - run automated removal of correlated variables (>0.7) and run cluster with default settings
    * cluster tab requires a lot of clicking around but those users who want to try it will figure it out
  * declutter app by removal of items that can instead be hovered (what could be moved there? --- unit)


* Visualising tab:
  * status quo plotted in the right color
  * tables are not super clear - add unit and show when hovered

* Analysis tab
  * HTML download for measure implementation maps
  * no replotting when table is touched, that is so annoying

* AHP
  * more reasonable representation of variables across different scales - we will need to test that with different datasets (currently just multiplied by 1000 if <0.005)
  * can some of the crowded labels only be shown when hovered?
  * add the option for rounding labels on sliders
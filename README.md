ParetoPick-R is part of the post processing of the [OPTAIN Project](https://www.optain.eu/). It shall facilitate the analysis of the Pareto front across objectives and support decision making for measure implementation.
It provides a dashboard for the user to supply their own data, visualise it and alter a range of parameters. 
The code allows the user to select variables to be analysed in a correlation analysis and a cluster algorithm. 

* Variables considered by the cluster algorithm (produced in a call to convert_optain)
  1. catchment area covered by measure (distinguished by measure type) **share_tot**
  2. ratio between area covered by measure and area available for measure implementation (distinguished by measure type) **share_con**
  3. fraction of water from measure hru draining straight into the channel (distinguished by measure type) **channel_frac**
  4. Moran's I **moran**
  5. ratio of structural to management options **linE**


The cluster algorithm relies on Principal Component Analysis (PCA) and kmeans/kmedoid. While the variables that can be considered in 
the algorithm are fixed, several settings such as outlier treatment and the number of tested principal components, can be set by the user. 
ParetoPick-R also integrates an Analytical Hierarchy Process (AHP) allowing the user to determine weights for the objectives based on pair-wise comparisons. The results of the clustering and the AHP can be combined and the app provides a range of methods for visualising the results.


# Folder Structure
* the project consists of six folders:
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
* var_corr_par.csv (created in convert_optain.R, contains all variables considered in the clustering)
* hru_in_optima.RDS (created in convert_optain.R based on measure_location.csv, connection between activated HRUs and optima)
* object_names.RDS
* pca_content.RDS
* all_var.RDS
* units.RDS

### Input through the user 
(app pulls these into the data folder)
* pareto_genomes.txt
* pareto_fitness.txt
* hru.con
* measure_location.csv
* hru shapefile consisting of: hru.shp, hru.dbf, hru.prj, hru.shx
* sq_fitness.txt
* rout_unit.con

## Elements touched in config.ini (adapt when final version of Python project available)
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

# Unclear
* AHP
  * the initial state of the pairwise comparison as "Equal" amplifies the mathematical definition of inconsistency, therefore only when at least three sliders are NOT set to "Equal" is inconsistency considered at all
  * unclear how to best normalise data, currently scaled to between 0 and 1 using the old dataset as anchor

# Missing/Nice to have
* sliders with actual values instead of scaled values?
* saving the WHOLE scatter plot, not only the last_plot() device
* cluster tab sometimes requires a lot of clicking around but users will figure it out

* Play around tab
  * status quo plotted in the right color when sliders are moved

* Klaipeda:
  * full list of nswrm implemented across all catchments to properly assign priorities incl. info on weather management or structural
  * suggestions for pca variables using SWAT+ outputs/inputs
 
* AHP tab: 
  * map with measure implementation of selected optimum
  * more reasonable rendering of variable and different scales (currently just multiplied by 1000 if <0.005)

* Analysis tab: 
  * status message so user knows the plot is being rendered!!
  * location of zoom in basin
  
* Correlation Analysis:
  * output largest accepted correlation, maybe in bold over the table
  * add a description of the variables in the "selected variables" table...
  * deviations_step is currently the default value 
  * explain the effects of high correlation



ParetoPick-R is part of the final analyses of the [OPTAIN Project](https://www.optain.eu/). It shall facilitate the analysis of the Pareto front across objectives and support decision making for measure implementation.
It provides a dashboard for the user to supply their own data, visualise it and alter a range of parameters. The code allows the user to select variables to be analysed in a correlation analysis and a PCA. 
It then performs both and provides the relevant statistics, plots and analyses.

# File Structure
* the project consists of four folders:
1. app folder: contains all required scripts (ui.R, global.R, functions.R, server.R, convert_optain.R)
2. output folder (empty in beginning)
3. data folder (empty in beginning): folder the user input is written to
4. input folder (containing nswrm_priorities.csv and config.ini): folder the datafiles are written/read to/from 
5. data for container (containing config.ini and nswrm_priorities.csv for hard reset)

## Files provided in the package
* config.ini 
* nswrm_priorities.csv 


### Files created and used in the process
* var_corr_par.csv (created in convert_optain.R, contains all variables considered in the clustering)
* hru_in_optima.RDS (created in convert_optain.R based on measure_location.csv, connection between activated HRUs and optima)
* object_names.RDS
* pca_content.RDS
* all_var.RDS
* units.RDS

## Input through the user 
(app pulls these into the data folder)
* pareto_genomes.txt
* pareto_fitness.txt
* hru.con
* measure_location.csv
* hru shapefile consisting of: hru.shp, hru.dbf, hru.prj, hru.shx

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
  * unclear how to best normalise data, difficult particularly for when objectives are scaled very differently (log, z scores etc do not yield satisfying variability across the set)

# Missing/Nice to have
* sliders with actual values instead of scaled values?
* implement other variables to consider in the PCA! using SWAT+ inputs

* Klaipeda:
  * full list of nswrm implemented across all catchments to properly assign priorities incl. info on weather management or structural

* AHP tab: 
  * map with measure implementation of selected optimum
  * more reasonable rendering of variable and different scales (currently just multiplied by 1000 if <0.005)

* Analysis tab: 
  * status message so user knows the plot is being rendered!!

* Correlation Analysis:
  * output largest accepted correlation, maybe in bold over the table
  * deviations_step is currently the default value 
  * explain the effects of high correlation



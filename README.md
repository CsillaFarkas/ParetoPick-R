This code is part of the final analyses of the [OPTAIN Project](https://www.optain.eu/). It shall facilitate the analysis of the Pareto front across objectives and support decision making for measure implementation.
It provides a dashboard for the user to supply their own data, visualise it and alter a range of parameters. The code allows the user to select variables to be analysed in a correlation analysis and a PCA. 
It then performs both and provides the relevant statistics, plots and analyses.
## File Structure
* the project consists of five folders:
1. app contains all required functions
2. output folder (empty in beginning)
3. data folder (empty in beginning): folder the user input is written to
4. input folder (containing nswrm_priorities.csv and config.ini): folder the datafiles written/read are to/from 
## Required files/input through the user
* pareto_genomes.txt
* pareto_fitness.txt
* hru.con
* measure_location.csv
* hru shapefile consisting of: hru.shp, hru.dbf, hru.prj, hru.shx
* config.ini (included in package, please download)
* nswrm_priorities.csv (included in package, please download)
 
### Files created and used in the process
* var_corr_par.csv
* hru_in_optima.RDS
* object_names.RDS
* pca_content.RDS
* all_var.RDS
## Missing/Nice to have
* Plot: background, quicker through all in functions, buffer for visibility, if quicker we could do several next to each other
* Front page explaining the process: Data Preparation only has to run once etc.
* full list of nswrm implemented across all catchments to properly assign priorities
* Bigger collection of management and structural nswrm implemented in OPTAIN
* Correlation Analysis: output largest accepted correlation, maybe in bold in table
* deviations_step is currently the default value 
* explain the effects of high correlation
* check if the selection of optima in Sydney's output is ordered the same way

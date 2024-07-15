# pareto_optain
This software is part of the final analyses of the [OPTAIN Project](https://www.optain.eu/). It shall facilitate the analysis of the Pareto front across objectives and support decision making for measure implementation.
It provides a dashboard for the user to supply their own data, visualise it and alter a range of parameters. The software allows the user to select variables to be analysed in a correlation analysis and a PCA. 
It then performs both and provides the relevant statistics, plots and analyses.




## Required files/input through the user
* pareto_genomes.txt
* pareto_fitness.txt
* hru.con
* measure_location.csv
* config.ini (included in package)
* nswrm_priorities.csv (included in package)

## Missing/Nice to have
* full list of nswrm implemented across all catchments to properly assign priorities
* PCA - number of fixed clusters adapted to number of variables
* Bigger collection of management and structural nswrm implemented in OPTAIN
* Correlation Analysis: output largest accepted correlation, maybe in bold in table

# pareto_optain
This dashboard first performs a correlation analysis and allows the user to select variables to be analysed in a PCA. It then performs a PCA.
## Required files
* pareto_genomes.txt
* pareto_fitness.txt
* hru.con
* measure_location.csv

## Missing/Nice to have
* Data Preparation Tab - get user to input the order of measure implementation (=priority)
* Dynamic reading of .csv file (once it is produced in the first tab, currently in global)
* user input on which measures are management and which are structural (required for linE calculation)
* distinction of measures across variables (e.g. with a matrix decision board) == unlikely to be realised as number of measures is so variable
* PCA - selection of how many variables shall be plotted
* PCA - allow either kmedoids or kmeans (does not work on my machine so I cannot test it)
* PCA - other selections like violin plot possible in Sydney's most recent script
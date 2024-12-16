######################### GLOBAL ###################################
# comments: we assume four variables delineating pareto front
# Project: Clustering Pareto solutions/Multi-objective visualisation
# author: cordula.wittekind@ufz.de
####################################################################
## loading new packages
foo1 <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE , quiet = T)
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}
## check if any packages are missing (not only here but also for external convert_optain)
foo1(c("configr","corrplot", "dplyr","DT", "fs","fst","geosphere", "ggplot2",  "ggtext", "gridExtra", 
       "here",  "htmltools","htmlwidgets",   "ini",    "leaflet",  "leafsync",
       "mapview",  "plotly",  "processx", "purrr",
        "quanteda",   "RColorBrewer",  "readr",  "reticulate",
       "scales",  "sf",  "shiny", "shinycssloaders","shinydashboard", 
       "shinyFiles", "shinyjs","shinythemes",  "shinyWidgets",  "sp", 
       "spdep",  "tibble",  "tidyr",  "tidyverse",  "tmap",  "viridis","webshot"))


options(shiny.maxRequestSize = 1000*1024^2)

options(warn = -1)
source("functions.R")

save_dir <- "../data/"
input_dir <- "../input/"
output_dir <- "../output/"
pareto_path <- "../data/pareto_fitness.txt" #used too frequently..
if(!dir.exists(save_dir)){  dir.create(save_dir)}
if(!dir.exists(output_dir)){  dir.create(output_dir)}
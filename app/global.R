######################### GLOBAL ###################################
# comments: we assume four variables delineating pareto front
# Project: Clustering Pareto Solutions/Multi-objective visualisation
####################################################################
library(configr)#
library(corrplot)#
library(dplyr)#
library(DT)#
library(fs)
library(fst)
library(ggplot2)
library(gridExtra) # or patchwork
library(htmltools)
# library(htmlwidgets) #save html files
library(ini)#
library(leaflet)#
library(processx) # manage system processes from within shiny (e.g. for pulling R output into shiny)
library(purrr)
library(leafsync)
library(quanteda)
library(RColorBrewer)
library(reticulate)
library(scales)
library(sf)#
library(shiny)#
options(shiny.maxRequestSize = 1000*1024^2)
library(shinycssloaders) # for withSpinner()
library(shinydashboard)#
library(shinyFiles) #drag and drop
library(shinyjs) # for hiding parts of the app
library(shinyWidgets)#
library(tibble)
library(tidyr)
library(tidyverse)
library(viridis)

source("functions.R")
options(warn = -1)
save_dir <- "../data/"
input_dir <- "../input/"
output_dir <- "../output/"
pareto_path <- "../data/pareto_fitness.txt" #used too frequently..
if(!dir.exists(save_dir)){  dir.create(save_dir)}
if(!dir.exists(output_dir)){  dir.create(output_dir)}

##descriptions of variables included in PCA and created in convert_optain.R

optain_pca_content <- c(
  
  "share_tot" = "Each measure's share in total catchment area.",
  "share_con"= "Each measure's share in area considered for implementation.",
  "moran"= "The median spatial autocorrelation between HRUs allocated to each implemented measure.",
  "channel_frac"= "The median fraction of water under each implemented measure that is routed directly into the channel.",
  "linE"= "The ratio between structural and management measures."
)

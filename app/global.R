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
library(ini)#
library(leaflet)#
library(plotly)
library(processx) # manage system processes from within shiny (e.g. for pulling R output into shiny)
library(purrr)
library(quanteda)
library(RColorBrewer)
library(reticulate)
library(scales)
library(sf)#
library(shiny)#
library(shinydashboard)#
library(shinyFiles) #drag and drop
library(shinyjs) # for hiding parts of the app
library(shinyWidgets)#
library(tibble)
library(tidyr)
library(tidyverse)

source("functions.R")
save_dir <- "../data/"
input_dir <- "../input/"
pareto_path <- "../data/pareto_fitness.txt"
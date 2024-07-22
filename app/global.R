######################### GLOBAL #################################
# comments: we assume four variables delineating pareto front
# Project: Clustering of pareto front to reduce objective space
##################################################################
library(configr)#
library(leaflet)#
library(sf)#
library(tibble)
library(tidyverse)
library(fs)
library(RColorBrewer)
library(corrplot)#
library(reticulate)
library(shiny)#
library(dplyr)#
library(ini)#
library(shinydashboard)#
library(shinyWidgets)#
library(DT)#
library(shinyjs) # for hiding parts of the app
library(processx) # manage system processes from within shiny (e.g. for pulling R output into shiny)
library(shinyFiles) #drag and drop
source("functions.R")
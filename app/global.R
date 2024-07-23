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
library(ggplot2)
library(GGally)
library(scales)
library(purrr)
library(tidyr)
library(plotly)
library(tibble)
library(htmltools)
library(fst)
library(quanteda)
library(shinyjs) # for hiding parts of the app
library(processx) # manage system processes from within shiny (e.g. for pulling R output into shiny)
library(shinyFiles) #drag and drop
source("functions.R")

save_dir <- "../data/"
pareto_path <- "../data/pareto_fitness.txt"
## all of the following stuff is from multiobjectives plotting and has to be turned reactive
objectives <- tibble(
  short = c('HC', 'HQ', 'P', 'AP'),
  full  = c('Habitat\n connectivity',
            'Habitat\n quality',
            'Phosphorous\n load',
            'Agricultural\n production'), 
  full_plotly = c('Probability of Connectivity',
                  'Habitat\n quality',
                  'Phosphorous load\n (kgN yr<sup>-1</sup>)',
                  'Agricultural\n production\n
                  (grain units)'),
  full_ggplot = c(expression(Probability~of~connectivity),
                  expression(Habitat~Quality),
                  expression(Phosphorous~load~'('~kg~P~yr^{-1}~')'),
                  expression(Agricultural~Production~'('~grain~units~')'))
  
)

# read in pareto_fitness







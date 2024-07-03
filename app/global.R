######################### GLOBAL #################################
# comments: we assume four variables delineating pareto front
# 
# used files:
# Project: Clustering of pareto front to reduce objective space
##################################################################
library(configr)
library(corrplot)
library(shiny)
library(reticulate)
library(readr)
library(dplyr)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
source("functions.R")

## Data Prep
  required_files <- c("../data/pareto_genomes.txt", "../data/pareto_fitness.txt", "../data/hru.con", "../data/measure_location.csv")   # Example file names

## get measures
  mes = read.csv("../data/measure_location.csv")
  mes = unique(mes$nswrm)
  nm = length(mes)

## read config
  config <- read.config("../input/config.ini")
  
  # this is produced in first tab
  hauptdat = read.csv("../input/var_corr_par.csv")
  all_var = colnames(hauptdat)[5:ncol(hauptdat)] #assuming four variables here
  
  
  
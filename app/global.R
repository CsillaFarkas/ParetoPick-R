################ GLOBAL ############################
library(configr)
library(corrplot)
library(shiny)
library(reticulate)
library(readr)
library(dplyr)
source("functions.R")

## get measures
  mes = read.csv("../data/measure_location.csv")
  mes = unique(mes$nswrm)
  nm = length(mes)

## read config
  config <- read.config("../input/config.ini")


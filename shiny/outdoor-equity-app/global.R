## HOW TO DEPLOY APP ##
# use function deployApp()
# deployApp("/capstone/outdoorequity/halina/outdoor-equity/shiny/outdoor-equity-app")

# attached packages ----
library(rsconnect)
library(bslib)
library(shinyWidgets)
library(tidycensus)
library(shiny)
library(leaflet)
library(tidyverse)
library(tmap)

# import data ---- 
data_test <- readRDS("./data_resMedIncome_CAmap_2018.rds")
data_zip_geometries_ca <- readRDS("./data_zip_geometries_ca.rds")


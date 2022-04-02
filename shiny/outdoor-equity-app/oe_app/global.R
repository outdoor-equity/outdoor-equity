# HOW TO DEPLOY APP ----
# use function deployApp()
# deployApp("/capstone/outdoorequity/halina/outdoor-equity/shiny/outdoor-equity-app/oe_app")

# attached packages ----
library(rsconnect)
library(bslib)
library(shinyWidgets)
library(tidycensus)
library(shiny)
library(shinydashboard)
library(leaflet)
library(tidyverse)
library(tmap)
library(janitor)
library(lubridate)
library(sf)
library(patchwork)
library(scales)
library(paletteer)

# import data ---- 
## CA prototype joined 2018 data ----
data_joined_2018 <- readRDS("data/data_joined_2018.rds")
data_hist_distance_traveled <- readRDS("data/data_hist_distance_traveled.rds")
# agency analysis race hist
data_hist_race <- readRDS("data/2018_data_plot_col_race.rds")
# agency analysis comp dist travel x race 
data_comp_dist_travel_race <- readRDS("data/data_comp_dist_travel_race.rds")
# data need to create regional map for site analysis
data_ca_geom <- readRDS("data/data_ca_geom.rds")
# data_comb_CAmap <- readRDS("data/data_combined_CAmap_2018.rds")
# data_reg_comb <- readRDS("data/data_regional_combined_2018.rds")

# source input functions ----
source("r/inputs.R")


# CA prototype objects ----
# use in R scripts, ui or server

## CA agency list ----
ca_agency <- as.vector(unique(data_joined_2018$agency))

## CA admin units ----
admin_units <- as.vector(unique(data_joined_2018$regional_area))

## CA reservable sites ----
sites <- as.vector(unique(data_joined_2018$park))

## PRETTY NAMES ----
### distribution vars ----
dist_vars <- c("Booking window" = "booking_window",
               "Daily cost" = "daily_cost_per_visitor",
               "Distance traveled" = "distance_traveled_mi",
               # cat education
               "Education" = "hs_GED_or_below",
               "Education" = "college",
               "Education" = "some_college",
               "Education" = "master_or_above",
               "Estimated household income" = "median_income",
               "Language - English only" = "english_only",
               "Language - Not English only" = "not_english_only",
               "Length of stay" = "length_of_stay",
               "Race - Asian" = "asian",
               "Race - Black" = "black",
               "Race - Hispanic/Latinx" = "hispanic_latinx",
               "Race - Multiracial" = "multiracial",
               "Race - Native American" = "native_american",
               "Race - Pacific Islander" = "pacific_islander",
               "Race - White" = "white",
               "Race - Other" = "other")
               #"Total number of different site types" = "aggregated_site_type",
               #"Total number of sites" = "park", # is this right?
               #"Total number of visits" = "count"

### comparison vars ----
compare_vars <- c("Education - High School/GED" = "median_hs_GED_or_below",
                  "Education - College" = "median_college",
                  "Education - Some college" = "median_some_college",
                  "Estimated household income" = "median_median_income",
                  "Education - Master or above" = "median_master_or_above",
                  "Language - English only" = "median_english_only",
                  "Language - Not English only" = "median_not_english_only",
                  "Race - Asian" = "median_asian",
                  "Race - Black" = "median_black",
                  "Race - Hispanic/Latinx" = "median_hispanic_latinx",
                  "Race - Multiracial" = "median_multiracial",
                  "Race - Native American" = "median_native_american",
                  "Race - Pacific Islander" = "median_pacific_islander",
                  "Race - White" = "median_white",
                  "Race - Other" = "median_other")

compare_vars_2 <- c("Booking window" = "median_booking_window",
                    "Daily cost" = "median_daily_cost_per_visitor",
                    "Distance traveled" = "distance_traveled_m",
                    "Length of stay" = "median_length_of_stay",
                    "Site Type" = "aggregated_site_type")

# comparing with booking window
booking_scat_var <- c("Booking window" = "median_booking_window")

agency_comp_scat_vars <- c("Distance traveled" = "distance_traveled_m",
                           "Site Type" = "aggregated_site_type")

# HOW TO DEPLOY APP ----
# use function deployApp()
# deployApp("/capstone/outdoorequity/halina/outdoor-equity/shiny/outdoor-equity-app/oe_app")

# attached packages ----
library(rsconnect)
library(bslib)
library(tidycensus)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders) # loading icon 
library(reactlog)
#library(leaflet)
library(tidyverse)
library(tmap)
library(janitor)
library(lubridate)
library(sf)
library(patchwork)
library(scales)
library(paletteer)
#library(DT)
library(collections)

reactlog_enable()

# IMPORT DATA ---- 
## CA prototype joined 2018 ----
data_joined_2018 <- readRDS("data/2018_joined_data.rds")
## booking window ----
data_plot_boooking_window <- readRDS("data/2018_data_plot_boooking_window.rds")
## yosemite visitorsheds ----
data_yosemite_upper_pines_geom <- readRDS("data/2018_data_map_ca_yosemite_upper_pines_geom.rds")
data_zip_geometries_ca <- readRDS("data/2018_data_map_ca_yosemite_upper_pines_zip_geometries_ca.rds")
data_geometries_us <- readRDS("data/2018_data_map_us_yosemite_upper_pines_geometries_us.rds")
## race dist travel comparison ----
data_race_dist_travel <- readRDS("data/2018_data_plot_race_distance_traveled.rds")

# SOURCE FUNCTIONS ----
## source input functions ----
source("r/inputs/select_agency.R")
source("r/inputs/select_admin_unit.R")
source("r/inputs/select_site.R")
source("r/inputs/select_data_summary_vars.R")
source("r/inputs/select_relationships_vars.R")

## source summary rdf functions ----
source("r/summary_plots/dist_travel_rdf.R")

## source summary plot functions ----
source("r/summary_plots/dist_travel_plot.R")



# CA objects ----
# use in R scripts, ui or server

## CA agency list ----
ca_agency <- as.vector(unique(data_joined_2018$agency))

## CA admin units ----
admin_units <- as.vector(unique(data_joined_2018$admin_unit))

## CA reservable sites ----
sites <- as.vector(unique(data_joined_2018$park))

# VARS ----
## data summary vars ----
summary_vars <- c("Distance traveled" = "distance_traveled_mi")
  
  # c("Booking window" = "booking_window",
  #              "Daily cost" = "daily_cost_per_visitor",
  #              "Distance traveled" = "distance_traveled_mi",
  #              # cat education
  #              # "Education" = "hs_GED_or_below",
  #              # "Education" = "college",
  #              # "Education" = "some_college",
  #              # "Education" = "master_or_above",
  #              "Estimated household income" = "median_income",
  #              # cat language
  #              # "Language - English only" = "english_only",
  #              # "Language - Not English only" = "not_english_only",
  #              "Length of stay" = "length_of_stay",
  #              "Race - Asian" = "asian",
  #              "Race - Black" = "black",
  #              "Race - Hispanic/Latinx" = "hispanic_latinx",
  #              "Race - Multiracial" = "multiracial",
  #              "Race - Native American" = "native_american",
  #              "Race - Pacific Islander" = "pacific_islander",
  #              "Race - White" = "white",
  #              "Race - Other" = "other")
  #              #"Total number of different site types" = "aggregated_site_type",
  #              #"Total number of sites" = "park", # is this right?
  #              #"Total number of visits" = "count"

## relationship vars ----
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

## DICTIONARY ----

# use keys() to view all the keys in the dict
# use get() to get specific values from a specific key

### agency to admin units ----
agency_to_admin_unit_dict <- dict()

for (i in seq_along(ca_agency)){
  # pull out each agency
  ag_df <- data_joined_2018 %>% filter(agency == ca_agency[[i]]) 
  # pull out each admin unit
  value_vector <- unique(ag_df$admin_unit)
  
  agency_to_admin_unit_dict$set(ca_agency[[i]], value_vector)
} # EO agency to admin unit dictionary

### admin unit to sites ----
admin_units_to_site_dict <- dict()

for (i in seq_along(admin_units)){
  # pull out each admin unit
  au_df <- data_joined_2018 %>% filter(admin_unit == admin_units[[i]]) 
  # pull out each park
  value_vector <- unique(au_df$park)
  
  admin_units_to_site_dict$set(admin_units[[i]], value_vector)
} # EO admin unit to site dictionary

## HOW TO DEPLOY APP ##
# use function deployApp()
# deployApp("/capstone/outdoorequity/halina/outdoor-equity/shiny/outdoor-equity-app/oe_app")

# attached packages ----
library(rsconnect)
library(bslib)
library(shinyWidgets)
library(tidycensus)
library(shiny)
library(leaflet)
library(tidyverse)
# library(tmap)

# import data ---- 
data_joined_2018 <- readRDS("data/data_joined_2018.rds")
data_hist_distance_traveled <- readRDS("data/data_hist_distance_traveled.rds")

# pretty names here to reference in ui or server 
# variables from 2018_joined_data.rds 
agency_hist_vars <- c("Booking window" = "booking_window",
                      "Daily cost" = "daily_cost_per_visitor",
                      "Distance traveled" = "distance_traveled_mi",
                      # cat education
                      "Education - High School/GED" = "hs_GED_or_below",
                      "Education - College" = "college",
                      "Education - Some college" = "some_college",
                      "Education - Master or above" = "master_or_above",
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
                      "Race - Other" = "other",
                      #"Total number of different site types" = "aggregated_site_type",
                      #"Total number of sites" = "park", # is this right?
                      #"Total number of visits" = "count",
                      "Transportation - No access to vehicle" = "no_vehicle")
       
agency_comp_acs_col_vars <- c("Education - High School/GED" = "median_hs_GED_or_below",
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
                          "Race - Other" = "median_other",
                          "Transportation - No access to vehicle" = "median_no_vehicle")

agency_comp_col_vars <- c("Booking window" = "median_booking_window",
                           "Daily cost" = "median_daily_cost_per_visitor",
                           "Distance traveled" = "distance_traveled_m",
                           "Length of stay" = "median_length_of_stay",
                           "Site Type" = "aggregated_site_type")

# comparing with booking window
booking_scat_var <- c("Booking window" = "median_booking_window")

agency_comp_scat_vars <- c("Distance traveled" = "distance_traveled_m",
                           "Site Type" = "aggregated_site_type")

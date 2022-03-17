## HOW TO DEPLOY APP ##
# use function deployApp()
# deployApp("/capstone/outdoorequity/halina/outdoor-equity/shiny/outdoor-equity-app/OE_app")

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
# data_test <- readRDS("./data_resMedIncome_CAmap_2018.rds")
# data_zip_geometries_ca <- readRDS("./data_zip_geometries_ca.rds")

# pretty names here to reference in ui or server 
agency_hist_vars <- c("Booking window" = "median_booking_window",
                      "Daily cost" = "median_daily_cost_per_visitor",
                      "Distance traveled" = "ADD",
                      "Education - High School/GED" = "median_hs_GED_or_below",
                      "Education - College" = "median_college",
                      "Education - Some college" = "median_some_college",
                      "Education - Master or above" = "median_master_or_above",
                      "Estimated household income" = "median_median_income",
                      "Language - English only" = "median_english_only",
                      "Language - Not English only" = "median_not_english_only",
                      "Length of stay" = "median_length_of_stay",
                      "Race - Asian" = "median_asian",
                      "Race - Black" = "median_black",
                      "Race - Hispanic/Latinx" = "median_hispanic_latinx",
                      "Race - Multiracial" = "median_multiracial",
                      "Race - Native American" = "median_native_american",
                      "Race - Pacific Islander" = "median_pacific_islander",
                      "Race - White" = "median_white",
                      "Race - Other" = "median_other",
                      "Total number of different site types" = "ADD",
                      "Total number of sites" = "park", # is this right?
                      "Total number of visits" = "count",
                      "Transportation - No access to vehicle" = "median_no_vehicle")
       
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
                           "Distance traveled" = "ADD",
                           "Length of stay" = "median_length_of_stay",
                           "Site Type" = "NEED TO ADD THIS TO DF")

# comparing with booking window
booking_scat_var <- c("Booking window" = "median_booking_window")

agency_comp_scat_vars <- c("Distance traveled" = "ADD",
                           "Site Type" = "NEED TO ADD THIS TO DF")

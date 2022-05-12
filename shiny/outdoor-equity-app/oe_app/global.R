# HOW TO DEPLOY APP ----
# use function deployApp()
# deployApp("/capstone/outdoorequity/halina/outdoor-equity/shiny/outdoor-equity-app/oe_app")

# attached packages ----
library(rsconnect)
library(bslib)
library(tidycensus)
library(shiny)
library(shinydashboard) 
library(shinydashboardPlus) # updateBox() 
library(shinyWidgets)
library(shinycssloaders) # loading icon 
library(shinyjs) # hide / show boxes in ui
library(reactlog)
library(tidyverse)
library(tmap)
library(janitor)
library(lubridate)
library(sf)
library(scales)
library(DT)
library(collections)
library(plotly)
library(rmapshaper) # ms_simplify to reduce maps
#library(tmaptools)

reactlog_enable()

# IMPORT DATA ---- 
## CA prototype joined 2018 ----
data_joined_2018 <- readRDS("data/2018_joined_data.rds")
## CA ACS all ----
data_ca_acs_2018 <- readRDS("data/2018_ca_acs_all.rds")
## yosemite visitorsheds ----
data_yosemite_upper_pines_geom <- readRDS("data/2018_data_map_ca_yosemite_upper_pines_geom.rds")
data_zip_geometries_ca <- readRDS("data/2018_data_map_ca_yosemite_upper_pines_zip_geometries_ca.rds")
# simplifying geoms
data_zip_geometries_ca_simple <- data_zip_geometries_ca %>% rmapshaper::ms_simplify(keep = 0.003)
data_geometries_us <- readRDS("data/2018_data_map_us_yosemite_upper_pines_geometries_us.rds")
# simplifying geoms
data_geometries_us_simple <- data_geometries_us %>% rmapshaper::ms_simplify(keep = 0.006)

# SOURCE FUNCTIONS ----
## source input functions ----
source("r/inputs/select_agency.R")
source("r/inputs/select_admin_unit.R")
source("r/inputs/select_site.R")
source("r/inputs/select_data_summary_vars.R")
source("r/inputs/select_relationships_vars.R")

## source summary plot functions ----
#source("r/outputs/summary-boxes-plots.R") see if I can get this to work later
source("r/summary_plots/dist_travel_plot.R")
source("r/summary_plots/booking_window_plot.R")
source("r/summary_plots/daily_cost_visitor_plot.R")
source("r/summary_plots/daily_cost_plot.R")
source("r/summary_plots/length_of_stay_plot.R")
source("r/summary_plots/site_type_plot.R")
source("r/summary_plots/race_plot.R")
source("r/summary_plots/education_plot.R")
source("r/summary_plots/median_income_plot.R")
source("r/summary_plots/language_plot.R")

## source relationships plot functions ----
# quartile plots
source("r/relationship_plots/race_top_quartile_res_data.R")
source("r/relationship_plots/race_top_quartile_res_plot.R")
source("r/relationship_plots/education_top_quartile_res_data.R")
source("r/relationship_plots/education_top_quartile_res_plot.R")
source("r/relationship_plots/language_top_quartile_res_data.R")
source("r/relationship_plots/language_top_quartile_res_plot.R")
source("r/relationship_plots/median_income_top_quartile_res_data.R")
source("r/relationship_plots/median_income_top_quartile_res_plot.R")
# calculating bins (quartiles, deciles) 
source("r/relationship_plots/education_top_quartile.R")
source("r/relationship_plots/race_top_quartile.R")
source("r/relationship_plots/language_top_quartile.R")
source("r/relationship_plots/median_income_deciles.R")
# edu x booking window
source("r/relationship_plots/education_booking_window_data.R")
source("r/relationship_plots/education_booking_window_plot.R")
# edu x daily cost
source("r/relationship_plots/education_daily_cost_data.R")
source("r/relationship_plots/education_daily_cost_plot.R")
# edu x daily cost per visitor
source("r/relationship_plots/education_daily_cost_per_visitor_data.R")
source("r/relationship_plots/education_daily_cost_per_visitor_plot.R")
# edu x dist travel 
source("r/relationship_plots/education_dist_travel_data.R")
source("r/relationship_plots/education_dist_travel_plot.R")
# edu x length of stay
source("r/relationship_plots/education_length_of_stay_data.R")
source("r/relationship_plots/education_length_of_stay_plot.R")
# edu x site type
source("r/relationship_plots/education_site_type_data.R")
source("r/relationship_plots/education_site_type_plot.R")
# lang x booking window
source("r/relationship_plots/language_booking_window_data.R")
source("r/relationship_plots/language_booking_window_plot.R")
# lang x daily cost
source("r/relationship_plots/language_daily_cost_data.R")
source("r/relationship_plots/language_daily_cost_plot.R")
# lang x daily cost per visitor
source("r/relationship_plots/language_daily_cost_per_visitor_data.R")
source("r/relationship_plots/language_daily_cost_per_visitor_plot.R")
# lang x dist travel
source("r/relationship_plots/language_dist_travel_data.R")
source("r/relationship_plots/language_dist_travel_plot.R")
# lang x length of stay
source("r/relationship_plots/language_length_of_stay_data.R")
source("r/relationship_plots/language_length_of_stay_plot.R")
# lang x site type
source("r/relationship_plots/language_site_type_data.R")
source("r/relationship_plots/language_site_type_plot.R")
# median income x booking window
source("r/relationship_plots/median_income_booking_window_data.R")
source("r/relationship_plots/median_income_booking_window_plot.R")
# median income x daily cost
source("r/relationship_plots/median_income_daily_cost_data.R")
source("r/relationship_plots/median_income_daily_cost_plot.R")
# median income x daily cost per visitor
source("r/relationship_plots/median_income_daily_cost_per_visitor_data.R")
source("r/relationship_plots/median_income_daily_cost_per_visitor_plot.R")
# median income x dist travel
source("r/relationship_plots/median_income_dist_travel_data.R")
source("r/relationship_plots/median_income_dist_travel_plot.R")
# median income x length of stay
source("r/relationship_plots/median_income_length_of_stay_data.R")
source("r/relationship_plots/median_income_length_of_stay_plot.R")
# median income x site type
source("r/relationship_plots/median_income_site_type_data.R")
source("r/relationship_plots/median_income_site_type_plot.R")
# race x booking window
source("r/relationship_plots/race_booking_window_data.R")
source("r/relationship_plots/race_booking_window_plot.R")
# race x daily cost
source("r/relationship_plots/race_daily_cost_data.R")
source("r/relationship_plots/race_daily_cost_plot.R")
# race x daily cost per visitor
source("r/relationship_plots/race_daily_cost_per_visitor_data.R")
source("r/relationship_plots/race_daily_cost_per_visitor_plot.R")
# race X dist travel
source("r/relationship_plots/race_dist_travel_data.R")
source("r/relationship_plots/race_dist_travel_plot.R")
# race x length of stay
source("r/relationship_plots/race_length_of_stay_data.R")
source("r/relationship_plots/race_length_of_stay_plot.R")
# race x site type
source("r/relationship_plots/race_site_type_data.R")
source("r/relationship_plots/race_site_type_plot.R")

## source observe event functions ----
source("r/inputs/observe_event_agency_to_admin_dict.R")
source("r/inputs/observe_event_admin_unit_to_site_dict.R")

# need to move these sourced functions to a new section
source("r/other_plots/tot_site_agency_plot.R")
source("r/other_plots/tot_res_agency_plot.R")

# PLOT OBJECTS ----

# columns data_joined_2018
cols_data_joined_2018 <- as.vector(names(data_joined_2018))

# CA objects ----
## CA agency list ----
ca_agency <- as.vector(unique(data_joined_2018$agency))

## CA admin units ----
admin_units_vec <- as.vector(unique(data_joined_2018$admin_unit))

## CA reservable sites ----
sites <- as.vector(unique(data_joined_2018$park))

# VARS ----
## data summary vars ----
summary_vars <- c("Distance traveled" = "distance_traveled_mi",
                  "Booking window" = "booking_window",
                  "Daily cost" = "daily_cost",
                  "Daily cost per visitor" = "daily_cost_per_visitor",
                  "Length of stay" = "length_of_stay",
                  "Site type" = "aggregated_site_type",
                  "Race" = "race",
                  "Education" = "education",
                  "Median income" = "median_income",
                  "Language" = "not_english_only")

## relationship vars ----
## ADD HERE IF ONCE SPECIFIC VARS HAVE BEEN CHOSEN

# DICTIONARY ----

# use keys() to view all the keys in the dict
# use get() to get specific values from a specific key

### agency to admin units ----
agency_to_admin_unit_dict <- dict()

for (i in seq_along(ca_agency)){
  # pull out each agency
  ag_df <- data_joined_2018 %>% filter(agency == ca_agency[[i]]) 
  # pull out each admin unit
  value_vector_au <- unique(ag_df$admin_unit)
  
  agency_to_admin_unit_dict$set(ca_agency[[i]], value_vector_au)
} # EO agency to admin unit dictionary

### admin unit to sites ----
admin_units_to_site_dict <- dict()

for (i in seq_along(admin_units_vec)){
  # pull out each admin unit
  au_df <- data_joined_2018 %>% filter(admin_unit == admin_units_vec[[i]]) 
  # pull out each site
  value_vector_site <- unique(au_df$park)
  
  admin_units_to_site_dict$set(admin_units_vec[[i]], value_vector_site)
} # EO admin unit to site dictionary

# distance traveled rdf and wrangling
# used in REACTIVE DATA FRAMES in server
# using input id's for summary page in ui
dist_travel_rdf <- function(){

  data_joined_2018 %>%
    filter(agency %in% input$agency_summary,
           admin_unit %in% input$admin_unit_summary,
           park %in% input$site_summary) %>%
    mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
    select(agency, admin_unit, park, distance_traveled_mi)
  
} # EO function

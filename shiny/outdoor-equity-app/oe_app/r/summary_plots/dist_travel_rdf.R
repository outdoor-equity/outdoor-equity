# distance traveled rdf and wrangling
# used in REACTIVE DATA FRAMES in server
# using input id's for summary page in ui

dist_travel_rdf <- function(agencyId = input$agency_summary, 
                               admin_unitId = input$admin_unit_summary, 
                               parkId = input$site_summary){
  
  
  data_joined_2018 %>%
    filter(agency %in% agencyId,
           admin_unit %in% admin_unitId,
           park %in% parkId) %>%
    mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
    select(agency, admin_unit, park, distance_traveled_mi)
  
  
  
} # EO function

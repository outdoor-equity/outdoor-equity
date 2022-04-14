# booking window rdf and wrangling
# used in REACTIVE DATA FRAMES in server
# using input id's for summary page in ui

booking_window_rdf <- function(){
  
  data_joined_2018 %>%
    filter(agency %in% input$agency_summary,
           admin_unit %in% input$admin_unit_summary,
           park %in% input$site_summary) %>%
    filter(booking_window > 0,
           booking_window < 510) %>% 
    select(agency, admin_unit, park, booking_window)
  
} # EO function

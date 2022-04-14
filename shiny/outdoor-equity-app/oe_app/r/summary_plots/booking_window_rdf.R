# booking window rdf and wrangling
booking_window_rdf <- function(agencyId = input$agency_summary, 
                               admin_unitId = input$admin_unit_summary, 
                               parkId = input$site_summary){
  
  
  data_joined_2018 %>%
    filter(agency %in% agencyId,
           admin_unit %in% admin_unitId,
           park %in% parkId) #%>%
  # need to add more
  
  
  
} # EO function

# distance traveled rdf and wrangling

data_joined_2018 %>% 
  filter(agency %in% input$agency_summary,
         admin_unit %in% input$admin_unit_summary,
         park %in% input$site_summary) #%>%
  # need to add more

# server instructions ----
server <- function(input, output, session){
  
# OBSERVE EVENTS ----  
## SO OE press agency ----
### summary box 1 ----
observeEvent(input$agency_summary_1, {

  # function to call values based on key from agency to admin dict
  oe_agency_to_admin_dict(isInput_key = input$agency_summary_1,
                          page = "agency_summary_1")

  # update input with new choices
  updateSelectizeInput(session, "admin_unit_summary_1",
                       choices = sort(choices)
                       )
}) ## EO OE press agency on summary page box 1
  
### summary box 2 ----
observeEvent(input$agency_summary_2, {

  # function to call values based on key from agency to admin dict
  oe_agency_to_admin_dict(isInput_key = input$agency_summary_2,
                          page = "agency_summary_2")

  # update input with new choices
  updateSelectizeInput(session, "admin_unit_summary_2",
                       choices = sort(choices)
                       )
  }) ## EO OE press agency on summary page box 2
  

### summary box 3 ----
observeEvent(input$agency_summary_3, {

    # function to call values based on key from agency to admin dict
    oe_agency_to_admin_dict(isInput_key = input$agency_summary_3,
                            page = "agency_summary_3")

    # update input with new choices
    updateSelectizeInput(session, "admin_unit_summary_3",
                         choices = sort(choices)
    )
  }) ## EO OE press agency on summary page box 3
  
### summary box 4 ----
observeEvent(input$agency_summary_4, {

    # function to call values based on key from agency to admin dict
    oe_agency_to_admin_dict(isInput_key = input$agency_summary_4,
                            page = "agency_summary_4")

    # update input with new choices
    updateSelectizeInput(session, "admin_unit_summary_4",
                         choices = sort(choices)
    )
  }) ## EO OE press agency on summary page box 4

  
### relationships page ----
observeEvent(input$agency_relationships, {

  # function to call values based on key from agency to admin dict
  oe_agency_to_admin_dict(isInput_key = input$agency_relationships,
                          page = "agency_relationships")

  # update input with new choices
  updateSelectizeInput(session, "admin_unit_relationships",
                       choices = sort(choices)
                       )
}) ## EO OE press agency on relationships page

### visitorsheds page ----
observeEvent(input$agency_visitorsheds, {

  # function to call values based on key from agency to admin dict
  oe_agency_to_admin_dict(isInput_key = input$agency_visitorsheds,
                          page = "agency_visitorsheds")

  # update input with new choices
  updateSelectizeInput(session, "admin_unit_visitorsheds",
                       choices = sort(choices)
                       )
}) ## EO OE press agency on visitorsheds page

### data download page ----
observeEvent(input$agency_data_download, {

  # function to call values based on key from agency to admin dict
  oe_agency_to_admin_dict(isInput_key = input$agency_data_download,
                          page = "agency_data_download")

  # update input with new choices
  updateSelectizeInput(session, "admin_unit_data_download",
                       choices = sort(choices)
                       )
}) ## EO press agency on data download page
  
  
## SO OE press admin unit ----
### summary box 1 ----
observeEvent(input$admin_unit_summary_1, {

  # function to call values based on key from agency to admin dict
  oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary_1,
                             page = "admin_unit_summary_1")

  # update input with new choices
  updateSelectizeInput(session, "site_summary_1",
                       choices = sort(choices)
                       )
}) ## EO press admin unit on summary page box 1
  
### summary box 2 ----
observeEvent(input$admin_unit_summary_2, {

  # function to call values based on key from agency to admin dict
  oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary_2,
                             page = "admin_unit_summary_2")

  # update input with new choices
  updateSelectizeInput(session, "site_summary_2",
                       choices = sort(choices)
                       )
  }) ## EO press admin unit on summary page box 2
  
### summary box 3 ----
observeEvent(input$admin_unit_summary_3, {

  # function to call values based on key from agency to admin dict
  oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary_3,
                             page = "admin_unit_summary_3")

  # update input with new choices
  updateSelectizeInput(session, "site_summary_3",
                       choices = sort(choices)
                       )
  }) ## EO press admin unit on summary page box 3
  
### summary box 4 ----
observeEvent(input$admin_unit_summary_4, {

  # function to call values based on key from agency to admin dict
  oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary_4,
                             page = "admin_unit_summary_4")

  # update input with new choices
  updateSelectizeInput(session, "site_summary_4",
                       choices = sort(choices)
                       )
  }) ## EO press admin unit on summary page box 4
  
  
  
### relationships page ----
observeEvent(input$admin_unit_relationships, {

  # function to call values based on key from agency to admin dict
  oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_relationships,
                             page = "admin_unit_relationships")

  # update input with new choices
  updateSelectizeInput(session, "site_relationships",
                       choices = sort(choices)
                       )
  }) ## EO press admin unit on relationships page
  
  
### visitorsheds page ----
observeEvent(input$admin_unit_visitorsheds, {

  # function to call values based on key from agency to admin dict
  oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_visitorsheds,
                             page = "admin_unit_visitorsheds")

  # update input with new choices
  updateSelectizeInput(session, "site_visitorsheds",
                       choices = sort(choices)
                       )
  }) ## EO press admin unit on visitorsheds page

### data download page ----
observeEvent(input$admin_unit_data_download, {

  # function to call values based on key from agency to admin dict
  oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_data_download,
                             page = "admin_unit_data_download")

  # update input with new choices
  updateSelectizeInput(session, "site_data_download",
                       choices = sort(choices)
                       )
}) ## EO press admin unit on data download page

## SO OE press num_viz ----
observeEvent(input$num_viz, {

  if (input$num_viz == 1) {
    
    shinyjs::hide(id = "num_viz_2")

    shinydashboardPlus::updateBox(id = "num_viz_1",
                                  action = "update",
                                  options = list(width = 12))
  }
  else if (input$num_viz == 2) {
    
    shinyjs::show(id = "num_viz_2")

    shinydashboardPlus::updateBox(id = "num_viz_1",
                                  action = "update",
                                  options = list(width = 6))

  }
}) ## EO OE press num_viz


  
# observeEvent(input$data_summary, {
#   if (req(input$admin_unit_summary_1, input$site_summary_1)) {
#     # if inputs exist hide emoty input text
#     output$data_summary_text <- renderPrint({ "" })
#   } else {
#     #else show empty input text
#     output$data_summary_text <- renderPrint({ "Please select a reservable site to display a plot" })
#   }
#   
# })

# RENDER PLOTS ----
## SO DATA SUMMARY PLOTS 1 ----
output$data_summary_plot_1 <- renderPlotly({
  ### SO distance traveled ----
  if (input$data_summary == "distance_traveled_mi") {

    dist_travel_plot(admin_unitInput = input$admin_unit_summary_1,
                     siteInput = input$site_summary_1)

  } ## EO if distance traveled

  ## SO booking window ----
  else if (input$data_summary == "booking_window") {
    
    booking_window_plot(admin_unitInput = input$admin_unit_summary_1,
                        siteInput = input$site_summary_1)

  } ## EO else if booking window
  
  ## SO daily cost ----
  else if (input$data_summary == "daily_cost") {
    
    daily_cost_plot(admin_unitInput = input$admin_unit_summary_1,
                    siteInput = input$site_summary_1)
    
  } ## EO else if daily cost

  ## SO daily cost per visitor ----
  else if (input$data_summary == "daily_cost_per_visitor") {

    daily_cost_visitor_plot(admin_unitInput = input$admin_unit_summary_1,
                            siteInput = input$site_summary_1)

  } ## EO else if daily cost per visitor
  
  ## SO education ----
  else if (input$data_summary == "education") {
    
    education_plot(admin_unitInput = input$admin_unit_summary_1,
                   siteInput = input$site_summary_1)
    
  } ## EO education

  ## SO length of stay ----
  else if (input$data_summary == "length_of_stay") {

    length_of_stay_plot(admin_unitInput = input$admin_unit_summary_1,
                        siteInput = input$site_summary_1)

  } ## EO else if length of stay

  ## SO site type ----
  else if (input$data_summary == "aggregated_site_type") {

    site_type_plot(admin_unitInput = input$admin_unit_summary_1,
                   siteInput = input$site_summary_1)

  } ## EO else if site type

  ## SO race ----
  # else if (input$data_summary == "race") {
  #
  #   race_plot(agencyInput = input$agency_summary,
  #             admin_unitInput = input$admin_unit_summary,
  #             siteInput = input$site_summary,
  #             titleInput = input$site_summary)
  #
  # } ## EO else if race


  ## SO median income ----
  else if (input$data_summary == "median_income") {

    median_income_plot(admin_unitInput = input$admin_unit_summary_1,
                       siteInput = input$site_summary_1)

  } ## EO else if median income
  
  ## SO language ----
  else if (input$data_summary == "not_english_only") {
    
    language_plot(admin_unitInput = input$admin_unit_summary_1, 
                  siteInput = input$site_summary_1)
    
  } ## EO language 

}) ## EO data summary plots


## DATA SUMMARY PLOTS 2 ----
output$data_summary_plot_2 <- renderPlotly({
  ### SO distance traveled ----
  if (input$data_summary == "distance_traveled_mi") {
    
    dist_travel_plot(admin_unitInput = input$admin_unit_summary_2,
                     siteInput = input$site_summary_2)
    
  } ## EO if distance traveled
  
  ## SO booking window ----
  else if (input$data_summary == "booking_window") {
    
    booking_window_plot(admin_unitInput = input$admin_unit_summary_2,
                        siteInput = input$site_summary_2)
    
  } ## EO else if booking window
  
  ## SO daily cost ----
  else if (input$data_summary == "daily_cost") {
    
    daily_cost_plot(admin_unitInput = input$admin_unit_summary_2,
                    siteInput = input$site_summary_2)
    
  } ## EO else if daily cost
  
  ## SO daily cost per visitor ----
  else if (input$data_summary == "daily_cost_per_visitor") {
    
    daily_cost_visitor_plot(admin_unitInput = input$admin_unit_summary_2,
                            siteInput = input$site_summary_2)
    
  } ## EO else if daily cost per visitor
  
  ## SO education ----
  else if (input$data_summary == "education") {
    
    education_plot(admin_unitInput = input$admin_unit_summary_2,
                   siteInput = input$site_summary_2)
    
  } ## EO education
  
  ## SO length of stay ----
  else if (input$data_summary == "length_of_stay") {
    
    length_of_stay_plot(admin_unitInput = input$admin_unit_summary_2,
                        siteInput = input$site_summary_2)
    
  } # EO length of stay
  
  ## SO site type ----
  else if (input$data_summary == "aggregated_site_type") {
    
    site_type_plot(admin_unitInput = input$admin_unit_summary_2,
                   siteInput = input$site_summary_2)
    
  } ## EO else if site type
  
  ## SO median income ----
  else if (input$data_summary == "median_income") {
    
    median_income_plot(admin_unitInput = input$admin_unit_summary_2,
                       siteInput = input$site_summary_2)
    
  } ## EO else if median income
  
  ## SO language ----
  else if (input$data_summary == "not_english_only") {
    
    language_plot(admin_unitInput = input$admin_unit_summary_2, 
                  siteInput = input$site_summary_2)
    
  } ## EO language 
  
}) ## EO DATA SUMMARY PLOTS 2 

## SO RELATIONSHIPS PLOTS NO REACTIVE ----
### race wrangling ----
race_group <- c("other", "pacific_islander", "multiracial", "asian",
                 "black", "white", "native_american", "hispanic_latinx")
# “high” cutoff value
data_race_quants <-
  race_group %>%
  map_dbl(race_top_quartile, acs_df = data_ca_acs_2018) %>%
  cbind("race_group" = race_group,
         "weighted_quartile" = .) %>%
  as.data.frame()

print(data_race_quants)

#if (req(input$admin_unit_relationships, input$site_relationships, data_race_quants, data_joined_2018))
output$data_relationships_plot <- renderPlotly({
### SO race x dist travel ----
if (input$data_relationships == "Race x Distance traveled") {
  
race_dist_travel_plot(admin_unitInput = input$admin_unit_relationships,
                      siteInput = input$site_relationships,
                      race_top_quartile_df = data_race_quants,
                      ridb_df = data_joined_2018)
  
} # EO race x dist travel
   
}) ## EO relationships plots 

## SO VISITORSHEDS PLOTS YES REACTIVE ----
### yosemite plot ----
output$caVisitorshed_plot <- renderTmap({
  
  tm_shape(data_zip_geometries_ca) +
    tm_fill(col = "number_reservations",
            title = "Number of Visits",
            palette = "PuRd",
            style = "jenks",
            n = 10, 
            popup.vars = c("Total Visits" = "number_reservations")) +
    tm_shape(data_yosemite_upper_pines_geom) +
    tm_dots(col = "#009900FF", size = 0.1, alpha = 0.9,
            id = "park") +
    tm_view(set.view = c(-119.559917, 37.061753, 6))
  
})


output$usVisitorshed_plot <- renderTmap({
  
  tm_shape(data_geometries_us) +
    tm_borders(col = "grey", alpha = 0.5) +
    tm_fill(col = "number_reservations",
            title = "Number of Visits",
            palette = "YlGn",
            n = 10, 
            style = "jenks",
            id = "zip_state_abbr", 
            popup.vars = c("Total Visits" = "number_reservations")) +
    tm_view(set.view = c(-101.834335, 40.022356, 3))
  
}) # EO visitorsheds plots

## SO DATA DOWNLOAD ----
# create RDF
data_download_dt <- reactive({
  
  data_joined_2018 %>% filter(park %in% input$site_data_download)

})
# DT table
output$data_download_table <- renderDT({
  
  DT::datatable(data_download_dt())
  
})



} ## EO server

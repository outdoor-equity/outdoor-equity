# server instructions ----
server <- function(input, output, session){

  
  
# OBSERVE EVENTS ----  
## SO OE press agency ----
### data summary page box 1 ----
observeEvent(input$agency_summary_1, {
  
  # function to call values based on key from agency to admin dict
  oe_agency_to_admin_dict(isInput_key = input$agency_summary_1,
                          page = "agency_summary_1")

  # update input with new choices
  updateSelectizeInput(session, "admin_unit_summary_1",
                       choices = sort(choices)
                       )
}) ## EO OE press agency on summary page
  
### data summary page box 2 ----
observeEvent(input$agency_summary_2, {
  
  # function to call values based on key from agency to admin dict
  oe_agency_to_admin_dict(isInput_key = input$agency_summary_2,
                          page = "agency_summary_2")
  
  # update input with new choices
  updateSelectizeInput(session, "admin_unit_summary_2",
                       choices = sort(choices)
    )
  }) ## EO OE press agency on summary page

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
### summary page ----
observeEvent(input$admin_unit_summary_1, {
  
  # function to call values based on key from agency to admin dict
  oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary_1,
                             page = "admin_unit_summary_1")
  
  # update input with new choices
  updateSelectizeInput(session, "site_summary_1",
                       choices = sort(choices)
                       )
}) ## EO press admin unit on summary page
  
## SITE_SUMMARY_2
observeEvent(input$admin_unit_summary_2, {
  # function to call values based on key from agency to admin dict
  oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary_2,
                             page = "admin_unit_summary_2")
  
  # update input with new choices
  updateSelectizeInput(session, "site_summary_2",
                       choices = sort(choices))
  }) ## EO press admin unit on summary page
  
### relationships page ----
observeEvent(input$admin_unit_relationships, {
  
  # function to call values based on key from agency to admin dict
  oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary,
                             page = "admin_unit_summary")
  
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
# observeEvent(input$num_viz, {
# 
#   if (input$num_viz == "1") {
#     shinyjs::hide(id = c("site_summary_2", "data_summary_plot_2"))
#   }
#   else if (input$num_viz == 2) {
#     shinyjs::hide(id = "site_summary_1")
#   }
#   # else (input$num_viz == 3) {
#   #   shinyjs::hide(id %in% c("num_viz_1", "num_viz_2"))
#   # }
# 
# }) ## EO OE press num_viz

## race RDF ----
race_df <- reactive({
  
  data_joined_2018 %>% 
    filter(agency %in% input$agency_summary,
           admin_unit %in% input$admin_unit_summary,
           park %in% input$site_summary) %>% 
    summarize(white = (mean(white, na.rm = TRUE) * 100),
              black = (mean(black, na.rm = TRUE) * 100),
              asian = (mean(asian, na.rm = TRUE) * 100),
              multiracial = (mean(multiracial, na.rm = TRUE) * 100),
              other = (mean(other, na.rm = TRUE) * 100),
              native_american = (mean(native_american, na.rm = TRUE) * 100),
              pacific_islander = (mean(pacific_islander, na.rm = TRUE) * 100),
              hispanic_latinx = (mean(hispanic_latinx, na.rm = TRUE) * 100)) %>%
    pivot_longer(cols = 1:8, names_to = "race", values_to = "race_percent_average") %>% 
    mutate(race = str_replace(string = race,
                              pattern = "_",
                              replacement = " "),
           race = str_to_title(race))
})

# RENDER PLOTS ----
## SO DATA SUMMARY PLOTS ----
output$data_summary_plot <- renderPlotly({
  ### SO distance traveled ----
  if (input$data_summary == "distance_traveled_mi") {
    
    dist_travel_plot(#agencyInput = input$agency_summary,
                     admin_unitInput = input$admin_unit_summary,
                     siteInput = input$site_summary_1)
    
  } ## EO if distance traveled
  
  ## SO booking window ----
  else if (input$data_summary == "booking_window") {
    
    booking_window_plot(agencyInput = input$agency_summary,
                        admin_unitInput = input$admin_unit_summary,
                        siteInput = input$site_summary,
                        titleInput = input$site_summary)
    
  } ## EO else if booking window
  
  ## SO daily cost per visitor ----
  else if (input$data_summary == "daily_cost_per_visitor") {
    
    daily_cost_visitor_plot(agencyInput = input$agency_summary,
                            admin_unitInput = input$admin_unit_summary,
                            siteInput = input$site_summary,
                            titleInput = input$site_summary)
    
  } ## EO else if daily cost per visitor
  
  ## SO length of stay ----
  else if (input$data_summary == "length_of_stay") {
    
    length_of_stay_plot(agencyInput = input$agency_summary,
                        admin_unitInput = input$admin_unit_summary,
                        siteInput = input$site_summary,
                        titleInput = input$site_summary)
    
  } ## EO else if length of stay
  
  ## SO site type ----
  else if (input$data_summary == "aggregated_site_type") {
    
    site_type_plot(agencyInput = input$agency_summary,
                   admin_unitInput = input$admin_unit_summary,
                   siteInput = input$site_summary,
                   titleInput = input$site_summary)
    
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
  
  ## SO education ----
  else if (input$data_summary == "education") {
    
    education_plot(agencyInput = input$agency_summary,
                   admin_unitInput = input$admin_unit_summary,
                   siteInput = input$site_summary,
                   titleInput = input$site_summary)
    
  } ## EO else if education
  
  ## SO median income ----
  else if (input$data_summary == "median_income") {
    
    median_income_plot(agencyInput = input$agency_summary,
                       admin_unitInput = input$admin_unit_summary,
                       siteInput = input$site_summary,
                       titleInput = input$site_summary)
    
  } ## EO else if median income
  
}) ## EO data summary plots

## DATA SUMMARY PLOTS 2 ----
output$data_summary_plot_2 <- renderPlotly({
  ### SO distance traveled ----
  if (input$data_summary == "distance_traveled_mi") {
    
    dist_travel_plot(#agencyInput = input$agency_summary,
                     admin_unitInput = input$admin_unit_summary,
                     siteInput = input$site_summary_2)
    
  } ## EO if distance traveled
  
  ## SO booking window ----
  else if (input$data_summary == "booking_window") {
    
    booking_window_plot(agencyInput = input$agency_summary,
                        admin_unitInput = input$admin_unit_summary,
                        siteInput = input$site_summary_2,
                        titleInput = input$site_summary_2)
    
  } ## EO else if booking window
  
}) ## EO DATA SUMMARY PLOTS 2 

## SO RELATIONSHIPS PLOTS NO REACTIVE ----
output$data_relationships_plot <- renderPlot({
  
  # parameters
  racial_group_colors <- c("Other" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                           "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                           "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  # plot for shiny
  ggplot(data = data_race_dist_travel) +
    geom_col(aes(x = factor(distance_traveled_bins),
                 y = race_percentage,
                 fill = race)) +
    facet_wrap(~race, scales = "free_x") +
    scale_fill_manual(values = racial_group_colors) +
    scale_x_discrete(labels = c("1" = "0 - 60 mi", 
                                "2" = "60 - 117 mi", 
                                "3" = "117 - 181 mi", 
                                "4" = "181 - 299 mi", 
                                "5" = "299 - 3,614 mi")) + 
    labs(x = "Distance Traveled (miles)",
         y = "Percentage (%)",
         title = "Breakdown of Race of Home ZIP Codes vs. Distance Traveled",
         subtitle = "for Overnight Reservations in California") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.background = element_rect("white"),
          panel.grid.major.x = element_blank()) +
    coord_flip()
  
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



} ## EO server


## race plot
# else if(input$data_summary == "Race"){
#   
#   ggplot(data = race_df()) +
#     geom_col(aes(x = race_percent_average,
#                  y = reorder(race, race_percent_average)),
#              stat = "identity") +
#     scale_fill_manual(values = groups_colors_ridb_ca) +  
#     geom_text(aes(x = race_percent_average,
#                   y = reorder(race, race_percent_average),
#                   label = paste0(round(race_percent_average, 1), "%")), 
#               position = position_dodge(width = 1), 
#               hjust = -0.1, size = 4) +
#     scale_color_manual(values = groups_colors_ridb_ca) +
#     labs(x = "Percentage (%)",
#          y = "",
#          title = "Racial Breakdown of ZIP Codes in 2018",
#          subtitle = "Visitors' home ZIP codes for Overnight Reservations in California \nvs. California Residents") +
#     scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10), minor_breaks = seq(0, 60, 5)) +
#     theme_minimal() +
#     theme(plot.background = element_rect("white"),
#           panel.grid.major.y = element_blank()
#     )
#   
#   
# } ## EO of race plot
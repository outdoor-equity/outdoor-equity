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

## SO OE press site relationship plots ----
# observeEvent(input$data_relationships, {
#   
#   if(input$data_relationships == "Race x Site type") {
#     
#     shinyjs::hide(id = "data_relationships_plot")
#     
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
  else if (input$data_summary == "race") {

    race_plot(admin_unitInput = input$admin_unit_summary_1,
              siteInput = input$site_summary_1)

  } ## EO else if race


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
  
  ## SO race ----
  else if (input$data_summary == "race") {
    
    race_plot(admin_unitInput = input$admin_unit_summary_2,
              siteInput = input$site_summary_2)
    
  } ## EO else if race
  
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

## SO RELATIONSHIPS PLOTS ----

### SO race wrangling ----
# race wrangling used for all race relationship plots
race_group <- c("other", "pacific_islander", "multiracial", "asian",
                "black", "white", "native_american", "hispanic_latinx")
# “high” cutoff value for all race relationship plots
data_race_quants <-
  race_group %>%
  map_dbl(race_top_quartile, acs_df = data_ca_acs_2018) %>%
  cbind("race_group" = race_group,
        "weighted_quartile" = .) %>%
  as.data.frame()

### SO education wrangling ----
# education wrangling used for all education relationship plots
education_group <- c("hs_GED_or_below", "some_college", "college", "master_or_above")
# “high” cutoff value for all education relationship plots
data_education_quants <-
  education_group %>%
  map_dbl(education_top_quartile, acs_df = data_ca_acs_2018) %>%
  cbind("education_group" = education_group,
        "weighted_quartile" = .) %>%
  as.data.frame()

### SO language wrangling ----
# language wrangling used for all language relationship plots
language_group <- c("english_only", "not_english_only")
# “high” cutoff value for all language relationship plots
data_language_quants <-
  language_group %>%
  map_dbl(language_top_quartile, acs_df = data_ca_acs_2018) %>%
  cbind("language_group" = language_group,
        "weighted_quartile" = .) %>%
  as.data.frame()

### SO median-income wrangling ----
# median-income wrangling used for all median-income relationship plots
median_income_decile_list <- median_income_deciles(acs_df = data_ca_acs_2018) %>% 
  as.list()



### SO relationships plots ----
output$data_relationships_plot <- renderPlotly({
  
  #### SO education x booking window plot function ----
  if (input$data_relationships == "Education x Booking window") {
    
    education_booking_window_plot(admin_unitInput = input$admin_unit_relationships,
                                  siteInput = input$site_relationships,
                                  education_top_quartile_df = data_education_quants,
                                  ridb_df = data_joined_2018)
    
  } # EO education x booking window
  
  #### SO education x daily cost plot function ----
  else if (input$data_relationships == "Education x Daily cost") {
    
    education_daily_cost_plot(admin_unitInput = input$admin_unit_relationships,
                              siteInput = input$site_relationships,
                              education_top_quartile_df = data_education_quants,
                              ridb_df = data_joined_2018)
    
  } # EO education x daily cost
  
  #### SO education x daily cost per visitor plot function ----
  else if (input$data_relationships == "Education x Daily cost per visitor") {
    
    education_daily_cost_per_visitor_plot(admin_unitInput = input$admin_unit_relationships,
                                          siteInput = input$site_relationships,
                                          education_top_quartile_df = data_education_quants,
                                          ridb_df = data_joined_2018)
    
  } # EO education x daily cost per visitor
  
  #### SO education x dist traveled plot function ----
  else if (input$data_relationships == "Education x Distance traveled") {
    
    education_dist_travel_plot(admin_unitInput = input$admin_unit_relationships,
                               siteInput = input$site_relationships,
                               education_top_quartile_df = data_education_quants,
                               ridb_df = data_joined_2018)
    
  } # EO education x dist traveled
  
  #### SO education x length of stay function ----
  else if (input$data_relationships == "Education x Length of stay") {
    
    education_length_of_stay_plot(admin_unitInput = input$admin_unit_relationships,
                                  siteInput = input$site_relationships,
                                  education_top_quartile_df = data_education_quants,
                                  ridb_df = data_joined_2018)
    
  } # EO education x length of stay
  
  #### SO education x site type function ----
  else if (input$data_relationships == "Education x Site type") {
    
    education_site_type_plot(admin_unitInput = input$admin_unit_relationships,
                             siteInput = input$site_relationships,
                             education_top_quartile_df = data_education_quants,
                             ridb_df = data_joined_2018,
                             site_type_string = "equestrian")
    # education_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          education_top_quartile_df = data_education_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "remote")
    # education_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          education_top_quartile_df = data_education_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "rv only")
    # education_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          education_top_quartile_df = data_education_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "rv or tent")
    # education_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          education_top_quartile_df = data_education_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "shelter")
    # education_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          education_top_quartile_df = data_education_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "tent only")
    # education_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          education_top_quartile_df = data_education_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "water")
    
  } # EO education x site type
  
  
  #### SO language x booking window plot function ----
  else if (input$data_relationships == "Language x Booking window") {
    
    language_booking_window_plot(admin_unitInput = input$admin_unit_relationships,
                                 siteInput = input$site_relationships,
                                 language_top_quartile_df = data_language_quants,
                                 ridb_df = data_joined_2018)
    
  } # EO language x booking window
  
  #### SO language x daily cost plot function ----
  else if (input$data_relationships == "Language x Daily cost") {
    
    language_daily_cost_plot(admin_unitInput = input$admin_unit_relationships,
                             siteInput = input$site_relationships,
                             language_top_quartile_df = data_language_quants,
                             ridb_df = data_joined_2018)
    
  } # EO language x daily cost
  
  #### SO language x daily cost per visitor plot function ----
  else if (input$data_relationships == "Language x Daily cost per visitor") {
    
    language_daily_cost_per_visitor_plot(admin_unitInput = input$admin_unit_relationships,
                                         siteInput = input$site_relationships,
                                         language_top_quartile_df = data_language_quants,
                                         ridb_df = data_joined_2018)
    
  } # EO language x daily cost per visitor
  
  #### SO language x dist traveled plot function ----
  else if (input$data_relationships == "Language x Distance traveled") {
    
    language_dist_travel_plot(admin_unitInput = input$admin_unit_relationships,
                              siteInput = input$site_relationships,
                              language_top_quartile_df = data_language_quants,
                              ridb_df = data_joined_2018)
    
  } # EO language x dist traveled
  
  #### SO language x length of stay function ----
  else if (input$data_relationships == "Language x Length of stay") {
    
    language_length_of_stay_plot(admin_unitInput = input$admin_unit_relationships,
                                 siteInput = input$site_relationships,
                                 language_top_quartile_df = data_language_quants,
                                 ridb_df = data_joined_2018)
    
  } # EO language x length of stay
  
  #### SO language x site type function ----
  else if (input$data_relationships == "Language x Site type") {
    
    language_site_type_plot(admin_unitInput = input$admin_unit_relationships,
                            siteInput = input$site_relationships,
                            language_top_quartile_df = data_language_quants,
                            ridb_df = data_joined_2018,
                            site_type_string = "equestrian")
    # language_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          language_top_quartile_df = data_language_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "remote")
    # language_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          language_top_quartile_df = data_language_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "rv only")
    # language_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          language_top_quartile_df = data_language_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "rv or tent")
    # language_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          language_top_quartile_df = data_language_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "shelter")
    # language_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          language_top_quartile_df = data_language_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "tent only")
    # language_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          language_top_quartile_df = data_language_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "water")
    
  } # EO language x site type
  
  
  #### SO median-income x booking window plot function ----
  else if (input$data_relationships == "Median-income x Booking window") {
    
    median_income_booking_window_plot(admin_unitInput = input$admin_unit_relationships,
                                 siteInput = input$site_relationships,
                                 ridb_df = data_joined_2018,
                                 median_income_binned = median_income_decile_list)
    
  } # EO median-income x booking window
  
  #### SO median-income x daily cost plot function ----
  else if (input$data_relationships == "Median-income x Daily cost") {
    
    median_income_daily_cost_plot(admin_unitInput = input$admin_unit_relationships,
                             siteInput = input$site_relationships,
                             ridb_df = data_joined_2018,
                             median_income_binned = median_income_decile_list)
    
  } # EO median-income x daily cost
  
  #### SO median-income x daily cost per visitor plot function ----
  else if (input$data_relationships == "Median-income x Daily cost per visitor") {
    
    median_income_daily_cost_per_visitor_plot(admin_unitInput = input$admin_unit_relationships,
                                         siteInput = input$site_relationships,
                                         ridb_df = data_joined_2018,
                                         median_income_binned = median_income_decile_list)
    
  } # EO median-income x daily cost per visitor
  
  #### SO median-income x dist traveled plot function ----
  else if (input$data_relationships == "Median-income x Distance traveled") {
    
    median_income_dist_travel_plot(admin_unitInput = input$admin_unit_relationships,
                              siteInput = input$site_relationships,
                              ridb_df = data_joined_2018,
                              median_income_binned = median_income_decile_list)
    
  } # EO median-income x dist traveled
  
  #### SO median-income x length of stay function ----
  else if (input$data_relationships == "Median-income x Length of stay") {
    
    median_income_length_of_stay_plot(admin_unitInput = input$admin_unit_relationships,
                                 siteInput = input$site_relationships,
                                 ridb_df = data_joined_2018,
                                 median_income_binned = median_income_decile_list)
    
  } # EO median-income x length of stay
  
  #### SO median-income x site type function ----
  else if (input$data_relationships == "Median-income x Site type") {
    
    median_income_site_type_plot(admin_unitInput = input$admin_unit_relationships,
                            siteInput = input$site_relationships,
                            ridb_df = data_joined_2018,
                            median_income_binned = median_income_decile_list,
                            site_type_string = "equestrian")
    # median_income_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          ridb_df = data_joined_2018,
    #                          median_income_binned = median_income_decile_list,
    #                          site_type_string = "remote")
    # median_income_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          ridb_df = data_joined_2018,
    #                          median_income_binned = median_income_decile_list,
    #                          site_type_string = "rv only")
    # median_income_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          ridb_df = data_joined_2018,
    #                          median_income_binned = median_income_decile_list,
    #                          site_type_string = "rv or tent")
    # median_income_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          ridb_df = data_joined_2018,
    #                          median_income_binned = median_income_decile_list,
    #                          site_type_string = "shelter")
    # median_income_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          ridb_df = data_joined_2018,
    #                          median_income_binned = median_income_decile_list,
    #                          site_type_string = "tent only")
    # median_income_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          ridb_df = data_joined_2018,
    #                          median_income_binned = median_income_decile_list,
    #                          site_type_string = "water")
    
  } # EO median-income x site type
  
  
  #### SO race x booking window plot function ----
  if (input$data_relationships == "Race x Booking window") {
    
    race_booking_window_plot(admin_unitInput = input$admin_unit_relationships,
                                  siteInput = input$site_relationships,
                                  race_top_quartile_df = data_race_quants,
                                  ridb_df = data_joined_2018)
    
  } # EO race x booking window
  
  #### SO race x daily cost plot function ----
  else if (input$data_relationships == "Race x Daily cost") {
    
    race_daily_cost_plot(admin_unitInput = input$admin_unit_relationships,
                              siteInput = input$site_relationships,
                              race_top_quartile_df = data_race_quants,
                              ridb_df = data_joined_2018)
    
  } # EO race x daily cost
  
  #### SO race x daily cost per visitor plot function ----
  else if (input$data_relationships == "Race x Daily cost per visitor") {
    
    race_daily_cost_per_visitor_plot(admin_unitInput = input$admin_unit_relationships,
                                          siteInput = input$site_relationships,
                                          race_top_quartile_df = data_race_quants,
                                          ridb_df = data_joined_2018)
    
  } # EO race x daily cost per visitor
  
  #### SO race x dist traveled plot function ----
  else if (input$data_relationships == "Race x Distance traveled") {
    
    race_dist_travel_plot(admin_unitInput = input$admin_unit_relationships,
                               siteInput = input$site_relationships,
                               race_top_quartile_df = data_race_quants,
                               ridb_df = data_joined_2018)
    
  } # EO race x dist traveled
  
  #### SO race x length of stay function ----
  else if (input$data_relationships == "Race x Length of stay") {
    
    race_length_of_stay_plot(admin_unitInput = input$admin_unit_relationships,
                                  siteInput = input$site_relationships,
                                  race_top_quartile_df = data_race_quants,
                                  ridb_df = data_joined_2018)
    
  } # EO race x length of stay
  
  #### SO race x site type function ----
  else if (input$data_relationships == "Race x Site type") {
    
    race_site_type_plot(admin_unitInput = input$admin_unit_relationships,
                             siteInput = input$site_relationships,
                             race_top_quartile_df = data_race_quants,
                             ridb_df = data_joined_2018,
                             site_type_string = "equestrian")
    # race_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          race_top_quartile_df = data_race_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "remote")
    # race_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          race_top_quartile_df = data_race_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "rv only")
    # race_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          race_top_quartile_df = data_race_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "rv or tent")
    # race_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          race_top_quartile_df = data_race_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "shelter")
    # race_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          race_top_quartile_df = data_race_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "tent only")
    # race_site_type_plot(admin_unitInput = input$admin_unit_relationships,
    #                          siteInput = input$site_relationships,
    #                          race_top_quartile_df = data_race_quants,
    #                          ridb_df = data_joined_2018,
    #                          site_type_string = "water")
    
  } # EO race x site type

}) # EO relationships plots 

## SO VISITORSHEDS PLOTS YES REACTIVE ----
### yosemite plot ----
output$caVisitorshed_plot <- renderTmap({
  
  tm_shape(data_zip_geometries_ca_simple) +
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
  
  tm_shape(data_geometries_us_simple) +
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
  
  DT::datatable(data_download_dt(),
                extensions = 'Buttons',
                options = list(
                  server = FALSE,
                  paging = TRUE,
                  pageLength = 12,
                  buttons = c('csv', 'excel'),
                  dom = 'Bfrtip',
                  columnDefs = list(visible = TRUE)
                ))
  
}) # EO render data table



} ## EO server

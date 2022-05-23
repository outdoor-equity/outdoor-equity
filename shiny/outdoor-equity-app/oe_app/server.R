# server instructions ----
server <- function(input, output, session) {
  
  # OBSERVE EVENTS ----
  ## SO OE press agency ----
  ### summary box 1 ----
  observeEvent(input$agency_summary_1, {
    # function to call values based on key from agency to admin dict
    oe_agency_to_admin_dict(isInput_key = input$agency_summary_1,
                            page = "agency_summary_1")
    
    # update input with new choices
    updateSelectizeInput(session, "admin_unit_summary_1",
                         choices = sort(choices))
  }) ## EO OE press agency on summary page box 1
  
  ### summary box 2 ----
  observeEvent(input$agency_summary_2, {
    # function to call values based on key from agency to admin dict
    oe_agency_to_admin_dict(isInput_key = input$agency_summary_2,
                            page = "agency_summary_2")
    
    # update input with new choices
    updateSelectizeInput(session, "admin_unit_summary_2",
                         choices = sort(choices))
  }) ## EO OE press agency on summary page box 2
  
  
  ### summary box 3 ----
  observeEvent(input$agency_summary_3, {
    # function to call values based on key from agency to admin dict
    oe_agency_to_admin_dict(isInput_key = input$agency_summary_3,
                            page = "agency_summary_3")
    
    # update input with new choices
    updateSelectizeInput(session, "admin_unit_summary_3",
                         choices = sort(choices))
  }) ## EO OE press agency on summary page box 3
  
  ### summary box 4 ----
  observeEvent(input$agency_summary_4, {
    # function to call values based on key from agency to admin dict
    oe_agency_to_admin_dict(isInput_key = input$agency_summary_4,
                            page = "agency_summary_4")
    
    # update input with new choices
    updateSelectizeInput(session, "admin_unit_summary_4",
                         choices = sort(choices))
  }) ## EO OE press agency on summary page box 4
  
  
  ### relationships page ----
  observeEvent(input$agency_relationships, {
    # function to call values based on key from agency to admin dict
    oe_agency_to_admin_dict(isInput_key = input$agency_relationships,
                            page = "agency_relationships")
    
    # update input with new choices
    updateSelectizeInput(session, "admin_unit_relationships",
                         choices = sort(choices))
  }) ## EO OE press agency on relationships page
  
  ### visitorsheds page ----
  observeEvent(input$agency_visitorsheds, {
    # function to call values based on key from agency to admin dict
    oe_agency_to_admin_dict(isInput_key = input$agency_visitorsheds,
                            page = "agency_visitorsheds")
    
    # update input with new choices
    updateSelectizeInput(session, "admin_unit_visitorsheds",
                         choices = sort(choices))
  }) ## EO OE press agency on visitorsheds page
  
  ### data download page ----
  observeEvent(input$agency_data_download, {
    # function to call values based on key from agency to admin dict
    oe_agency_to_admin_dict(isInput_key = input$agency_data_download,
                            page = "agency_data_download")
    
    # update input with new choices
    updateSelectizeInput(session, "admin_unit_data_download",
                         choices = sort(choices))
  }) ## EO press agency on data download page
  
  
  ## SO OE press admin unit ----
  ### summary box 1 ----
  observeEvent(input$admin_unit_summary_1, {
    # function to call values based on key from agency to admin dict
    oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary_1,
                               page = "admin_unit_summary_1")
    
    # update input with new choices
    updateSelectizeInput(session, "site_summary_1",
                         choices = sort(choices))
  }) ## EO press admin unit on summary page box 1
  
  ### summary box 2 ----
  observeEvent(input$admin_unit_summary_2, {
    # function to call values based on key from agency to admin dict
    oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary_2,
                               page = "admin_unit_summary_2")
    
    # update input with new choices
    updateSelectizeInput(session, "site_summary_2",
                         choices = sort(choices))
  }) ## EO press admin unit on summary page box 2
  
  ### summary box 3 ----
  observeEvent(input$admin_unit_summary_3, {
    # function to call values based on key from agency to admin dict
    oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary_3,
                               page = "admin_unit_summary_3")
    
    # update input with new choices
    updateSelectizeInput(session, "site_summary_3",
                         choices = sort(choices))
  }) ## EO press admin unit on summary page box 3
  
  ### summary box 4 ----
  observeEvent(input$admin_unit_summary_4, {
    # function to call values based on key from agency to admin dict
    oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_summary_4,
                               page = "admin_unit_summary_4")
    
    # update input with new choices
    updateSelectizeInput(session, "site_summary_4",
                         choices = sort(choices))
  }) ## EO press admin unit on summary page box 4
  
  
  
  ### relationships page ----
  observeEvent(input$admin_unit_relationships, {
    # function to call values based on key from agency to admin dict
    oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_relationships,
                               page = "admin_unit_relationships")
    
    # update input with new choices
    updateSelectizeInput(session, "site_relationships",
                         choices = sort(choices))
  }) ## EO press admin unit on relationships page
  
  
  ### visitorsheds page ----
  observeEvent(input$admin_unit_visitorsheds, {
    # function to call values based on key from agency to admin dict
    oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_visitorsheds,
                               page = "admin_unit_visitorsheds")
    
    # update input with new choices
    updateSelectizeInput(session, "site_visitorsheds",
                         choices = sort(choices))
  }) ## EO press admin unit on visitorsheds page
  
  ### data download page ----
  observeEvent(input$admin_unit_data_download, {
    # function to call values based on key from agency to admin dict
    oe_admin_unit_to_site_dict(isInput_key = input$admin_unit_data_download,
                               page = "admin_unit_data_download")
    
    # update input with new choices
    updateSelectizeInput(session, "site_data_download",
                         choices = sort(choices))
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
  
  ## SO OE hide / show summary_captions ----
  observeEvent(input$data_summary, {
    if (input$data_summary %in% c("distance_traveled_mi",
                                  "daily_cost",
                                  "daily_cost_per_visitor",
                                  "length_of_stay",
                                  "aggregated_site_type",
                                  "race",
                                  "education")) {
      
      shinyjs::hide(id = "summary_captions")
    }
    else if (input$data_summary %in% c("booking_window",
                                       "median_income",
                                       "not_english_only")) {
      
      shinyjs::show(id = "summary_captions")
    }
  }) ## EO OE hide / show summary_captions
  
  ## SO OE press site relationships ----
  observeEvent(input$data_relationships, {
    if (input$data_relationships %in% c("Race x Site type",
                                        "Education x Site type",
                                        "Language x Site type",
                                        "Median-income x Site type")) {
      
      shinyjs::hide(id = "data_relationships_plot") 
      shinyjs::hide(id = "high_relationships_output")
      
      shinydashboardPlus::updateBox(id = "relationships_outputs",
                                    action = "update",
                                    options = list(width = 12))
    }
    
    else if (input$data_relationships != c("Race x Site type",
                                           "Education x Site type",
                                           "Language x Site type",
                                           "Median-income x Site type")) {
      
      shinyjs::show(id = "data_relationships_plot")
      shinyjs::show(id = "high_relationships_output")
      
      shinydashboardPlus::updateBox(id = "relationships_outputs",
                                    action = "update",
                                    options = list(width = 8))
      
    }
  }) ## EO OE press site relationships
  
  # RENDER PLOTS ----
  
  
  ## SO ABOUT PAGE PLOTS ----
  
  ### SO when to book ----
  output$about_example_1_plot <- renderPlotly({
    not_reactive_booking_window_plot(admin_unit = "Yosemite National Park", 
                                     site = "Upper Pines", 
                                     ridb_df = data_joined_2018)
  }) # OE when to book
  
  ### SO distance traveled by race ----
  output$about_example_2_plot <- renderPlotly({
    not_reactive_race_dist_travel_plot(admin_unit = "Yosemite National Park", 
                                       site = "Upper Pines", 
                                       ridb_df = data_joined_2018,
                                       race_top_quartile_df = data_race_quants)
  }) # EO distance traveled by race
  
  ### SO distance traveled by income ----
  output$about_example_3_plot <- renderPlotly({
    not_reactive_median_income_dist_travel_plot(admin_unit = "Yosemite National Park", 
                                                site = "Upper Pines", 
                                                ridb_df = data_joined_2018,
                                                median_income_binned = median_income_decile_list)
  }) # OE distance traveled by income
  
  ### SO distance traveled by income ----
  output$about_example_4_plot <- renderTmap({
    not_reactive_state_visitorshed_map(site = "Upper Pines", 
                                       ridb_df = data_joined_2018,
                                       state_geometries_df = data_state_geometries)
  }) # OE distance traveled by income
  

  
  ## SO DATA SUMMARY PLOTS 1 ----
  output$data_summary_plot_1 <- renderPlotly({
    ### SO distance traveled ----
    if (input$data_summary == "distance_traveled_mi") {
      dist_travel_plot(
        admin_unitInput = input$admin_unit_summary_1,
        siteInput = input$site_summary_1,
        ridb_df = data_joined_2018
      )
      
    } ## EO if distance traveled
    
    ## SO booking window ----
    else if (input$data_summary == "booking_window") {
      booking_window_plot(
        admin_unitInput = input$admin_unit_summary_1,
        siteInput = input$site_summary_1,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if booking window
    
    ## SO daily cost ----
    else if (input$data_summary == "daily_cost") {
      daily_cost_plot(
        admin_unitInput = input$admin_unit_summary_1,
        siteInput = input$site_summary_1,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if daily cost
    
    ## SO daily cost per visitor ----
    else if (input$data_summary == "daily_cost_per_visitor") {
      daily_cost_visitor_plot(
        admin_unitInput = input$admin_unit_summary_1,
        siteInput = input$site_summary_1,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if daily cost per visitor
    
    ## SO education ----
    else if (input$data_summary == "education") {
      education_plot(
        admin_unitInput = input$admin_unit_summary_1,
        siteInput = input$site_summary_1,
        ridb_df = data_joined_2018
      )
      
    } ## EO education
    
    ## SO length of stay ----
    else if (input$data_summary == "length_of_stay") {
      length_of_stay_plot(
        admin_unitInput = input$admin_unit_summary_1,
        siteInput = input$site_summary_1,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if length of stay
    
    ## SO site type ----
    else if (input$data_summary == "aggregated_site_type") {
      site_type_plot(
        admin_unitInput = input$admin_unit_summary_1,
        siteInput = input$site_summary_1,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if site type
    
    ## SO race ----
    else if (input$data_summary == "race") {
      race_plot(
        admin_unitInput = input$admin_unit_summary_1,
        siteInput = input$site_summary_1,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if race
    
    
    ## SO median income ----
    else if (input$data_summary == "median_income") {
      median_income_plot(
        admin_unitInput = input$admin_unit_summary_1,
        siteInput = input$site_summary_1,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if median income
    
    ## SO language ----
    else if (input$data_summary == "not_english_only") {
      language_plot(
        admin_unitInput = input$admin_unit_summary_1,
        siteInput = input$site_summary_1,
        ridb_df = data_joined_2018
      )
      
    } ## EO language
    
  }) ## EO data summary plots
  
  
  ## DATA SUMMARY PLOTS 2 ----
  output$data_summary_plot_2 <- renderPlotly({
    ### SO distance traveled ----
    if (input$data_summary == "distance_traveled_mi") {
      dist_travel_plot(
        admin_unitInput = input$admin_unit_summary_2,
        siteInput = input$site_summary_2,
        ridb_df = data_joined_2018
      )
      
    } ## EO if distance traveled
    
    ## SO booking window ----
    else if (input$data_summary == "booking_window") {
      booking_window_plot(
        admin_unitInput = input$admin_unit_summary_2,
        siteInput = input$site_summary_2,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if booking window
    
    ## SO daily cost ----
    else if (input$data_summary == "daily_cost") {
      daily_cost_plot(
        admin_unitInput = input$admin_unit_summary_2,
        siteInput = input$site_summary_2,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if daily cost
    
    ## SO daily cost per visitor ----
    else if (input$data_summary == "daily_cost_per_visitor") {
      daily_cost_visitor_plot(
        admin_unitInput = input$admin_unit_summary_2,
        siteInput = input$site_summary_2,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if daily cost per visitor
    
    ## SO education ----
    else if (input$data_summary == "education") {
      education_plot(
        admin_unitInput = input$admin_unit_summary_2,
        siteInput = input$site_summary_2,
        ridb_df = data_joined_2018
      )
      
    } ## EO education
    
    ## SO length of stay ----
    else if (input$data_summary == "length_of_stay") {
      length_of_stay_plot(
        admin_unitInput = input$admin_unit_summary_2,
        siteInput = input$site_summary_2,
        ridb_df = data_joined_2018
      )
      
    } # EO length of stay
    
    ## SO site type ----
    else if (input$data_summary == "aggregated_site_type") {
      site_type_plot(
        admin_unitInput = input$admin_unit_summary_2,
        siteInput = input$site_summary_2,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if site type
    
    ## SO race ----
    else if (input$data_summary == "race") {
      race_plot(
        admin_unitInput = input$admin_unit_summary_2,
        siteInput = input$site_summary_2,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if race
    
    ## SO median income ----
    else if (input$data_summary == "median_income") {
      median_income_plot(
        admin_unitInput = input$admin_unit_summary_2,
        siteInput = input$site_summary_2,
        ridb_df = data_joined_2018
      )
      
    } ## EO else if median income
    
    ## SO language ----
    else if (input$data_summary == "not_english_only") {
      language_plot(
        admin_unitInput = input$admin_unit_summary_2,
        siteInput = input$site_summary_2,
        ridb_df = data_joined_2018
      )
    } ## EO language
    
  }) ## EO DATA SUMMARY PLOTS 2
  
  
  # DATA SUMMARY CAPTIONS ----
  output$data_summary_captions <- renderText({
    ## SO median income ----
    if (input$data_summary == "median_income") {
      "The shape of the curve can provide insight on the distribution of 
      median-incomes. Taller peaks indicate a concentration of more people at a 
      given median-income level, while flatter curves indicate a more even 
      distribution. Comparing the green curve (all visitors to this site) to the 
      grey curve (all California residents), can show who is more or less 
      represented at this site compared to the California census."
    } ## EO else if median income
    
    ## SO language ----
    else if (input$data_summary == "not_english_only") {
      "The shape of the curve can provide insight on the distribution of the 
      percentage of people in a community who speak languages other than English 
      at home. Taller peaks indicate a concentration of more people living in 
      communities at a given percent level, while flatter curves indicate a more 
      even distribution. Comparing the green curve (all visitors to this site) 
      to the grey curve (all California residents), can show who is more or less 
      represented at this site compared to the CA census."
    } ## EO language
    
    ## SO booking window ----
    else if (input$data_summary == "booking_window") {
      paste("Booking windows vary by location. To check season dates and when sites 
      are available to reserve look for the Season and Fees tab within a 
      site-specific page on [Recreation.gov](https://www.recreation.gov/).", ca_agency[[1]])
    } ## EO booking window
    
  }) ## EO DATA SUMMARY CAPTIONS
  
  
  ## SO RELATIONSHIPS PLOTS ----
  
  ### SO race wrangling ----
  # race wrangling used for all race relationship plots
  race_group <- c(
    "other",
    "pacific_islander",
    "multiracial",
    "asian",
    "black",
    "white",
    "native_american",
    "hispanic_latinx"
  )
  # “high” cutoff value for all race relationship plots
  data_race_quants <-
    race_group %>%
    map_dbl(race_top_quartile, acs_df = data_ca_acs_2018) %>%
    cbind("race_group" = race_group,
          "weighted_quartile" = .) %>%
    as.data.frame()
  
  ### SO education wrangling ----
  # education wrangling used for all education relationship plots
  education_group <-
    c("hs_GED_or_below",
      "some_college",
      "college",
      "master_or_above")
  # “high” cutoff value for all education relationship plots
  data_education_quants <-
    education_group %>%
    map_dbl(education_top_quartile, acs_df = data_ca_acs_2018) %>%
    cbind("education_group" = education_group,
          "weighted_quartile" = .) %>%
    as.data.frame()
  
  print(paste("data education quants df:", data_education_quants))
  
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
  median_income_decile_list <-
    median_income_deciles(acs_df = data_ca_acs_2018) %>%
    as.list()
  
  
  ### SO relationships plots ----
  output$data_relationships_plot <- renderPlotly({
    #### SO education x booking window plot function ----
    if (input$data_relationships == "Education x Booking window") {
      
      education_booking_window_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018
      )
      
    } # EO education x booking window
    
    ### SO education x daily cost plot function ----
    else if (input$data_relationships == "Education x Daily cost") {
      
      education_daily_cost_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018
      )
      
    } # EO education x daily cost

    ### SO education x daily cost per visitor plot function ----
    else if (input$data_relationships == "Education x Daily cost per visitor") {
      
      education_daily_cost_per_visitor_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018
      )

    } # EO education x daily cost per visitor

    ### SO education x dist traveled plot function ----
    else if (input$data_relationships == "Education x Distance traveled") {
      
      education_dist_travel_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018
      )

    } # EO education x dist traveled

    ### SO education x length of stay function ----
    else if (input$data_relationships == "Education x Length of stay") {
      
      education_length_of_stay_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018
      )

    } # EO education x length of stay

    ### SO language x booking window plot function ----
    else if (input$data_relationships == "Language x Booking window") {
      
      language_booking_window_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018
      )

    } # EO language x booking window
    
    ### SO language x daily cost plot function ----
    else if (input$data_relationships == "Language x Daily cost") {
      
      language_daily_cost_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018
      )

    } # EO language x daily cost

    ### SO language x daily cost per visitor plot function ----
    else if (input$data_relationships == "Language x Daily cost per visitor") {
      
      language_daily_cost_per_visitor_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018
      )

    } # EO language x daily cost per visitor

    ### SO language x dist traveled plot function ----
    else if (input$data_relationships == "Language x Distance traveled") {
      
      language_dist_travel_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018
      )

    } # EO language x dist traveled

    ### SO language x length of stay function ----
    else if (input$data_relationships == "Language x Length of stay") {
      
      language_length_of_stay_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018
      )

    } # EO language x length of stay

    ### SO median-income x booking window plot function ----
    else if (input$data_relationships == "Median-income x Booking window") {
      
      median_income_booking_window_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list
      )

    } # EO median-income x booking window

    ### SO median-income x daily cost plot function ----
    else if (input$data_relationships == "Median-income x Daily cost") {
      
      median_income_daily_cost_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list
      )

    } # EO median-income x daily cost

    ### SO median-income x daily cost per visitor plot function ----
    else if (input$data_relationships == "Median-income x Daily cost per visitor") {
      
      median_income_daily_cost_per_visitor_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list
      )

    } # EO median-income x daily cost per visitor

    ### SO median-income x dist traveled plot function ----
    else if (input$data_relationships == "Median-income x Distance traveled") {
      
      median_income_dist_travel_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list
      )

    } # EO median-income x dist traveled

    ### SO median-income x length of stay function ----
    else if (input$data_relationships == "Median-income x Length of stay") {
      
      median_income_length_of_stay_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list
      )

    } # EO median-income x length of stay

    ### SO race x booking window plot function ----
    else if (input$data_relationships == "Race x Booking window") {

      race_booking_window_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018
      )

    } # EO race x booking window

    ### SO race x daily cost plot function ----
    else if (input$data_relationships == "Race x Daily cost") {

      race_daily_cost_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018
      )

    } # EO race x daily cost

    ### SO race x daily cost per visitor plot function ----
    else if (input$data_relationships == "Race x Daily cost per visitor") {
      
      race_daily_cost_per_visitor_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018
      )

    } # EO race x daily cost per visitor

    ### SO race x dist traveled plot function ----
    else if (input$data_relationships == "Race x Distance traveled") {
      
      race_dist_travel_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018
      )

    } # EO race x dist traveled

    ### SO race x length of stay function ----
    else if (input$data_relationships == "Race x Length of stay") {
      
      race_length_of_stay_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018
      )

    } # EO race x length of stay
  }) # EO relationships plots
  
  ### SO relationships high plots ----
  output$high_relationships_plot <- renderPlotly({
    
    #### SO race quartile ----
    if (input$data_relationships %in% c("Race x Booking window",
                                        "Race x Daily cost",
                                        "Race x Daily cost per visitor",
                                        "Race x Distance traveled",
                                        "Race x Length of stay",
                                        "Race x Site type")) {
      
      race_top_quartile_res_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018
      )
      
    } # EO race quartile 
    
    ### SO education quartile ----
    else if (input$data_relationships %in% c("Education x Booking window",
                                             "Education x Daily cost",
                                             "Education x Daily cost per visitor",
                                             "Education x Distance traveled",
                                             "Education x Length of stay",
                                             "Education x Site type")) {
      
      education_top_quartile_res_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018
      )
      
    } # EO education quartile
    
    ### SO language quartile ----
    else if (input$data_relationships %in% c("Language x Booking window",
                                             "Language x Daily cost",
                                             "Language x Daily cost per visitor",
                                             "Language x Distance traveled",
                                             "Language x Length of stay",
                                             "Language x Site type")) {
      
      language_top_quartile_res_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018
      )
      
    } # EO language quartile
    
    ### SO median-income quartile ----
    else if (input$data_relationships %in% c("Median-income x Booking window",
                                             "Median-income x Daily cost",
                                             "Median-income x Daily cost per visitor",
                                             "Median-income x Distance traveled",
                                             "Median-income x Length of stay",
                                             "Median-income x Site type")) {
      
      median_income_top_quartile_res_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        median_income_binned = median_income_decile_list,
        ridb_df = data_joined_2018
      )
      
    } # EO median-income quartile
    
  }) # EO relationships high plots 
  
  
  ### SO RENDER UI relationships ----
  #SO education site type relationships render ui
  output$relationships_tab_layout <- renderUI({
    
    #### SO education x site ----
    if (input$data_relationships == "Education x Site type"){
    
    tabsetPanel(
      tabPanel(title = "Equestrian",
               plotlyOutput(outputId = "edu_site_equestrian_plot") %>%
                 withSpinner(color = spinner_color)
      ), # EO tabPanel edu x equestrian
      
      tabPanel(title = "Remote",
               plotlyOutput(outputId = "edu_site_remote_plot") %>%
                 withSpinner(color = spinner_color)
               ), # EO tabPanel edu x remote
      
      tabPanel(title = "RV only",
               plotlyOutput(outputId = "edu_site_rvOnly_plot") %>%
                 withSpinner(color = spinner_color)
      ), # EO tabPanel edu x rv only
      
      tabPanel(title = "RV or Tent",
               plotlyOutput(outputId = "edu_site_rvTent_plot") %>%
                 withSpinner(color = spinner_color)
      ), # EO tabPanel edu x rv or tent
      
      tabPanel(title = "Shelter",
               plotlyOutput(outputId = "edu_site_shelter_plot") %>%
                 withSpinner(color = spinner_color)
      ), # EO tabPanel edu x shelter
      
      tabPanel(title = "Tent only",
               plotlyOutput(outputId = "edu_site_tentOnly_plot") %>%
                 withSpinner(color = spinner_color)
      ), # EO tabPanel edu x tent only
      
      tabPanel(title = "Water",
               plotlyOutput(outputId = "edu_site_water_plot") %>%
                 withSpinner(color = spinner_color)
      ), # EO tabPanel edu x tent only
      
    ) # EO edu tabsetPanel
    } # EO if edu statement
    
    ### SO language x site ----
    else if (input$data_relationships == "Language x Site type"){
      
      tabsetPanel(
        tabPanel(title = "Equestrian",
                 plotlyOutput(outputId = "lang_site_equestrian_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel language x equestrian
        
        tabPanel(title = "Remote",
                 plotlyOutput(outputId = "lang_site_remote_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel language x remote
        
        tabPanel(title = "RV only",
                 plotlyOutput(outputId = "lang_site_rvOnly_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel language x rv only
        
        tabPanel(title = "RV or Tent",
                 plotlyOutput(outputId = "lang_site_rvTent_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel language x rv or tent
        
        tabPanel(title = "Shelter",
                 plotlyOutput(outputId = "lang_site_shelter_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel language x shelter
        
        tabPanel(title = "Tent only",
                 plotlyOutput(outputId = "lang_site_tentOnly_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel language x tent only
        
        tabPanel(title = "Water",
                 plotlyOutput(outputId = "lang_site_water_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel language x tent only
        
      ) # EO language tabsetPanel
    } # EO else if language statement
    
    ### SO median-income x site ----
    else if (input$data_relationships == "Median-income x Site type"){
      
      tabsetPanel(
        tabPanel(title = "Equestrian",
                 plotlyOutput(outputId = "medInc_site_equestrian_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel median-income x equestrian
        
        tabPanel(title = "Remote",
                 plotlyOutput(outputId = "medInc_site_remote_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel median-income x remote
        
        tabPanel(title = "RV only",
                 plotlyOutput(outputId = "medInc_site_rvOnly_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel median-income x rv only
        
        tabPanel(title = "RV or Tent",
                 plotlyOutput(outputId = "medInc_site_rvTent_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel median-income x rv or tent
        
        tabPanel(title = "Shelter",
                 plotlyOutput(outputId = "medInc_site_shelter_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel median-income x shelter
        
        tabPanel(title = "Tent only",
                 plotlyOutput(outputId = "medInc_site_tentOnly_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel median-income x tent only
        
        tabPanel(title = "Water",
                 plotlyOutput(outputId = "medInc_site_water_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel median-income x tent only
        
      ) # EO median-income tabsetPanel
    } # EO else if median-income statement
    
    ### SO race x site ----
    else if (input$data_relationships == "Race x Site type"){
      
      tabsetPanel(
        tabPanel(title = "Equestrian",
                 plotlyOutput(outputId = "race_site_equestrian_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel race x equestrian
        
        tabPanel(title = "Remote",
                 plotlyOutput(outputId = "race_site_remote_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel race x remote
        
        tabPanel(title = "RV only",
                 plotlyOutput(outputId = "race_site_rvOnly_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel race x rv only
        
        tabPanel(title = "RV or Tent",
                 plotlyOutput(outputId = "race_site_rvTent_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel race x rv or tent
        
        tabPanel(title = "Shelter",
                 plotlyOutput(outputId = "race_site_shelter_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel race x shelter
        
        tabPanel(title = "Tent only",
                 plotlyOutput(outputId = "race_site_tentOnly_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel race x tent only
        
        tabPanel(title = "Water",
                 plotlyOutput(outputId = "race_site_water_plot") %>%
                   withSpinner(color = spinner_color)
        ), # EO tabPanel race x tent only
        
      ) # EO race tabsetPanel
    } # EO else if race statement
    
  }) # EO data relationships render ui
  
  
  ### SO SITE relationships plots ----
  #### SO education x site ----
  # SO edu x equestrian relationships plot
  output$edu_site_equestrian_plot <- renderPlotly({
    
    if (input$data_relationships == "Education x Site type") {
      
      education_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018,
        site_type_string = "equestrian"
      )
    }
    
    # Note (HD): Maybe need to add an else if here? To make it empty?
    
  }) # EO SITE edu x equestrian relationships plot
  
  # SO edu x remote relationships plot
  output$edu_site_remote_plot <- renderPlotly({
    
    if (input$data_relationships == "Education x Site type") {
      
      education_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018,
        site_type_string = "remote"
      )
    }
    
  }) # EO SITE edu x remote relationships plot
  
  # SO edu x rv only relationships plot
  output$edu_site_rvOnly_plot <- renderPlotly({
    
    if (input$data_relationships == "Education x Site type") {
      
      education_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018,
        site_type_string = "rv only"
      )
    }
    
  }) # EO SITE edu x rv only relationships plot
  
  # SO edu x rv or tent relationships plot
  output$edu_site_rvTent_plot <- renderPlotly({
    
    if (input$data_relationships == "Education x Site type") {
      
      education_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018,
        site_type_string = "rv or tent"
      )
    }
    
  }) # EO SITE edu x rv or tent relationships plot
  
  # SO edu x shelter relationships plot
  output$edu_site_shelter_plot <- renderPlotly({
    
    if (input$data_relationships == "Education x Site type") {
      
      education_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018,
        site_type_string = "shelter"
      )
    }
    
  }) # EO SITE edu x shelter relationships plot
  
  # SO edu x tent only relationships plot
  output$edu_site_tentOnly_plot <- renderPlotly({
    
    if (input$data_relationships == "Education x Site type") {
      
      education_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018,
        site_type_string = "tent only"
      )
    }
    
  }) # EO SITE edu x tent only relationships plot
  
  # SO edu x water relationships plot
  output$edu_site_water_plot <- renderPlotly({
    
    if (input$data_relationships == "Education x Site type") {
      
      education_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        education_top_quartile_df = data_education_quants,
        ridb_df = data_joined_2018,
        site_type_string = "water"
      )
    }
    
  }) # EO SITE edu x water relationships plot
  
  #### SO language x site ----
  # SO language x equestrian relationships plot
  output$lang_site_equestrian_plot <- renderPlotly({
    
    if (input$data_relationships == "Language x Site type") {
      
      language_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018,
        site_type_string = "equestrian"
      )
    }
    
  }) # EO SITE language x equestrian relationships plot
  
  # SO language x remote relationships plot
  output$lang_site_remote_plot <- renderPlotly({
    
    if (input$data_relationships == "Language x Site type") {
      
      language_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018,
        site_type_string = "remote"
      )
    }
    
  }) # EO SITE language x remote relationships plot
  
  # SO language x rv only relationships plot
  output$lang_site_rvOnly_plot <- renderPlotly({
    
    if (input$data_relationships == "Language x Site type") {
      
      language_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018,
        site_type_string = "rv only"
      )
    }
    
  }) # EO SITE language x rv only relationships plot
  
  # SO language x rv or tent relationships plot
  output$lang_site_rvTent_plot <- renderPlotly({
    
    if (input$data_relationships == "Language x Site type") {
      
      language_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018,
        site_type_string = "rv or tent"
      )
    }
    
  }) # EO SITE language x rv or tent relationships plot
  
  # SO language x shelter relationships plot
  output$lang_site_shelter_plot <- renderPlotly({
    
    if (input$data_relationships == "Language x Site type") {
      
      language_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018,
        site_type_string = "shelter"
      )
    }
    
  }) # EO SITE language x shelter relationships plot
  
  # SO language x tent only relationships plot
  output$lang_site_tentOnly_plot <- renderPlotly({
    
    if (input$data_relationships == "Language x Site type") {
      
      language_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018,
        site_type_string = "tent only"
      )
    }
    
  }) # EO SITE language x tent only relationships plot
  
  # SO language x water relationships plot
  output$lang_site_water_plot <- renderPlotly({
    
    if (input$data_relationships == "Language x Site type") {
      
      language_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        language_top_quartile_df = data_language_quants,
        ridb_df = data_joined_2018,
        site_type_string = "water"
      )
    }
    
  }) # EO SITE language x water relationships plot
  
  #### SO median-income x site ----
  # SO median-income x equestrian relationships plot
  output$medInc_site_equestrian_plot <- renderPlotly({
    
    if (input$data_relationships == "Median-income x Site type") {
      
      median_income_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list,
        site_type_string = "equestrian"
      )
    }
    
  }) # EO SITE median-income x equestrian relationships plot
  
  # SO median-income x remote relationships plot
  output$medInc_site_remote_plot <- renderPlotly({
    
    if (input$data_relationships == "Median-income x Site type") {
      
      median_income_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list,
        site_type_string = "remote"
      )
    }
    
  }) # EO SITE median-income x remote relationships plot
  
  # SO median-income x rv only relationships plot
  output$medInc_site_rvOnly_plot <- renderPlotly({
    
    if (input$data_relationships == "Median-income x Site type") {
      
      median_income_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list,
        site_type_string = "rv only"
      )
    }
    
  }) # EO SITE median-income x rv only relationships plot
  
  # SO median-income x rv or tent relationships plot
  output$medInc_site_rvTent_plot <- renderPlotly({
    
    if (input$data_relationships == "Median-income x Site type") {
      
      median_income_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list,
        site_type_string = "rv or tent"
      )
    }
    
  }) # EO SITE median-income x rv or tent relationships plot
  
  # SO median-income x shelter relationships plot
  output$medInc_site_shelter_plot <- renderPlotly({
    
    if (input$data_relationships == "Median-income x Site type") {
      
      median_income_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list,
        site_type_string = "shelter"
      )
    }
    
  }) # EO SITE median-income x shelter relationships plot
  
  # SO median-income x tent only relationships plot
  output$medInc_site_tentOnly_plot <- renderPlotly({
    
    if (input$data_relationships == "Median-income x Site type") {
      
      median_income_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list,
        site_type_string = "tent only"
      )
    }
    
  }) # EO SITE median-income x tent only relationships plot
  
  # SO median-income x water relationships plot
  output$medInc_site_water_plot <- renderPlotly({
    
    if (input$data_relationships == "Median-income x Site type") {
      
      median_income_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        ridb_df = data_joined_2018,
        median_income_binned = median_income_decile_list,
        site_type_string = "water"
      )
    }
    
  }) # EO SITE median-income x water relationships plot
  
  #### SO race x site ----
  # SO race x equestrian relationships plot
  output$race_site_equestrian_plot <- renderPlotly({
    
    if (input$data_relationships == "Race x Site type") {
      
      race_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018,
        site_type_string = "equestrian"
      )
    }
    
  }) # EO SITE race x equestrian relationships plot
  
  # SO race x remote relationships plot
  output$race_site_remote_plot <- renderPlotly({
    
    if (input$data_relationships == "Race x Site type") {
      
      race_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018,
        site_type_string = "remote"
      )
    }
    
  }) # EO SITE race x remote relationships plot
  
  # SO race x rv only relationships plot
  output$race_site_rvOnly_plot <- renderPlotly({
    
    if (input$data_relationships == "Race x Site type") {
      
      race_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018,
        site_type_string = "rv only"
      )
    }
    
  }) # EO SITE race x rv only relationships plot
  
  # SO race x rv or tent relationships plot
  output$race_site_rvTent_plot <- renderPlotly({
    
    if (input$data_relationships == "Race x Site type") {
      
      race_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018,
        site_type_string = "rv or tent"
      )
    }
    
  }) # EO SITE race x rv or tent relationships plot
  
  # SO race x shelter relationships plot
  output$race_site_shelter_plot <- renderPlotly({
    
    if (input$data_relationships == "Race x Site type") {
      
      race_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018,
        site_type_string = "shelter"
      )
    }
    
  }) # EO SITE race x shelter relationships plot
  
  # SO race x tent only relationships plot
  output$race_site_tentOnly_plot <- renderPlotly({
    
    if (input$data_relationships == "Race x Site type") {
      
      race_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018,
        site_type_string = "tent only"
      )
    }
    
  }) # EO SITE race x tent only relationships plot
  
  # SO race x water relationships plot
  output$race_site_water_plot <- renderPlotly({
    
    if (input$data_relationships == "Race x Site type") {
      
      race_site_type_plot(
        admin_unitInput = input$admin_unit_relationships,
        siteInput = input$site_relationships,
        race_top_quartile_df = data_race_quants,
        ridb_df = data_joined_2018,
        site_type_string = "water"
      )
    }
    
  }) # EO SITE race x water relationships plot
  
  
  
  ## SO VISITORSHEDS maps ----
  ### CA ZIP codes plot ----
  output$caVisitorshed_plot <- renderTmap({
    ca_zip_code_visitorshed_map(siteInput = input$site_visitorsheds,
                                ridb_df = data_joined_2018,
                                zip_geometries_df = data_ca_zip_code_geometries, 
                                ca_cities_df = data_ca_cities_geometries)

  })
  
  ### state codes plot ----
  output$usVisitorshed_plot <- renderTmap({
    state_visitorshed_map(siteInput = input$site_visitorsheds,
                          ridb_df = data_joined_2018,
                          state_geometries_df = data_state_geometries)

  }) # EO visitorsheds plots
  
  ## SO DATA DOWNLOAD ----
  # create RDF
  data_download_dt <- reactive({
    data_joined_2018 %>%
      filter(park %in% input$site_data_download) %>% 
      select(input$cols_data_download)
  })
  # DT table
  output$data_download_table <- renderDT({
    DT::datatable(
      data_download_dt(),
      caption = htmltools::tags$caption(style = "caption-side: top; text-align: left",
                                        htmltools::em("Preview of selected dataset")),
      class = "cell-border stripe",
      rownames = FALSE,
      extensions = "FixedColumns", #"Buttons"
      options = list(
        server = FALSE,
        paging = TRUE,
        pageLength = 5,
        autoWidth = TRUE,
        scrollX = TRUE,
        fixedColumns = list(leftColumns = 3),
        #buttons = 'colvis',
        dom = "Brtip",
        columnDefs = list(list(
          targets = "_all",
          className = "dt-center"
        )) # EO columnDefs
      ) # EO options
    ) # EO datatable()
    
  }) # EO render data table
  
  # data download handler
  output$data_download <- downloadHandler(
    filename = "my_ridb_data.csv",
    content = function(file) {
      write.csv(data_download_dt(), file)
    }
  ) # EO data download handler
  
} ## EO server

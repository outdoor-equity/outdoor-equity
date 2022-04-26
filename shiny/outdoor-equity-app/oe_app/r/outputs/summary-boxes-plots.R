## function to create plots for data summary boxes ##

summary_plots <- function(){
  
  summary_plots <- renderPlotly({
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
    
    ## SO daily cost per visitor ----
    else if (input$data_summary == "daily_cost_per_visitor") {
      
      daily_cost_visitor_plot(admin_unitInput = input$admin_unit_summary,
                              siteInput = input$site_summary)
      
    } ## EO else if daily cost per visitor
    
    ## SO length of stay ----
    else if (input$data_summary == "length_of_stay") {
      
      length_of_stay_plot(dmin_unitInput = input$admin_unit_summary,
                          siteInput = input$site_summary)
      
    } ## EO else if length of stay
    
    ## SO site type ----
    else if (input$data_summary == "aggregated_site_type") {
      
      site_type_plot(admin_unitInput = input$admin_unit_summary,
                     siteInput = input$site_summary)
      
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
    # else if (input$data_summary == "education") {
    #   
    #   education_plot(agencyInput = input$agency_summary,
    #                  admin_unitInput = input$admin_unit_summary,
    #                  siteInput = input$site_summary,
    #                  titleInput = input$site_summary)
      
    # } ## EO else if education
    # 
    # ## SO median income ----
    # else if (input$data_summary == "median_income") {
    #   
    #   median_income_plot(agencyInput = input$agency_summary,
    #                      admin_unitInput = input$admin_unit_summary,
    #                      siteInput = input$site_summary,
    #                      titleInput = input$site_summary)
      
    # } ## EO else if median income
    
  }) ## EO data summary plots
  
  return(summary_plots)
  
} # EO function

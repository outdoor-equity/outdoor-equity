## function to create plots for data summary boxes ##

summary_plots <- function(isInput_id, isInput_au, isInput_site){
  
  summary_plots <- renderPlotly({
    ### SO distance traveled ----
    if (isInput_id == "distance_traveled_mi") {
      
      dist_travel_plot(admin_unitInput = isInput_au,
                       siteInput = isInput_site)
      
    } ## EO if distance traveled
    
    ## SO booking window ----
    else if (isInput_id == "booking_window") {
      
      booking_window_plot(admin_unitInput = isInput_au,
                          siteInput = isInput_site)
      
    } ## EO else if booking window
    
    ## SO daily cost per visitor ----
    else if (isInput_id == "daily_cost_per_visitor") {
      
      daily_cost_visitor_plot(admin_unitInput = isInput_au,
                              siteInput = isInput_site)
      
    } ## EO else if daily cost per visitor
    
    ## SO length of stay ----
    else if (isInput_id == "length_of_stay") {
      
      length_of_stay_plot(dmin_unitInput = isInput_au,
                          siteInput = isInput_site)
      
    } ## EO else if length of stay
    
    ## SO site type ----
    else if (isInput_id == "aggregated_site_type") {
      
      site_type_plot(admin_unitInput = isInput_au,
                     siteInput = isInput_site)
      
    } ## EO else if site type
    
    ## SO race ----
    # else if (isInput_id == "race") {
    #   
    #   race_plot(agencyInput = input$agency_summary,
    #             admin_unitInput = isInput_au,
    #             siteInput = isInput_site,
    #             titleInput = input$site_summary)
    #   
    # } ## EO else if race
    
    ## SO education ----
    # else if (isInput_id == "education") {
    #   
    #   education_plot(agencyInput = input$agency_summary,
    #                  admin_unitInput = isInput_au,
    #                  siteInput = isInput_site,
    #                  titleInput = input$site_summary)
      
    # } ## EO else if education
    # 
    # ## SO median income ----
    # else if (isInput_id == "median_income") {
    #   
    #   median_income_plot(agencyInput = input$agency_summary,
    #                      admin_unitInput = isInput_au,
    #                      siteInput = isInput_site,
    #                      titleInput = input$site_summary)
      
    # } ## EO else if median income
    
  }) ## EO data summary plots
  
  return(summary_plots)
  
} # EO function

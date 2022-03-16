# server instructions ----
server <- function(input, output){
  
  # filter body masses ----
  medinCA_df <- reactive({
    
    data_test %>% filter(mean_median_income %in% input$medianIncome_CA[1]:input$medianIncome_CA[2])
    
  })
  
  # render scatterplot ----
  output$medinCA_leflet <- renderTmap({
    
    # code to generate tmap here
    # reactive df always needs an open/close parentheses 
    tmap_mode("view") 
    tm_shape(data_zip_geometries_ca) +
      tm_polygons() +
      tm_shape(medinCA_df()) +
      tm_symbols(size = "count",
                 col = "mean_median_income",
                 alpha = 0.8,
                 title.col = "Avg Median Household Income of Visitors' Home ZIP Code",
                 breaks = c(40000, 50000, 60000, 70000, 80000, 90000, 100000, 150000, 215000),
                 palette = "viridis",
                 popup.vars = c("Agency" = "agency",
                                "Number of Reservations" = "count", 
                                "Average Median Household Income" = "mean_median_income"))
    
  })  

}
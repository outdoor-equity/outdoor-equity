# server instructions ----
server <- function(input, output){
  
  # # reactive df? maybe...
  # name_df <- reactive ({
  #   
  # })
  # render test plot ---
  output$test <- renderPlot({
    ggplot(data = data_combined_2018_clean, aes(x = mean_median_income)) +
      geom_histogram()
  })
  # # render map ----
  # output$resMedIncome_CAmap <- renderLeaflet({
  #   tmap_mode("view") 
  #   tm_shape(zip_geometries_ca) +
  #     tm_polygons() +
  #     tm_shape(data_combined_2018_clean) +
  #     tm_view(view.legend.position = c("bottom", "right")) +
  #     tm_symbols(size = "count",
  #                col = "mean_median_income",
  #                alpha = 0.8,
  #                title.col = "Avg Median Household Income of Visitors' Home ZIP Code",
  #                breaks = c(40000, 50000, 60000, 70000, 80000, 90000, 100000, 150000, 200000),
  #                palette = "viridis",
  #                popup.vars = c("Agency" = "agency",
  #                               "Number of Reservations" = "count", 
  #                               "Average Median Household Income" = "mean_median_income"))
  # })
  
  
}
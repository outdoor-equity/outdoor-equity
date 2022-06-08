
#' Median-income Number of Reservations Above Top Quartile Data Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param median_income_decile_df Object name for dataframe of all reservations split into median-income deciles
#'
#' @return Plotly of median-income categories compared to booking window
#'
#' @examples

median_income_top_quartile_res_plot <- function(admin_unitInput, 
                                                siteInput, 
                                                median_income_decile_df){
  
  # create reactive dataframe and further subset
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    median_income_decile_df %>%
      # filter to user site of choice
      filter(park %in% siteInput) %>%
      # select the variables of interest
      select(park, customer_zip, median_income_binned) %>% 
      drop_na(median_income_binned) %>% 
      # summarize to total reservations
      group_by(median_income_binned) %>% 
      summarize(count = n())
  }) # EO rdf
  
  # create plot
  plotly <- ggplot(data = rdf()) +
    geom_col(aes(x = count,
                 y = median_income_binned,
                 fill = median_income_binned,
                 text = paste0(scales::comma(count, accuracy = 1), 
                               " reservations were made<br>by people who live in ZIP codes<br>with median-incomes between<br>", 
                               median_income_binned, "."))) +
    scale_x_continuous(labels = comma_format(accuracy = 1)) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_viridis_d(direction = -1) +
    scale_color_viridis_d(direction = -1) +
    labs(x = paste("Number of Reservations"),
         y = "") + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8))
  
  # create plotly
  ggplotly(plotly,
           tooltip = list("text")) %>%
    config(modeBarButtonsToRemove = list("zoom", "pan", "select", "zoomIn", "zoomOut", "lasso2d", "resetScale2d",
                                         "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0("Reservations to this site from<br>ZIP codes with median-incomes of:"),
                        font = list(size = 10.5)),
           height = 405,
           width = 340)
  
} # EO function
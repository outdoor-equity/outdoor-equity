
#' Education Number of Reservations Above Top Quartile Data Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param education_top_quartile_df Object name for dataframe of all reservations above "high" threshold for education
#'
#' @return Plotly of educational categories compared to distance traveled
#'
#' @examples

education_top_quartile_res_plot <- function(admin_unitInput, 
                                            siteInput,
                                            education_top_quartile_df){
  
  # create reactive dataframe and further subset
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    education_top_quartile_df %>%
      # filter to user site of choice
      filter(park %in% siteInput) %>%
      # select the variables of interest
      select(park, customer_zip, 
             education, education_percentage, education_y_lab) %>% 
      drop_na(education_percentage) %>% 
      # summarize to inner quartile range, median, and total reservations
      group_by(education, education_y_lab) %>% 
      summarize(count = n())
  }) #EO rdf
  
  validate(need(
    nrow(rdf()) > 0,
    paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
           " that come from communities in the high range for any educational categories.")
  )) # EO validate
  
  # parameters
  education_group_colors <- c("HS, GED,\nor Below" = "#a6cee3", 
                              "Some College or\nTrade School"  = "#1f78b4", 
                              "Associates or\nBachelors Degree" = "#b2df8a", 
                              "Masters Degree\nor Above" = "#33a02c")
  
  # create plot
  plotly <- ggplot(data = rdf()) +
    geom_col(aes(x = count,
                 y = education_y_lab,
                 fill = education_y_lab,
                 text = paste0(scales::comma(count, accuracy = 1), 
                               " reservations were made<br>by people who live in ZIP codes",
                               "<br>",
                               "with high rates of people who have<br>", education, 
                               "<br>",
                               "as their highest level of education."))) +
    scale_x_continuous(labels = comma_format(accuracy = 1)) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_manual(values = education_group_colors) +
    scale_color_manual(values = education_group_colors) +
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
    layout(title = list(text = paste0("Reservations to this site from<br>ZIP codes with high proportions of:"),
                        font = list(size = 10.5)),
           height = 405,
           width = 340)
  
} # EO function
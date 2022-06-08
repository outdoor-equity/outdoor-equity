
#' Race Number of Reservations Above Top Quartile Data Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param race_top_quartile_df Object name for dataframe of all reservations above "high" threshold for race
#'
#' @return Plotly of racial categories compared to distance traveled
#'
#' @examples

race_top_quartile_res_plot <- function(admin_unitInput, 
                                       siteInput,
                                       race_top_quartile_df){
  
  # create reactive dataframe and further subset
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    race_top_quartile_df %>%
      # filter to user site of choice
      filter(park == siteInput) %>%
      # select to variables of interest
      select(park, customer_zip, 
             race, race_percentage) %>% 
      drop_na(race_percentage) %>% 
      # summarize to inner quartile range, median, and total reservations
      group_by(race) %>%
      summarize(count = n())
  }) # EO rdf
  
  validate(need(
    nrow(rdf()) > 0,
    paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
           " that come from communities in the high range for any racial categories.")
  )) # EO validate
  
  # parameters
  race_group_colors <- c("Other Race(s)" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                         "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                         "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  # create plot
  plotly <- ggplot(data = rdf()) +
    geom_col(aes(x = count,
                 y = reorder(race, count),
                 fill = race,
                 text = paste0(scales::comma(count, accuracy = 1), 
                               " reservations were made<br>by people who live in<br>ZIP codes with high<br>", 
                               race, " populations."))) +
    scale_x_continuous(labels = comma_format(accuracy = 1)) +
    scale_y_discrete(expand = c(0.1, 0)) +
    scale_fill_manual(values = race_group_colors) +
    scale_color_manual(values = race_group_colors) +
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
    layout(title = list(text = paste0("Number of Reservations to this site coming from<br>ZIP codes with high proportions of:"),
                        font = list(size = 10.5)),
           height = 405,
           width = 340)
  
} # EO function
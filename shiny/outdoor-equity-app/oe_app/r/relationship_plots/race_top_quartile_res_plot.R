
#' Race Number of Reservations Above Top Quartile Data Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param education_top_quartile_df Name of dataframe of values to iterate through for all 
#'     racial categories and 3rd quartile values associated with each
#' @param ridb_df RIDB dataframe object name
#'
#' @return Plotly of racial categories compared to distance traveled
#'
#' @examples

race_top_quartile_res_plot <- function(admin_unitInput, siteInput,
                                  race_top_quartile_df, ridb_df){
  
  # iterate through dataframe of all racial categories and 3rd quartile values
  # return combined dataframe of reservations in "high" range for all categories
  data_top_quartile_res_travel <- 
    race_top_quartile_df %>% pmap_dfr(race_top_quartile_res_data, 
                                      ridb_df = ridb_df, 
                                      siteInput = siteInput)
  
  validate(need(
    nrow(data_top_quartile_res_travel) > 0,
    paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
           " that come from communities in the high range for any racial categories.")
  )) # EO validate
  
  # parameters
  race_group_colors <- c("Other Race(s)" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                         "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                         "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  # create plot
  plotly <- ggplot(data = data_top_quartile_res_travel) +
    geom_col(aes(x = count,
                 y = reorder(race, count),
                 fill = race,
                 text = paste0(comma(count, accuracy = 1), 
                               " reservations were made by <br>people who live in ZIP codes with high ", 
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
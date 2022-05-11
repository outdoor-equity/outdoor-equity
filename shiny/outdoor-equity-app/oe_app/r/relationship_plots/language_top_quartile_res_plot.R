
#' Language Number of Reservations Above Top Quartile Data Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param education_top_quartile_df Name of dataframe of values to iterate through for all 
#'     language categories and 3rd quartile values associated with each
#' @param ridb_df RIDB dataframe object name
#'
#' @return Plotly of language categories compared to distance traveled
#'
#' @examples

language_top_quartile_res_plot <- function(admin_unitInput, siteInput,
                                           language_top_quartile_df, ridb_df){
  
  # iterate through dataframe of all language categories and 3rd quartile values
  # return combined dataframe of reservations in "high" range for all categories
  plot_data <- 
    language_top_quartile_df %>% pmap_dfr(language_top_quartile_res_data, 
                                          ridb_df = ridb_df, 
                                          siteInput = siteInput)
  
  validate(need(
    nrow(plot_data) > 0,
    paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
           " that come from communities in the high range for any language categories.")
  )) # EO validate
  
  # parameters
  language_group_colors <- c("People Who Speak Only<br>English At Home" = "#66c2a5", 
                             "People Who Speak<br>Language(s) Other Than<br>English At Home" = "#8da0cb")
  
  # create plot
  plotly <- ggplot(data = plot_data) +
    geom_col(aes(x = count,
                 y = language_y_lab,
                 fill = language_y_lab,
                 text = paste0(comma(count, accuracy = 1), 
                               " reservations were made by people who live in ZIP codes<br>with high rates of people who ", 
                               language, "."))) +
    scale_x_continuous(labels = comma_format(accuracy = 1)) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_manual(values = language_group_colors) +
    scale_color_manual(values = language_group_colors) +
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
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0("Reservations to this site from<br>ZIP codes with high proportions of:"),
                        font = list(size = 10.5)),
           height = 405,
           width = 340)
  
} # EO function
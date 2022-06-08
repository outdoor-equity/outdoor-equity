
#' Language Number of Reservations Above Top Quartile Data Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param language_top_quartile_df Object name for dataframe of all reservations above "high" threshold for language
#'
#' @return Plotly of language categories compared to distance traveled
#'
#' @examples

language_top_quartile_res_plot <- function(admin_unitInput, 
                                           siteInput,
                                           language_top_quartile_df){
  
  # create reactive dataframe and further subset
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    language_top_quartile_df %>%
      # filter to user site of choice
      filter(park == siteInput) %>%
      # select the variables of interest
      select(park, customer_zip, 
             language, language_percentage, language_y_lab) %>% 
      drop_na(language_percentage) %>% 
      # summarize to inner quartile range, median, and total reservations
      group_by(language, language_y_lab) %>% 
      summarize(count = n())
  }) # EO rdf
  
  validate(need(
    nrow(rdf()) > 0,
    paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
           " that come from communities in the high range for any language categories.")
  )) # EO validate
  
  # parameters
  language_group_colors <- c("Only English<br>At Home" = "#66c2a5", 
                             "Language(s)<br>Other Than<br>English At Home" = "#8da0cb")
  
  # create plot
  plotly <- ggplot(data = rdf()) +
    geom_col(aes(x = count,
                 y = language_y_lab,
                 fill = language_y_lab,
                 text = paste0(scales::comma(count, accuracy = 1), 
                               " reservations were made<br>by people who live in ZIP codes<br>with high rates of people who<br>", 
                               language, ".")),
             width = 0.5) +
    geom_text(aes(x = max(count) / 2,
                  y = language_y_lab,
                  label = language_y_lab), 
              size = 3,
              nudge_x = 0) +
    scale_x_continuous(labels = comma_format(accuracy = 1)) +
    scale_y_discrete(expand = c(0.45, 0)) +
    scale_fill_manual(values = language_group_colors) +
    scale_color_manual(values = language_group_colors) +
    labs(x = paste("Number of Reservations"),
         y = NULL) + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(size = 8),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(0, 0, 0, 0, unit = "pt"))
  
  # create plotly
  ggplotly(plotly,
           tooltip = list("text")) %>%
    config(modeBarButtonsToRemove = list("zoom", "pan", "select", "zoomIn", "zoomOut", "lasso2d", "resetScale2d",
                                         "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0("Reservations to this site from ZIP codes<br>with high proportions of people who speak:"),
                        font = list(size = 10.5)),
           height = 405,
           width = 340)
  
} # EO function
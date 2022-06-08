#' Median-income x Site Type Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param median_income_decile_df Object name for dataframe of all reservations split into median-income deciles
#' @param site_type_string String indicating what site to create plot of
#'     Options include: "equestrian", "remote", "rv only", "rv or tent", "shelter", "tent only", "water"
#'
#' @return Plotly of median-income categories compared to site type
#'
#' @examples

median_income_site_type_plot <- function(admin_unitInput, 
                                         siteInput, 
                                         median_income_decile_df, 
                                         site_type_string){
  
  # create reactive dataframe and further subset
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    median_income_decile_df %>%
      # filter to user site of choice
      filter(park %in% siteInput) %>%
      # select to variables of interest
      select(park, customer_zip, median_income_binned, aggregated_site_type) %>% 
      drop_na(median_income_binned) %>% 
      # summarize to total reservations for each site type
      group_by(aggregated_site_type, median_income_binned) %>% 
      summarize(count = n()) %>% 
      filter(aggregated_site_type == site_type_string) %>% 
      mutate(aggregated_site_type = str_replace(string = aggregated_site_type,
                                                pattern = "rv", 
                                                replacement = "RV"))
  }) #EO rdf
  
  validate(
    need(nrow(rdf()) > 0,
         paste0("There are no ", site_type_string %>%
                  str_replace(string = ., pattern = "rv", replacement = "RV"), " sites at ", siteInput, ", ", admin_unitInput, "."))
  ) # EO validate
  
  # create plot
  plotly <- ggplot(data = rdf()) +
    geom_col(aes(x = count,
                 y = median_income_binned,
                 fill = median_income_binned,
                 text = paste0("Of visits to ", aggregated_site_type, " overnight reservable sites, ", 
                               scales::comma(count, accuracy = 1), 
                               " reservations were made by <br>people who live in ZIP codes with median household incomes between ",
                               median_income_binned, "."))) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_viridis_d(direction = -1) +
    scale_color_viridis_d(direction = -1) +
    labs(x = paste("Number of Reservations"),
         y = "") + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none")
  
  # create plotly
  ggplotly(plotly,
           tooltip = list("text")) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      "Number of Reservations to ", 
                                      str_to_title(site_type_string) %>% 
                                        str_replace(string = ., pattern = "Rv", replacement = "RV"), 
                                      " Sites by Visitors with Different Median Household Incomes"),
                        font = list(size = 11.8)))
  
} # EO function
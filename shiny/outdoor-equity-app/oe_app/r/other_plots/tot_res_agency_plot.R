## total reservations by agency and parameters ##
# WILL LIKELY BE IN ABOUT PAGE in server
# NEED TO TEST

tot_res_ag_plot <- function(){
  
  # no data wrangling
  # no RDF
  
  # plot for shiny app
  ggplot(data = data_joined_2018) +
    geom_bar(aes(y = fct_infreq(agency)), fill = "#009900FF") +
    scale_y_discrete(labels = c("BOR" = "Bureau of Reclamation", "NPS" = "National Park Service", 
                                "USACE" = "US Army Corps of Engineers", "USFS" = "US Forest Service")) +
    #scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, 100), minor_breaks = seq(0, 40, 50)) +
    theme_minimal() +
    labs(y = "",
         x = "Total Reservations per Agency",
         fill = "Agency",
         title = "Total Reservations per Agency \nfor Reservable Overnight Sites in California in 2018") +
    theme(panel.grid.minor.x = element_blank(),
          plot.background = element_rect("white"))
  
} # EO function
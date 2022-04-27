## total sites by agency and parameters ##
# WILL LIKELY BE IN ABOUT PAGE in server
# NEED TO TEST

tot_site_ag_plot <- function(){
  
  # does not need to be reactive 
  data_plot_agency_park <- data_joined_2018 %>% 
    group_by(agency, park) %>% 
    summarise(count = n())
  
  # plot for shiny app
  ggplot(data_plot_agency_park) +
    geom_bar(aes(y = fct_infreq(agency, )), fill = "#009900FF") +
    scale_y_discrete(labels = c("BOR" = "Bureau of Reclamation", "NPS" = "National Park Service", 
                                "USACE" = "US Army Corps of Engineers", "USFS" = "US Forest Service")) +
    scale_x_continuous(limits = c(0, 400), breaks = seq(0, 400, 100), minor_breaks = seq(0, 40, 50)) +
    theme_minimal() +
    labs(fill = "Agency",
         y = "Agency",
         x = "Reservable Site Count",
         title = "Total Sites per Agency \nfor Reservable Overnight Sites in California in 2018") +
    theme(panel.grid.major.y = element_blank(),
          plot.background = element_rect("white"))
  
} # EO function
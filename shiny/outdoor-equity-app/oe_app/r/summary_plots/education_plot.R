## education plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

education_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  education_rdf <- reactive ({
    
    data_plot_education_ridb <- data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      summarize(hs_GED_or_below = mean(hs_GED_or_below, na.rm = TRUE),
                some_college = mean(some_college, na.rm = TRUE),
                college = mean(college, na.rm = TRUE),
                master_or_above = mean(master_or_above, na.rm = TRUE))  %>%
      pivot_longer(cols = 1:4, names_to = "education", values_to = "education_percent_average")
    
    # CA population
    data_plot_education_ca <- data_ca_acs_2018 %>%
      summarize(hs_GED_or_below = weighted.mean(hs_GED_or_below, mean_zip_code_population, 
                                                na.rm = TRUE),
                some_college = weighted.mean(some_college, mean_zip_code_population, 
                                             na.rm = TRUE),
                college = weighted.mean(college, mean_zip_code_population, 
                                        na.rm = TRUE),
                master_or_above = weighted.mean(master_or_above, mean_zip_code_population, 
                                                na.rm = TRUE))  %>%
      pivot_longer(cols = 1:4, names_to = "education", values_to = "education_percent_average")
    
    # join data for plotting
    # data_plot_education <-
    data_plot_education_ridb %>% 
      left_join(y = data_plot_education_ca,
                by = c("education"),
                suffix = c("_ridb", "_ca")) %>% 
      rename(RIDB = education_percent_average_ridb,
             CA = education_percent_average_ca) %>% 
      pivot_longer(cols = 2:3,
                   names_to = "data_source",
                   values_to = "education_percent_average") %>% 
      mutate(education = str_replace_all(string = education,
                                         pattern = "_", 
                                         replacement = " "),
             education = str_to_title(education),
             education = str_replace(string = education,
                                     pattern = "Hs Ged Or", 
                                     replacement = "HS, GED, or"),
             education = str_replace(string = education,
                                     pattern = "Some College",
                                     replacement = "Some College or Trade School"),
             education = str_replace(string = education,
                                     pattern = "^College$",
                                     replacement = "Associates or Bachelors Degree"),
             education = str_replace(string = education,
                                     pattern = "Master Or Above",
                                     replacement = "Masters Degree or Above"),
             education = factor(education, levels = c("HS, GED, or Below", "Some College or Trade School", 
                                                      "Associates or Bachelors Degree", "Masters Degree or Above")),
             data_source = factor(data_source, levels = c("RIDB", "CA")),
             tooltip_start = case_when(data_source == "RIDB" ~ "Visitors to California sites live in communities with an estimated ",
                                       data_source == "CA" ~ ""),
             tooltip_middle = case_when(data_source == "RIDB" ~ " of the population <br>with ",
                                        data_source == "CA" ~ " of Californians have "),
             tooltip_end = case_when(data_source == "RIDB" ~ " as their highest level of education.",
                                     data_source == "CA" ~ " as their highest level of education."))
    
  }) # EO RDF
  
  x_max <- max(education_rdf()$education_percent_average) + 0.1 # max x rounded to nearest 5
  
  # parameters
  groups_colors_ridb_ca <- c("RIDB" = "#009900FF", "CA" = "#666666")
  text_colors_ridb_ca <- c("RIDB" = "#006600", "CA" = "#282828")
  
  # plot for shiny app
  education_plotly <- ggplot(data = education_rdf()) +
    geom_col(aes(x = education_percent_average,
                 y = education,
                 fill = data_source,
                 text = paste0(tooltip_start, percent(education_percent_average, accuracy = 0.1), 
                               tooltip_middle, education, tooltip_end)),
             position = "dodge") +
    scale_x_continuous(labels = percent, limits = c(0, x_max)) +
    scale_fill_manual(values = groups_colors_ridb_ca) + 
    geom_text(aes(x = education_percent_average,
                  y = education,
                  label = percent(education_percent_average, accuracy = 0.1),
                  col = data_source), 
              position = position_dodge(width = 1), 
              size = 4) +
    scale_color_manual(values = text_colors_ridb_ca) +
    labs(x = "Percentage (%)",
         y = "",
         title = paste0("Estimated Highest Level of Education of <br>California Residents vs. Visitors to ", 
                        siteInput, ", ", admin_unitInput)) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(education_plotly,
           tooltip = list("text")) %>%
    style(hoverinfo = "none", traces = c(3, 4),
          textposition = "right") %>% 
    layout(showlegend = FALSE)
  
} # EO function
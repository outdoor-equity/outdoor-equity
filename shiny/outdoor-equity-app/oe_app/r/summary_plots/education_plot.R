## education plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

education_plot <- function(agencyInput, admin_unitInput, siteInput, titleInput){
  
  # reactive data frame 
  education_rdf <- reactive ({
    
    data_plot_education_ridb <- data_joined_2018 %>%
      filter(agency %in% agencyInput,
             admin_unit %in% admin_unitInput,
             park %in% siteInput) %>%
      summarize(hs_GED_or_below = (mean(hs_GED_or_below, na.rm = TRUE) * 100),
                  some_college = (mean(some_college, na.rm = TRUE) * 100),
                  college = (mean(college, na.rm = TRUE) * 100),
                  master_or_above = (mean(master_or_above, na.rm = TRUE) * 100))  %>%
        pivot_longer(cols = 1:4, names_to = "education", values_to = "education_percent_average") %>% 
        mutate(education = str_replace(string = education,
                                       pattern = "hs_GED_or_below",
                                       replacement = "High school, GED, or below"),
               education = str_replace(string = education,
                                       pattern = "some_college",
                                       replacement = "Some College"),
               education = str_replace(string = education,
                                       pattern = "college",
                                       replacement = "Associates or bachelors degree"),
               education = str_replace(string = education,
                                       pattern = "master_or_above",
                                       replacement = "Masters degree or higher"))
      
      # CA population
      data_plot_education_ca <- data_ca_acs_2018 %>%
        summarize(hs_GED_or_below = (weighted.mean(hs_GED_or_below, mean_zip_code_population, 
                                                   na.rm = TRUE) * 100),
                  some_college = (weighted.mean(some_college, mean_zip_code_population, 
                                                na.rm = TRUE) * 100),
                  college = (weighted.mean(college, mean_zip_code_population, 
                                           na.rm = TRUE) * 100),
                  master_or_above = (weighted.mean(master_or_above, mean_zip_code_population, 
                                                   na.rm = TRUE) * 100))  %>%
        pivot_longer(cols = 1:4, names_to = "education", values_to = "education_percent_average") %>% 
        mutate(education = str_replace(string = education,
                                       pattern = "hs_GED_or_below",
                                       replacement = "High school, GED, or below"),
               education = str_replace(string = education,
                                       pattern = "some_college",
                                       replacement = "Some College"),
               education = str_replace(string = education,
                                       pattern = "college",
                                       replacement = "Associates or bachelors degree"),
               education = str_replace(string = education,
                                       pattern = "master_or_above",
                                       replacement = "Masters degree or higher"))
      
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
        mutate(data_source = factor(data_source, levels = c("RIDB", "CA")),
               education = factor(education, levels = c("High school, GED, or below",
                                                        "Some College", 
                                                        "Associates or bachelors degree",
                                                        "Masters degree or higher")))
    
  }) # EO RDF
  
  # parameters
  groups_colors_ridb_ca <- c("RIDB" = "#009900FF", "CA" = "#666666")
  
  # plot for shiny app
  ggplot(data = education_rdf()) +
    geom_col(aes(x = education_percent_average,
                 y = education,
                 fill = data_source),
             stat = "identity",
             position = "dodge") +
    scale_fill_manual(values = groups_colors_ridb_ca) +  
    geom_text(aes(x = education_percent_average,
                  y = education,
                  label = paste0(round(education_percent_average, 1), "%"),
                  col = data_source), 
              position = position_dodge(width = 1), 
              hjust = -0.1, size = 4) +
    scale_color_manual(values = groups_colors_ridb_ca) +
    labs(x = "Percentage (%)",
         y = "",
         title = paste("Breakdown of Education Levels of ZIP Codes in 2018 for", titleInput),
         subtitle = "Visitors' home ZIP codes for Overnight Reservations in California \nvs. California Residents") +
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10), minor_breaks = seq(0, 60, 5)) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
} # EO function
# race rdf and wrangling

data_joined_2018 %>% 
  filter(agency %in% input$agency_summary,
         admin_unit %in% input$admin_unit_summary,
         park %in% input$site_summary) %>%
  # need to add more
  summarize(white = (mean(white, na.rm = TRUE) * 100),
            black = (mean(black, na.rm = TRUE) * 100),
            asian = (mean(asian, na.rm = TRUE) * 100),
            multiracial = (mean(multiracial, na.rm = TRUE) * 100),
            other = (mean(other, na.rm = TRUE) * 100),
            native_american = (mean(native_american, na.rm = TRUE) * 100),
            pacific_islander = (mean(pacific_islander, na.rm = TRUE) * 100),
            hispanic_latinx = (mean(hispanic_latinx, na.rm = TRUE) * 100)) %>%
  pivot_longer(cols = 1:8, names_to = "race", values_to = "race_percent_average") %>% 
  mutate(race = str_replace(string = race,
                            pattern = "_",
                            replacement = " "),
         race = str_to_title(race))
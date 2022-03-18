library(tidyverse)
library(here)


## RACE COL PLOT ----
## -- data wrangle -- ##

# reservations in CA
data_plot_col_race_ridb <- data_hist_race %>%
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
data_plot_col_race_ridb$race <- with(data_plot_col_race_ridb, reorder(race, race_percent_average))

# CA population
data_plot_col_race_ca <- data_acs_2018_race_percent_California %>%
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
data_plot_col_race_ca$race <- with(data_plot_col_race_ca, reorder(race, race_percent_average))

# join data for plotting
data_plot_col_race <- data_plot_col_race_ridb %>% 
  left_join(y = data_plot_col_race_ca,
            by = c("race"),
            suffix = c("_ridb", "_ca")) %>% 
  rename(RIDB = race_percent_average_ridb,
         CA = race_percent_average_ca) %>% 
  pivot_longer(cols = 2:3,
               names_to = "data_source",
               values_to = "race_percent_average") %>% 
  mutate(data_source = factor(data_source, levels = c("RIDB", "CA")))

## -- create plot -- ##

# parameters
race_colors_ridb_ca <- c("RIDB" = "#009900FF", "CA" = "#666666")

# plot for shiny app
ggplot(data = data_plot_col_race) +
  geom_col(aes(x = race_percent_average,
               y = race,
               fill = data_source),
           stat = "identity",
           position = "dodge") +
  scale_fill_manual(values = groups_colors_ridb_ca) +  
  geom_text(aes(x = race_percent_average,
                y = race,
                label = paste0(round(race_percent_average, 1), "%"),
                col = data_source), 
            position = position_dodge(width = 1), 
            hjust = -0.1, size = 4) +
  scale_color_manual(values = groups_colors_ridb_ca) +
  labs(x = "Percentage (%)",
       y = "") +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10), 
                     minor_breaks = seq(0, 60, 5)) +
  theme_minimal() +
  theme(plot.background = element_rect("white"),
        panel.grid.major.y = element_blank())



hist_colors <- c("#009900FF")

# plot for shiny app
ggplot(data = data_hist_distance_traveled) +
  geom_histogram(aes(x = distance_traveled_mi),
                 fill = hist_colors) +
  scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500), minor_breaks = seq(0, 3000, 250)) +
  labs(x = "Distance traveled (miles)",
       y = "") +
  theme_minimal() +
  theme(plot.background = element_rect("white"),
        panel.grid.major.y = element_blank())


# distance traveled ----
# parameters
hist_colors <- c("#009900FF")

# plot for shiny app
ggplot(data = data_hist_distance_traveled) +
  geom_histogram(aes(x = distance_traveled_mi),
                 fill = hist_colors) +
  labs(x = "Distance traveled (miles)",
       y = "",
       title = "Distribution of Distance Traveled to ",
       subtitle = "Overnight Reservations in California in 2018") +
  scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500), minor_breaks = seq(0, 3000, 250)) +
  theme_minimal() +
  theme(plot.background = element_rect("white"),
        panel.grid.major.y = element_blank()) 






combined_summary_2018 <- readRDS(here::here("../../data_clean/2018_joined_data_site_summary.rds"))
combined_2018 <- readRDS(here::here("../../data_clean/2018_joined_data.csv"))

ggplot(data = combined_2018, aes(x = mean_booking_window)) +
  geom_histogram()
## TO DO ## ----

# need to figure out why these variables won't show up on app. how do I give one condition id to multiple options?
choices = c(booking_scat_var = "book_scat",
            agency_comp_acs_col_vars = "acs_scat")

# when 2. What kind of analysis do you want to see? is empty how to remove step 4?  Pick second variable to compare 




## OLD ## ----

#### TABS TO THINK ABOUT READDING LATER #### ----  


# selectizeInput(inputId = "distribution",
#                label = "3. Pick a variable to see its distribution",
#                choices = c()),


# tabPanel(title = "Spatial Analysis",
#   "spatial analysis maps and inputs here",
#   # subset agency picker input ----
#   shinyWidgets::pickerInput(inputId = "agency",
#                             label = "Select an agency:",
#                             choices = c("USFS", "NPS", "USACE", "BOR"),
#                             # selected = c("USFS", "NPS", "USACE", "BOR"), 
#                             # use this to automatically to set default choices
#                             multiple = TRUE,
#                             options = pickerOptions(actionsBox = TRUE)),
#   shinyWidgets::pickerInput(inputId = "state",
#                             label = "Select a state:",
#                             choices = c("California", "Alaska", "Utah", "Maine"),
#                             selected = "California",
#                             multiple = TRUE,
#                             options = pickerOptions(actionsBox = TRUE)),
#   shinyWidgets::pickerInput(inputId = "year",
#                             label = "Select a year:",
#                             choices = 2018,
#                             multiple = TRUE,
#                             options = pickerOptions(actionsBox = TRUE)),
#   shinyWidgets::pickerInput(inputId = "variable",
#               label = "Select a variable",
#               choices = c("Median income", "Race", "Transportation"),
#               selected = " ", # need to figure out why there is a default select
#               multiple = FALSE,
#               options = pickerOptions(actionsBox = TRUE))
# ),
# tabPanel(title = "Temporal Analysis",
#   "graphs that show interesting temporal trends"
# ))
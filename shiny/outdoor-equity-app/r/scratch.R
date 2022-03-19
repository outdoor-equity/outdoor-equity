library(tidyverse)
library(here)

# parameters
racial_group_colors <- c("Other" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                         "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                         "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")

# plot for shiny
ggplot(data = data_comp_dist_travel_race) +
  geom_col(aes(x = distance_traveled_bins,
               y = race_percentage,
               fill = race),
           stat = "identity",
           position = "dodge")  +
  scale_fill_manual(values = racial_group_colors) +
  labs(y = "Distance Traveled (miles)",
       x = "Percentage (%)",
       caption = "Distance Travel Quintiles: 1 = less than 60.2 (with the lowest being 0.5), 2 = 60.2 - 116.9, 3 = 116.7 - 180.9, 4 = 180.9 - 229.3, 5 = above 229.3 (with the highest being 3613.7") +
  theme_minimal() +
  theme(plot.background = element_rect("white"),
        panel.grid.major.y = element_blank())



data_reg_comb <- data_joined_2018 %>% 
  group_by(regional_area, agency) %>% 
  summarize(count = n())


tmap_mode("plot")
tm_shape(data_ca_geom) +
  tm_polygons(col = "white") +
tm_shape(data_reg_comb) +
  tm_symbols(shape = "agency",
             title.shape = "Agency")

## RACE COL PLOT ----
## -- data wrangle -- ##

## -- create plot -- ##

# parameters
race_colors_ridb_ca <- c("RIDB" = "#009900FF", "CA" = "#666666")

# plot for shiny app
ggplot(data = data_hist_race) +
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
# END RACE PLOT



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
library(tidyverse)
library(here)

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
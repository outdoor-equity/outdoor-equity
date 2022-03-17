library(tidyverse)
library(here)

combined_summary_2018 <- readRDS(here::here("../../data_clean/2018_joined_data_site_summary.rds"))
combined_2018 <- readRDS(here::here("../../data_clean/2018_joined_data.csv"))

ggplot(data = combined_2018, aes(x = mean_booking_window)) +
  geom_histogram()
## TO DO ##

# need to figure out why these variables won't show up on app. how do I give one condition id to multiple options?
choices = c(booking_scat_var = "book_scat",
            agency_comp_acs_col_vars = "acs_scat")

# when 2. What kind of analysis do you want to see? is empty how to remove step 4?  Pick second variable to compare 
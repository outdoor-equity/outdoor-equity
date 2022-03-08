
## turn into function

data_combined_2018_site_summary <- 
  data_combined_2018 %>% 
  filter(daily_cost_per_visitor != "Inf") %>% #drop 48 rows with "Inf" for cost/visitor/day
  group_by(agency, park) %>% 
  summarize(median_paid = median(total_paid),
            mean_paid = mean(total_paid),
            max_paid = max(total_paid),
            min_paid = min(total_paid),
            median_number_of_people = median(number_of_people),
            mean_number_of_people = mean(number_of_people),
            max_number_of_people = max(number_of_people),
            min_number_of_people = min(number_of_people),
            median_length_of_stay = median(length_of_stay),
            mean_length_of_stay = mean(length_of_stay),
            max_length_of_stay = max(length_of_stay),
            min_length_of_stay = min(length_of_stay),
            median_booking_window = median(booking_window),
            mean_booking_window = mean(booking_window),
            max_booking_window = max(booking_window),
            min_booking_window = min(booking_window),
            median_daily_cost_per_visitor = median(daily_cost_per_visitor),
            mean_daily_cost_per_visitor = mean(daily_cost_per_visitor),
            max_daily_cost_per_visitor = max(daily_cost_per_visitor),
            min_daily_cost_per_visitor = min(daily_cost_per_visitor),
            # na.rm drops ~3% of data (missing ZIP codes from acs5)
            median_median_income = median(median_income, na.rm = TRUE),
            mean_median_income = mean(median_income, na.rm = TRUE),
            max_median_income = max(median_income, na.rm = TRUE),
            min_median_income = min(median_income, na.rm = TRUE),
            median_percent_asian = median(asian, na.rm = TRUE),
            mean_percent_asian = mean(asian, na.rm = TRUE),
            max_percent_asian = max(asian, na.rm = TRUE),
            min_percent_asian = min(asian, na.rm = TRUE),
            median_percent_black = median(black, na.rm = TRUE),
            mean_percent_black = mean(black, na.rm = TRUE),
            max_percent_black = max(black, na.rm = TRUE),
            min_percent_black = min(black, na.rm = TRUE),
            median_percent_multiracial = median(multiracial, na.rm = TRUE),
            median_percent_other = median(other, na.rm = TRUE),
            median_percent_white = median(white, na.rm = TRUE),
            median_percent_pacific_islander = median(pacific_islander, na.rm = TRUE),
            median_percent_native_american = median(native_american, na.rm = TRUE),
            median_percent_hispanic_latinx = median(hispanic_latinx, na.rm = TRUE),
            median_percent_college = median(college, na.rm = TRUE),
            median_percent_hs_GED_or_below = median(hs_GED_or_below, na.rm = TRUE),
            median_percent_master_or_above = median(master_or_above, na.rm = TRUE),
            median_percent_some_college = median(some_college, na.rm = TRUE),
            median_percent_no_vehicle = median(no_vehicle, na.rm = TRUE),
            # include count column listing number of reservations for each park
            count = n())

## figure out how to do it in a for loop

summary_variables <- c("total_paid", "number_of_people", "length_of_stay", 
                       "booking_window", "daily_cost_per_visitor", "asian",
                       "black", "hispanic_latinx", "multiracial", "native_american", 
                       "other", "pacific_islander", "white", "college", 
                       "hs_GED_or_be, master_or_above", "some_college", 
                       "median_income","no_vehicle")

test <- 
  data_combined_2018 %>% 
  filter(daily_cost_per_visitor != "Inf") %>% #drop 48 rows with "Inf" for cost/visitor/day
  group_by(agency, park) %>% 
  summarize(median_paid = median(total_paid),
            mean_paid = mean(total_paid),
            max_paid = max(total_paid),
            min_paid = min(total_paid),
            median_median_income = median(median_income, na.rm = TRUE),
            mean_median_income = mean(median_income, na.rm = TRUE),
            max_median_income = max(median_income, na.rm = TRUE),
            min_median_income = min(median_income, na.rm = TRUE),
            count = n())
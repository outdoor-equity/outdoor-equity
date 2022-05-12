
#' Language Number of Reservations Above Top Quartile Data
#'
#' @param siteInput User pick for site
#' @param education_group String indicating language category of interest
#' @param weighted_quartile Value of 3rd quartile for language category
#' @param ridb_df RIDB dataframe object name
#'
#' @return Reactive dataframe of all reservations that fall above 3rd quartile value for given language category at user picked site
#'
#' @examples

language_top_quartile_res_data <- function(siteInput, language_group, weighted_quartile, ridb_df){
  
  # reactive data frame 
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
      # filter to user site of choice
      filter(park %in% siteInput) %>%
      # select the variables of interest
      select(park, customer_zip, 
             english_only, not_english_only) %>% 
      pivot_longer(cols = 3:4, 
                   names_to = "language",
                   values_to = "language_percentage") %>% 
      # filter for specific language category
      filter(language == language_group) %>% 
      drop_na(language_percentage) %>% 
      # filter rows that fall above 3rd quartile value
      filter(language_percentage >= weighted_quartile) %>% 
      # summarize to inner quartile range, median, and total reservations
      summarize(count = n()) %>% 
      # updated racial category name strings for plotting
      mutate(language = paste0(language_group)) %>% 
      relocate(language, .before = 1) %>% 
      mutate(language = str_replace_all(string = language,
                                        pattern = "_",
                                        replacement = " "),
             language = str_replace(string = language,
                                    pattern = "^english only$",
                                    replacement = "speak only English at home"),
             language = str_replace(string = language,
                                    pattern = "^not english only$",
                                    replacement = "speak language(s) other than English at home"),
             language_y_lab = case_when(language == "speak only English at home" ~ 
                                          "Only English At Home",
                                        language == "speak language(s) other than English at home" ~ 
                                          "Language(s) Other Than English At Home"))
  })
  
  return(rdf())
  
} # EO function
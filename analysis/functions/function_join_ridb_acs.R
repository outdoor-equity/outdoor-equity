
#' Title
#'
#' @param ridb_df 
#' @param acs_df_race 
#' @param acs_df_education 
#' @param acs_df_median_income 
#' @param acs_df_language 
#'
#' @return
#'
#' @examples
join_ridb_acs_data <- function(ridb_df, acs_df_race, acs_df_education, 
                               acs_df_median_income, acs_df_language){
    # join input dfs
    df <- 
      left_join(x = ridb_df,
                y = acs_df_race,
                by = c("customer_zip" = "zip_code")) %>% 
      left_join(y = acs_df_education,
                by = c("customer_zip" = "zip_code")) %>% 
      left_join(y = acs_df_median_income,
                by = c("customer_zip" = "zip_code")) %>% 
      left_join(y = acs_df_language,
                by = c("customer_zip" = "zip_code"))
    
    # create df
    return(df)
  }
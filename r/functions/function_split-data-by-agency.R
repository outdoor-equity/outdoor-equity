## Function breaking - in progress


# split_data_by_agency <- function(clean_data_df) {
#   for (i in unique(clean_data_df$agency)) {
#     i_agency <- clean_data_df %>% filter(agency == i)
#     assign(x = paste0(clean_data_df, "_", i), 
#            value = data.frame(i_agency), 
#            envir = .GlobalEnv)
#   }
# }


# # test function
# split_data_by_agency(clean_data_df = ridb_2011)
# 
# 
# clean_data_df <- ridb_2011
# i <- "BLM"

# # test
# i = "BLM"
# i_agency <- ridb_2011 %>% filter(agency == i)
# assign(paste0(ridb_2011, "_", i), 
#        data.frame(i_agency), 
#        envir = .GlobalEnv)

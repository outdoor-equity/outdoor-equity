# library(ridbAPI) cannot use because not robust enough and it's a third party package so not sure how reliable that is
# https://ridb.recreation.gov/api/v1/activities?limit=50&offset=0&apikey=

library(httr)
library(jsonlite)
library(tidyverse)

api_txt <- "../../Desktop/private/ridb_api.txt"

response <- httr::GET("https://ridb.recreation.gov/api/v1/activities",
          query = list(limit = 5, 
                       offset = 0, 
                       apikey = "api_txt"))

content(response, "text")

fromJSON()




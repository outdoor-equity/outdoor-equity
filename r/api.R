# library(ridbAPI) cannot use because not robust enough

library(httr)
library(jsonlite)
# https://ridb.recreation.gov/api/v1/activities?limit=50&offset=0&apikey=8cd74694-f270-4b1d-9ace-4509bd98fd92

response <- httr::GET("https://ridb.recreation.gov/api/v1/activities",
          query = list(limit = 5, 
                       offset = 0, 
                       apikey = "8cd74694-f270-4b1d-9ace-4509bd98fd92"))

content(response, "text")

fromJSON()




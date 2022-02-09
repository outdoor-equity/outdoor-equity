
library(tidyverse)
library(httr)
library(jsonlite)

res <- GET("https://ridb.recreation.gov/docs") #this website doesn't seem to be helpful

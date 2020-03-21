## examples of how to get tidy tuesday sets via tidytuesdayR package

library(tidytuesdayR)
library(tidyverse)
library(tidylog)

## lists available sets in the viewer
tt_available()

# read in object
tt_bike <- tt_load("2019-11-05")
tt_bike_gh <- tt_load_gh("2019-11-05")

glimpse(tt_bike_gh)
# loads readme
readme(tt_bike_gh)

# change object to dataframe
tt_bikedf <- tt_bike_gh %>%
  tt_read_data("commute.csv")
  
glimpse(tt_bikedf)

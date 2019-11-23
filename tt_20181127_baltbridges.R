library(tidytuesdayR)
library(tidyverse)
library(tidylog)

today <- Sys.Date()
today_yr <- as.numeric(format(today, format="%Y"))
## lists available sets in the viewer
tt_available()


tt_balt1 <- tt_load("2018-11-27")
readme(tt_balt1)
ls(tt_balt1)

tt_baltdf <- as.data.frame(tt_balt1["baltimore_bridges"]) %>% 
  mutate(vehicles_n = as.numeric(str_remove(baltimore_bridges.vehicles, " vehicles"))) %>%
  mutate(age = today_yr - baltimore_bridges.yr_built)

glimpse(tt_baltdf)

# remove prefix from col names
colnames(tt_baltdf) <- sub("baltimore_bridges.", "", names(tt_baltdf), fixed = TRUE)
glimpse(tt_baltdf)

tt_baltdf %>%
  count(vehicles)

tt_baltdf %>%
  summarise()
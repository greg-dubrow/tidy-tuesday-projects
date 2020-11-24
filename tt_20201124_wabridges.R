library(tidytuesdayR)
library(tidyverse)
library(tidylog)

# read in object
tt_wabridge <- tt_load("2020-11-24")

glimpse(tt_wabridge)
# loads readme
readme(tt_wabridge)

### change object to dataframe, start cleaning & prepping
# 1) create columns for miles, direction, type from length
# 2) create specific location columns frolm location
# 3) change rafing, gain and highpoint to numeric
tt_wabridgedf <- tt_wabridge$hike_data %>%
  mutate(length_miles = as.numeric(str_extract(length, "^[^\\s]+"))) %>%
  mutate(trail_type = case_when(grepl("roundtrip", length) ~ "Round trip",
                          grepl("one-way", length) ~ "One Way",
                          grepl("of trails", length) ~ "Trails")) %>%
  # mutate(length_txt = str_extract(length, "\\s.+")) %>%
  # mutate(length_txt = str_trim(length_txt)) %>%
  # mutate(length_dir = str_extract(length_txt, '\\b[^,]+$')) %>%
  #mutate(location_region = str_extract(location, '\\b[^--]+$')) %>%
  #mutate(location_region = gsub("(.*)\\s[-][-].*","\\1",location)) %>%
  mutate(location_split = location) %>%
  separate(location_split, c("location_region","location_specific"), sep = ' -- ') %>%
  mutate(gain = as.numeric(gain)) %>%
  mutate(highpoint = as.numeric(highpoint)) %>%
  mutate(rating = as.numeric(rating)) %>%
  mutate(rating_grp = case_when(rating == 0 ~ "0",
                                rating >0 & rating < 2 ~ "1",
                                rating >=2 & rating < 3 ~ "2",
                                rating >=3 & rating < 4 ~ "3",
                                rating >=4 & rating < 5 ~ "4",
                                rating == 5 ~ "5")) %>%
  select(name, location_region, location_specific, trail_type, length_miles, 
         gain, highpoint, rating, rating_grp, features, description, location, length)


glimpse(tt_wabridgedf)

tt_wabridgedf %>%
  count(rating_grp, rating) %>%
  view()
  
tt_wabridgedf %>%
  group_by(location_region) %>%
  summarise(n_region = n(),
    avglength = mean(length_miles),
    avgrating = mean(rating),
    avggain = mean(gain),
    avghigh = mean(highpoint),
    minhigh = min(highpoint),
    maxhigh = max(highpoint))

tt_wabridgedf %>%
  ggplot(aes(length_miles, gain)) +
  geom_point() +
  facet_wrap(vars(rating_grp))

tt_wabridgedf %>%
  select(rating, length_miles, gain, highpoint) %>%
  corrr::correlate() %>%
  corrr::rplot(print_cor = TRUE) 


library(tidytuesdayR)
library(tidyverse)
library(tidylog)
library(tdf)
#library(tidygraph)
#library(ggraph)
library(gt)
library(reactable)
library(htmltools)
library(patchwork)
library(webshot)

# create notin operator
`%notin%` <- negate(`%in%`)


## overall race results for finishers up to 2020...need to figure out how to merge with tdf package sets
tdf_bigset <- read.csv("https://github.com/camminady/LeTourDataSet/blob/master/Riders.csv?raw=true") %>%
  mutate(Rider = str_to_title(Rider)) %>%
  rename(rownum = X)

glimpse(tdf_bigset)

tdf2020 <- tdf_bigset %>%
  filter(Year == 2020) %>%
  mutate(edition = 107) %>%
  view()

# load main file from tt repo
tt_tdf <- tidytuesdayR::tt_load('2020-04-07')

# unpack the three datasets

# create race winners set. comes from tdf package. includes up to 2019
tdf_winners <- as_tibble(tt_tdf$tdf_winners)
glimpse(tdf_winners)

tdf_winners %>%
  filter(edition == 1) %>%
  view()

# create stage winner set. in tt file, comes from kaggle, includes up to 2017
tdf_stagewin <- tt_tdf$tdf_stages %>%
  mutate(race_year = lubridate::year(Date)) %>% 
  mutate(Stage = ifelse(Stage == "P", "0", Stage)) %>%
  mutate(stage_ltr = case_when(str_detect(Stage, "a") ~ "a",
                               str_detect(Stage, "b") ~ "b",
                               str_detect(Stage, "c") ~ "c",
                               TRUE ~ "")) %>%
  mutate(stage_num = str_remove_all(Stage, "[abc]")) %>%
  mutate(stage_num = stringr::str_pad(stage_num, 2, side = "left", pad = 0)) %>% 
  mutate(stage_results_id = paste0("stage-", stage_num, stage_ltr)) %>%
  mutate(split_stage = ifelse(stage_ltr %in% c("a", "b", "c"), "yes", "no")) %>%
  select(race_year, stage_results_id, stage_date = Date, Type, split_stage,
         Origin, Destination, Distance, Winner, Winner_Country, everything())

glimpse(tdf_stagewin)

## stage data in CSV from tt repository messed up the times. need to pull from tdf package
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-07/readme.md using
# the cleaning script. some operations will take a while. Includes up to 2019

glimpse(tdf::editions)
stage_all_nest <- tdf::editions$stage_results

glimpse(stage_all_nest$`1938`)

tdfstages_1938 <- stage_all_nest$`1938` %>%
  flatten_df()

glimpse(tdfstages_1938)

all_years <- tdf::editions %>% 
  unnest_longer(stage_results) %>% 
  mutate(stage_results = map(stage_results, ~ mutate(.x, rank = as.character(rank)))) %>% 
  unnest_longer(stage_results) 

glimpse(all_years)

stage_all <- all_years %>% 
  select(stage_results) %>% 
  flatten_df()

glimpse(stage_all)

combo_df <- bind_cols(all_years, stage_all) %>% 
  select(-stage_results)

glimpse(combo_df)

combo_df %>%
  count(rank) %>%
  view()

tdf_stagedata <- as_tibble(combo_df %>% 
  select(edition, start_date,stage_results_id:last_col()) %>% 
  mutate(year = lubridate::year(start_date)) %>% 
  rename(age = age...25) %>% 
  
  # to add leading 0 to stage, extract num, create letter, add 0s to num, paste
  mutate(stage_num = str_replace(stage_results_id, "stage-", "")) %>%
  mutate(stage_ltr = case_when(str_detect(stage_num, "a") ~ "a",
                               str_detect(stage_num, "b") ~ "b",
                               TRUE ~ ""))) %>%
  mutate(stage_num = str_remove_all(stage_num, "[ab]")) %>%
  mutate(stage_num = stringr::str_pad(stage_num, 2, side = "left", pad = 0)) %>%
  mutate(stage_results_id2 = paste0("stage-", stage_num, stage_ltr)) %>%
  mutate(split_stage = ifelse(stage_ltr %in% c("a", "b"), "yes", "no")) %>% 
  
  # fix 1000s rank. change to DNF
  mutate(rank = ifelse(rank %in% c("1003", "1005", "1006"), "DNF", rank)) %>%
  mutate(rank2 = ifelse(rank %notin% c("DF", "DNF", "DNS", "DSQ","NQ","OTL"), 
                        stringr::str_pad(rank, 3, side = "left", pad = 0), rank)) %>% 
  select(-stage_results_id, -start_date, ) %>%
  select(edition, year, stage_results_id = stage_results_id2, split_stage, rider, rank2, 
         time, elapsed, points, bib_number, team, age, everything())

glimpse(tdf_stagedata)
saveRDS(tdf_stagedata, "data/tdf_stagedata.rds")

tdf_stagedata <- readRDS("data/tdf_stagedata.rds")

tdf_stagedata %>%
  filter(year == 1938) %>%
#  filter(stage_results_id == "stage-10") %>%
  view()

tdf_stagedata %>%
  count(year, stage_results_id) %>%
  arrange(year, stage_results_id) %>%
  view()

## stage winners
glimpse(tdf_stagewin)
tdf_stagewin %>%
  count(Date) %>%
  view()
glimpse(tdf_winners)


## detailed stage data
glimpse(tdf_stagedata)

tdf_stagedata %>%
#  filter(rank %in% c("1003", "1005", "1006")) %>%
  filter(year == 2019, stage_results_id == "stage-02") %>% 
  view()
  count(year, stage_results_id) %>%
  arrange(year, stage_results_id) %>%
  view()


tdf_stagedata %>%
  count(rank2, rank) %>%
  arrange(rank2, rank) %>%
  view()

## race winners
tdf_winners %>%
  count(start_date) %>%
  view()



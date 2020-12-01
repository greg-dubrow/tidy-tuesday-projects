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
glimpse(tt_tdf)

# unpack the three datasets

# create race winners set. comes from tdf package. includes up to 2019
tdf_winners <- as_tibble(tt_tdf$tdf_winners)
glimpse(tdf_winners)

tdf_winners %>%
  filter(edition == 1) %>%
  view()

# create stage winner set. in tt file, comes from kaggle, includes up to 2017
tdf_stagewin1 <- tt_tdf$tdf_stages %>%
  mutate_if(is.character, str_trim)
  
glimpse(tdf_stagewin1)

# pulled 2018 - 2020 from wikipedia
# read in excel - need to separate route field to Origin & Destination
tdf_stagewin2 <- readxl::read_excel("data/tdf_stagewinners_2018-20.xlsx") %>%
  mutate(Stage = as.character(Stage)) %>%
  mutate(Date = lubridate::as_date(Date)) %>% 
  separate(Course, c("Origin", "Destination"), "to", extra = "merge") %>%
  mutate_if(is.character, str_trim) %>%
  select(Stage, Date, Distance, Origin, Destination, Type, Winner, Winner_Country = Winner_country)

glimpse(tdf_stagewin2)

tdf_stagewin <- rbind(tdf_stagewin1, tdf_stagewin2) %>%
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
  
  # extract first and last names from winner field
  mutate(winner_first = str_match(Winner, "(^.+)\\s")[, 2]) %>%
  mutate(winner_last= gsub(".* ", "", Winner)) %>%

  # clean up stage type
  mutate(stage_type = case_when(Type %in% c("Flat cobblestone stage", "Flat stage", "Flat",
                                            "Flat Stage", "Hilly stage", "Plain stage", 
                                            "Plain stage with cobblestones") 
                                ~ "Flat / Plain / Hilly",
                                Type %in% c("High mountain stage", "Medium mountain stage",
                                            "Mountain stage", "Mountain Stage", "Stage with mountain",
                                            "Stage with mountain(s)", "Transition stage")
                                ~ "Mountain",
                                Type %in% c("Individual time trial", "Mountain time trial") 
                                ~ "Time Trail - Indiv",
                                Type == "Team time trial" ~ "Time Trail - Team",
                                TRUE ~ "Other")) %>% 
  mutate_if(is.character, str_trim) %>%
  arrange(desc(race_year), stage_results_id) %>%
  select(race_year, stage_results_id, stage_date = Date, stage_type, Type, split_stage,
         Origin, Destination, Distance, Winner, winner_first, winner_last,
         Winner_Country, everything())

glimpse(tdf_stagewin)

tdf_stagewin %>%
  filter(race_year == 1967) %>%
  view()
  count(stage_type, Type)


## stage data in CSV from tt repository messed up the times. need to pull from tdf package
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-04-07/readme.md using
# the cleaning script. some operations will take a while. Includes up to 2019

glimpse(tdf::editions)
stage_all_nest <- tdf::editions$stage_results

glimpse(stage_all_nest$`1938`)

tdfstages_1938 <- stage_all_nest$`1938` %>%
  flatten_df()

glimpse(tdfstages_1938)

## from tdf package cleaning script
all_years <- tdf::editions %>% 
  unnest_longer(stage_results) %>% 
  mutate(stage_results = map(stage_results, ~ mutate(.x, rank = as.character(rank)))) %>% 
  unnest_longer(stage_results) 

stage_all <- all_years %>% 
  select(stage_results) %>% 
  flatten_df()

combo_df <- bind_cols(all_years, stage_all) %>% 
  select(-stage_results)

tdf_stagedata <- as_tibble(combo_df %>% 
  select(edition, start_date,stage_results_id:last_col()) %>% 
  mutate(race_year = lubridate::year(start_date)) %>% 
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
  
  # extract first and last names from rider field
  mutate(rider_last = str_match(rider, "(^.+)\\s")[, 2]) %>%
  mutate(rider_first= gsub(".* ", "", rider)) %>%
  mutate(rider_firstlast = paste0(rider_first, " ", rider_last)) %>%
  select(-stage_results_id, -start_date, ) %>%
  
  # fix 1967 & 1968
  mutate(stage_results_id2 = ifelse((race_year %in% c(1967, 1968) & stage_results_id2 == "stage-00"),
         "stage-01a", stage_results_id2)) %>%
  mutate(stage_results_id2 = ifelse((race_year %in% c(1967, 1968) & stage_results_id2 == "stage-01"),
         "stage-01b", stage_results_id2)) %>%
  mutate(split_stage = ifelse((race_year %in% c(1967, 1968) & stage_results_id2 %in% c("stage-01a", "stage-01b")),
         "yes", split_stage)) %>%
  
  select(edition, race_year, stage_results_id = stage_results_id2, split_stage, 
         rider, rider_first, rider_last, rider_firstlast, rank2, 
         time, elapsed, points, bib_number, team, age, everything())

glimpse(tdf_stagedata)
saveRDS(tdf_stagedata, "data/tdf_stagedata.rds")

tdf_stagedata <- readRDS("data/tdf_stagedata.rds")

tdf_stagedata %>%
  filter(race_year == 2017 & stage_results_id  == "stage-04") %>%
  select(rider, bib_number,
         rank, time, elapsed) %>%
  view()

##### Analysis - 

### changes by stage / race in time of stage winner and spread between winner & last man
glimpse(tdf_stagedata)
glimpse(tdf_stagewin)

# merge stage data and stage winner data for full range of fields. keep all as stage data 
# in split stages doeesn't have all stages and duped results 
tdf_stageall <- merge(tdf_stagedata, tdf_stagewin, by.x = c("race_year", "stage_results_id"),
                      by.y = c("race_year", "stage_results_id"), all = T)
glimpse(tdf_stageall)

# tdf_stageall %>%
#   filter(race_year == 1967) %>%
#   filter(is.na(stage_type)) %>%
#   arrange(stage_results_id) %>%
#   view()
# 
# tdf_stageall %>%
#   filter(is.na(stage_ltr.x)) %>%
#   filter(stage_ltr.y %in% c("a", "b", "c")) %>%
#   view()
# 
# tdf_stageall %>%
# #  distinct(race_year, stage_results_id, .keep_all = TRUE) %>%
# #  filter(Type %in% c("Transition stage")) %>%
#   filter(is.na(stage_type)) %>%
#   arrange(race_year, stage_results_id, rank2) %>%
#   view()

### changes over time in final winner time and 1st - last spread, 
   #### normalized by race length & ratio of stage types

stage_gap <-
tdf_stageall %>%
  #  delete 1995 stage 16 - neutralized due to death in stage 15, all times the same
  mutate(out = ifelse((race_year == 1995 & stage_results_id == "stage-16"),
                       "drop", "keep")) %>%
  filter(out != "drop") %>%
  # delete  missing times
  filter(!is.na(time)) %>%
  # remove dupliate times
   arrange(race_year, stage_results_id, rank) %>%
  
  # distinct(race_year, stage_results_id, time, .keep_all = TRUE) %>%
  # ungroup() %>%
  # remove non-finishers/starters, change outside time limit rank to numeric to keep in set
  #filter(time != "0S") %>%
  filter(rank %notin% c("DF", "DNF", "DNS", "DSQ", "NQ")) %>%
  filter(!is.na(rank)) %>%
  mutate(rank_clean = case_when(rank == "OTL" ~ "999",
                           TRUE ~ rank)) %>% 
  mutate(rank_n = as.integer(rank_clean)) %>%
  # creates total time in minutes as numeric
  mutate(time_minutes = ifelse(!is.na(elapsed),
                              day(elapsed)*1440 + hour(elapsed)*60 + minute(elapsed) + second(elapsed)/60,
                               NA)) %>%
  mutate(time_minutes = round(time_minutes, 2)) %>%
  # create rank field to use to select winner, next best, last
  group_by(race_year, stage_results_id) %>% 
  arrange(race_year, stage_results_id, time_minutes) %>%
  mutate(rank_mins = rank(time_minutes)) %>%
  mutate(gap_keep = case_when(rank_mins %in% c(1, 2) ~ "keep",
                             time_minutes == max(time_minutes) ~ "keep",
                              TRUE ~ "drop")) %>%
  # field for winner, 2nd best, last
  mutate(compare_grp = case_when(rank == "1" ~ "Winner",
                                 rank_mins == 2 ~ "Next best",
                                 rank_mins == max(rank_mins) ~ "Last")) %>%
  mutate(compare_grp = factor(compare_grp, levels = c("Winner", "Next best", "Last"))) %>%
  # calculate time from stage winner
  mutate(time_diff = time_minutes - lag(time_minutes, default = first(time_minutes))) %>%
  mutate(time_diff_secs = time_diff*60) %>%
  mutate(time_diff = round(time_diff, 2)) %>%
  mutate(time_diff_secs = round(time_diff_secs, 0)) %>%
  mutate(time_diff_period = seconds_to_period(time_diff_secs)) %>%
  ungroup() %>%
  # keep only winner, next best and last
#  filter(gap_keep == "keep") %>%
  # create race decade field
  mutate(race_decade = floor(race_year / 10) * 10) %>%
  mutate(race_decade = as.character(paste0(race_decade, "s"))) %>%
  
  select(race_year, race_decade, stage_results_id, stage_type, rider_firstlast, bib_number, Winner_Country,
         rank, rank_clean, rank_n, time, elapsed, time_minutes, time_diff, time_diff_secs, time_diff_period, rank_mins, compare_grp) 

%>%
  view()

glimpse(stage_gap)

stage_gap %>%
  count(compare_grp, rank) %>%
  view()

stage_gap %>%
  filter(race_year == 1971, stage_results_id == "stage-00") %>%
  view()

tdf_stageall %>%
  filter(race_year == 1971, stage_results_id == "stage-00") %>%
  view()

# some stages had no gap except winner & last
stage_gap %>%
  filter(compare_grp != "Winner") %>%
  filter(stage_type != "Other") %>%
  group_by(stage_type, compare_grp) %>%
  summarise(num = n(), 
            avggap_next = mean(time_diff_period),
            avggap_next2 = round(seconds_to_period(mean(time_diff_secs)), 2))
  
# stage types
tdf_stageall %>%
  distinct(race_year, stage_results_id, .keep_all = TRUE) %>%
  count(stage_type)

## top 20 stage rides per rider
tdf_stageall %>%
  count(rider_firstlast) %>%
  view()

tdf_stageall %>%
  filter(rider_last == "Cist") %>%
  view()

### first & last names of stage winners
library(tidytuesdayR)
library(tidyverse)
library(tidylog)
library(boom)
library(kapow)
library(DataExplorer)
library(janitor)
library(ggtext)
library(ggrepel)
#library(tidygraph)
#library(ggraph)
library(gt)
library(reactable)
library(htmltools)
library(patchwork)
library(webshot)

library(wpp2019)


pctcalc <- function(x) {
  x / first(x) 
}

pctchange <- function(x) {
  (x - lag(x)) / lag(x)
}

# read in object
tt_hbcu_load <- tt_load("2021-02-02")

# tt_hbcu_bach <- as_tibble(tt_hbcu_load$bach_students)
# glimpse(tt_hbcu_bach)

tt_hbcu_all <- as_tibble(tt_hbcu_load$hbcu_all) %>%
  clean_names() %>%
  mutate(ethnicity = "All") %>%
  mutate(year = as.character(year)) %>%
  select(year, ethnicity, enroll_n = total_enrollment, women = females, men = males, 
         four_year_all = x4_year, two_year_all = x2_year,
         total_public, four_year_pub = x4_year_public, two_year_pub = x2_year_public, 
         total_private, four_year_pri = x4_year_private, two_year_pri = x2_year_private)  
glimpse(tt_hbcu_all)

tt_hbcu_all_pctchg <- tt_hbcu_all  %>% 
  arrange(year) %>%
#  mutate(across(3:13, pctchange, .names = "pctch_{.col}")) %>%
  mutate(across(3:13, pctchange))
glimpse(tt_hbcu_all_pctchg)

tt_hbcu_black <- as_tibble(tt_hbcu_load$hbcu_black) %>%
  clean_names() %>%
  mutate(year = as.character(year)) %>%
  mutate(ethnicity = "Black") %>%
  select(year, ethnicity, enroll_n = total_enrollment, women = females, men = males, 
         four_year_all = x4_year, two_year_all = x2_year,
         total_public, four_year_pub = x4_year_public, two_year_pub = x2_year_public, 
         total_private, four_year_pri = x4_year_private, two_year_pri = x2_year_private) 
glimpse(tt_hbcu_black)

tt_hbcu_black_pctchg <- tt_hbcu_black  %>% 
  arrange(year) %>%
  mutate(across(3:13, pctchange)) %>%
  select(year, ethnicity, everything()) 
# %>%
#   rename_with(~ paste(.x, "bl", sep = "_"), .cols = (3:23))
glimpse(tt_hbcu_black_pctchg)

tt_hbcu_notblack = as.data.frame(tt_hbcu_all) %>%
  select(-ethnicity) %>%
  rename_with(~ paste(.x, "all", sep = "_"), .cols = (2:12)) %>%
  bind_cols(tt_hbcu_black) %>%
  select(-year...13, -ethnicity) %>%
  rename(year = year...1) %>%
  mutate(enroll_n_nb = enroll_n_all - enroll_n) %>%
  mutate(women_nb = women_all - women) %>%
  mutate(men_nb = men_all - men) %>%
  mutate(four_year_all_nb = four_year_all_all - four_year_all) %>%
  mutate(two_year_all_nb = two_year_all_all - two_year_all) %>%
  mutate(total_public_nb = total_public_all - total_public) %>%
  mutate(four_year_pub_nb = four_year_pub_all - four_year_pub) %>%
  mutate(two_year_pub_nb = two_year_pub_all - two_year_pub) %>%
  mutate(total_private_nb = total_private_all - total_private) %>%
  mutate(four_year_pri_nb = four_year_pri_all - four_year_pri) %>%
  mutate(two_year_pri_nb = two_year_pri_all - two_year_pri) %>%
  mutate(ethnicity = "Not Black") %>%
  select(year, ethnicity, enroll_n_nb:two_year_pri_nb) %>%
  rename_with(~ str_remove(.x, "_nb"), .cols = (3:13))

glimpse(tt_hbcu_notblack)

tt_hbcu_notblack_pctchg <- tt_hbcu_notblack  %>% 
  arrange(year) %>%
  mutate(across(3:13, pctchange)) %>%
  select(year, ethnicity, everything()) %>%
  rename_with(~ str_remove(.x, "_nb"), .cols = (3:13)) 
glimpse(tt_hbcu_notblack_pctchg)

tt_hbcu_pctchg <- as.data.frame(rbind(tt_hbcu_black_pctchg, tt_hbcu_notblack_pctchg)) %>% 
  rbind(tt_hbcu_all_pctchg) %>%
  arrange(year, ethnicity) %>%
  filter(year > "1989")
glimpse(tt_hbcu_pctchg)

## can't do gender by sector so two dfs - one eth & gender, one eth & sector
# note sex pct is by eth group
tt_hbcu_enr_eth_sex = as.data.frame(rbind(tt_hbcu_all, tt_hbcu_black)) %>%
  rbind(tt_hbcu_notblack) %>%
  select(-four_year_all:-two_year_pri) %>%
  arrange(year, ethnicity) %>% 
  group_by(year) %>%
   mutate(enroll_eth_pct = enroll_n / first(enroll_n)) %>%
  ungroup() %>%
  pivot_longer(cols = women:men,
               names_to = "sex",
               values_to = "sex_n") %>%
  arrange(year, ethnicity) %>%
  group_by(year, ethnicity) %>%
  mutate(enroll_sex_pct = sex_n / sum(sex_n)) %>%
  ungroup() %>%
  select(year, ethnicity, enroll_n, enroll_eth_pct, sex, sex_n, enroll_sex_pct) %>%
  arrange(year, ethnicity, sex)

glimpse(tt_hbcu_enr_eth_sex)

# note pct_sect_eth is by eth group by year, pct_eth_sect is pct eth w/in sector
tt_hbcu_enr_eth_sect = rbind(tt_hbcu_all, tt_hbcu_black) %>%
  rbind(tt_hbcu_notblack) %>%
  select(-women, -men) %>%
  arrange(year, ethnicity) %>%
  pivot_longer(cols = four_year_all:two_year_pri,
               names_to = "sector", 
               values_to = "sector_n") %>%
  arrange(year, ethnicity) %>%
  mutate(pct_sect_eth = sector_n / enroll_n) %>%
  arrange(year, sector) %>%
  group_by(year, sector) %>%
  mutate(pct_eth_sect = sector_n / (sum(sector_n) /2)) %>%
  ungroup()

glimpse(tt_hbcu_enr_eth_sect)

# use dataexplorer package for EDA

## delta cost tuition - large dataset so Delta split to two files. Extract & modify each, then join
delta0015all <- (haven::read_sas("~/Data/ipeds/delta_public_release_00_15.sas7bdat", NULL))
glimpse(delta0015all)

delta0015all %>%
  count(sector, sector_revised)

delta0015_tuitenr <- delta0015all %>%
  filter(between(sector_revised, 1, 6)) %>%
  mutate(sector_desc = case_when(sector_revised == 1 ~	"Public 4yr",
                                 sector_revised == 2	~ "Private nonprofit 4yr",
                                 sector_revised == 3	~ "Private for-profit 4yr",
                                 sector_revised == 4	~ "Public 2yr",
                                 sector_revised == 5	~ "Private nonprofit 2yr",
                                 sector_revised == 6	~ "Private for-profit 2yr")) %>%
 # mutate(total_undergraduates = ifelse(is.na(total_undergraduates), 0, total_undergraduates)) %>%
  select(unitid, instname, sector_revised, sector_desc, hbcu,
         year = academicyear, tuition_fee_ug_in = tuitionfee02_tf, 
         tuition_fee_ug_oos = tuitionfee03_tf, 
         total_undergraduates)

glimpse(delta0015_tuitenr)

delta8799all <- (haven::read_sas("~/Data/ipeds/delta_public_release_87_99.sas7bdat", NULL))
glimpse(delta8799all)

delta8799_tuitenr <- as_tibble(delta8799all) %>%
  filter(between(sector_revised, 1, 6)) %>%
  mutate(sector_desc = case_when(sector_revised == 1 ~	"Public 4yr",
                                 sector_revised == 2	~ "Private nonprofit 4yr",
                                 sector_revised == 3	~ "Private for-profit 4yr",
                                 sector_revised == 4	~ "Public 2yr",
                                 sector_revised == 5	~ "Private nonprofit 2yr",
                                 sector_revised == 6	~ "Private for-profit 2yr")) %>%
 # mutate(total_undergraduates = ifelse(is.na(total_undergraduates), 0, total_undergraduates)) %>%
  select(unitid, instname, sector_revised, sector_desc, hbcu,
         year = academicyear, tuition_fee_ug_in = tuitionfee02_tf, 
         tuition_fee_ug_oos = tuitionfee03_tf, 
         total_undergraduates)

glimpse(delta8799_tuitenr)

delta8799_tuitenr %>%
  count(sector_revised)

tuitenr_8715 <- rbind(delta0015_tuitenr, delta8799_tuitenr)
glimpse(tuitenr_8715)

tuitenr_8715 %>% 
  count(hbcu, sector_desc)

tuitenr_8715 %>%
  filter(sector_desc == "Public 2yr") %>%
  filter(year == 2010) %>%
  summarise(toug = sum(total_undergraduates, na.rm = TRUE))
  write_csv("pub2yr2015.csv")

# create dataset of ug enrollment & avg tuition by year w/ cols for hbcu, sector, eth
tuitenr_8715_agg <-
  tuitenr_8715 %>%
  # filter(!sector_desc == "Private for-profit 2yr" &
  #          !sector_desc == "Private for-profit 4yr") %>%
#  filter(!is.na(tuition_fee_ug_in)) %>%
  group_by(year, sector_desc, hbcu) %>%
  summarise(enr_tot = sum(total_undergraduates, na.rm = TRUE),
            mean_tuit_in = mean(tuition_fee_ug_in, na.rm = TRUE),
            mean_tuit_oos = mean(tuition_fee_ug_oos, na.rm = TRUE)
            ) %>%
  mutate(level = ifelse(str_detect(sector_desc, "2yr"), "2yr", "4yr")) %>%
#  summarise(mean_tuit_in = mean(tuition_fee_ug_in, na.rm = T)) %>%
  ungroup() %>%
  mutate(hbcu_f = ifelse(hbcu == 1, "HBCU", "Not HBCU")) %>%
  mutate(hbcu_f = factor(hbcu_f, levels = c("HBCU", "Not HBCU"))) 

glimpse(tuitenr_8715_agg)


tuitenr_8715_agg %>% 
  count(hbcu, sector_desc)

tuit_8715_pctchgh1 <- tuitenr_8715_agg %>%
  filter(hbcu == 1, sector_desc == "Public 4yr") %>%
  select(year, hbcu, sector_desc, enr_tot, mean_tuit_in, mean_tuit_oos) %>% 
  arrange(year) %>% 
  mutate(across(4:6, pctchange)) %>% 
  select(year, hbcu, sector_desc, everything()) %>%
  ungroup() %>%
  filter(year > 1987)

tuit_8715_pctchgh2 <- tuitenr_8715_agg %>%
  filter(hbcu == 1, sector_desc == "Private nonprofit 4yr") %>%
  select(year, hbcu, sector_desc, enr_tot, mean_tuit_in, mean_tuit_oos) %>% 
  arrange(year) %>% 
  mutate(across(4:6, pctchange)) %>% 
  select(year, hbcu, sector_desc, everything()) %>%
  ungroup() %>%
  filter(year > 1987)

tuit_8715_pctchgh3 <- tuitenr_8715_agg %>%
  filter(hbcu == 1, sector_desc == "Public 2yr") %>%
  select(year, hbcu, sector_desc, enr_tot, mean_tuit_in, mean_tuit_oos) %>% 
  arrange(year) %>% 
  mutate(across(4:6, pctchange)) %>% 
  select(year, hbcu, sector_desc, everything()) %>%
  ungroup() %>%
  filter(year > 1987)

tuit_8715_pctchgh4 <- tuitenr_8715_agg %>%
  filter(hbcu == 1, sector_desc == "Private nonprofit 2yr") %>%
  select(year, hbcu, sector_desc, enr_tot, mean_tuit_in, mean_tuit_oos) %>% 
  arrange(year) %>% 
  mutate(across(4:6, pctchange)) %>% 
  select(year, hbcu, sector_desc, everything()) %>%
  ungroup() %>%
  filter(year > 1987)

tuit_8715_pctchgh5 <- tuitenr_8715_agg %>%
  filter(hbcu == 2, sector_desc == "Public 4yr") %>%
  select(year, hbcu, sector_desc, enr_tot, mean_tuit_in, mean_tuit_oos) %>% 
  arrange(year) %>% 
  mutate(across(4:6, pctchange)) %>% 
  select(year, hbcu, sector_desc, everything()) %>%
  ungroup() %>%
  filter(year > 1987)

tuit_8715_pctchgh6 <- tuitenr_8715_agg %>%
  filter(hbcu == 2, sector_desc == "Private nonprofit 4yr") %>%
  select(year, hbcu, sector_desc, enr_tot, mean_tuit_in, mean_tuit_oos) %>% 
  arrange(year) %>% 
  mutate(across(4:6, pctchange)) %>% 
  select(year, hbcu, sector_desc, everything()) %>%
  ungroup() %>%
  filter(year > 1987)

tuit_8715_pctchgh7 <- tuitenr_8715_agg %>%
  filter(hbcu == 2, sector_desc == "Public 2yr") %>%
  select(year, hbcu, sector_desc, enr_tot, mean_tuit_in, mean_tuit_oos) %>% 
  arrange(year) %>% 
  mutate(across(4:6, pctchange)) %>% 
  select(year, hbcu, sector_desc, everything()) %>%
  ungroup() %>%
  filter(year > 1987)

tuit_8715_pctchgh8 <- tuitenr_8715_agg %>%
  filter(hbcu == 2, sector_desc == "Private nonprofit 2yr") %>%
  select(year, hbcu, sector_desc, enr_tot, mean_tuit_in, mean_tuit_oos) %>% 
  arrange(year) %>% 
  mutate(across(4:6, pctchange)) %>% 
  select(year, hbcu, sector_desc, everything()) %>%
  ungroup() %>%
  filter(year > 1987)

tuit_8715_pctchgh9 <- tuitenr_8715_agg %>%
  filter(hbcu == 2, sector_desc == "Private for-profit 4yr") %>%
  select(year, hbcu, sector_desc, enr_tot, mean_tuit_in, mean_tuit_oos) %>% 
  arrange(year) %>% 
  mutate(across(4:6, pctchange)) %>% 
  select(year, hbcu, sector_desc, everything()) %>%
  ungroup() %>%
  filter(year > 1987)

tuit_8715_pctchgh10 <- tuitenr_8715_agg %>%
  filter(hbcu == 2, sector_desc == "Private for-profit 2yr") %>%
  select(year, hbcu, sector_desc, enr_tot, mean_tuit_in, mean_tuit_oos) %>% 
  arrange(year) %>% 
  mutate(across(4:6, pctchange)) %>% 
  select(year, hbcu, sector_desc, everything()) %>%
  ungroup() %>%
  filter(year > 1987)

tuit_8715_pctchgh <- rbind(tuit_8715_pctchgh1, tuit_8715_pctchgh2) %>%
  rbind(tuit_8715_pctchgh3) %>%
  rbind(tuit_8715_pctchgh4) %>%
  rbind(tuit_8715_pctchgh5) %>%
  rbind(tuit_8715_pctchgh6) %>%
  rbind(tuit_8715_pctchgh7) %>%
  rbind(tuit_8715_pctchgh8) %>%
  rbind(tuit_8715_pctchgh9) %>%
  rbind(tuit_8715_pctchgh10) 


## load urban institute data

### plots
## tt hbcu data, black men v women over time

tt_hbcu_enr_eth_sex %>%
  filter(year > 1989) %>%
  ggplot(aes(year, enroll_n, group = 1)) +
  geom_line(color = "#E69F00", size = 1) +
  scale_y_continuous(labels = scales::comma_format(), breaks = scales::pretty_breaks()) +
#  scale_x_discrete(breaks = scales::pretty_breaks()) +
  facet_grid(ethnicity ~ ., scales = "free_y") +
  labs(title = "Black Enrollment at HBCUs Rising & Falling Since 1990",
       subtitle = "Non-Black enrollment slowly & streadily increasing",
       caption = "Source: Tidy Tuesday data",
       x = "", y = "Total UG Enrollment") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"),
        plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

# eth as percent of total
tt_hbcu_enr_eth_sex %>%
  filter(year > 1989) %>%
  filter(ethnicity == "Black") %>%
  distinct(year, ethnicity, .keep_all = T) %>%
  ggplot(aes(year, enroll_eth_pct, group = 1)) +
  geom_line(color = "#E69F00", size = 1.25) +
  scale_y_continuous(limits = c(.5, 1), 
                     labels = scales::percent_format()) +
  scale_x_discrete(breaks = scales::pretty_breaks()) +
  labs(title = "Slight Decrease Over Time in Percentage of Black Students Enrolled at HBCUs",
       subtitle = "Might be due to changes in how ethnicity is coded by US Dept of Ed",
       caption = "Source: Tidy Tuesday data",
       x = "", y = "Pct Black students") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        plot.title = element_text(size = 11),
        plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

# sex - total N
tt_hbcu_enr_eth_sex %>%
  filter(year > 1989) %>%
  ggplot(aes(year, sex_n, group = 1)) +
  geom_line(color = "#E69F00", size = 1) +
  scale_y_continuous(labels = scales::comma_format()) +
  # scale_y_continuous(limits = c(0, 250000),
  #                    breaks = c(0, 50000, 100000, 150000, 200000, 250000),
  #                    labels = scales::comma_format()) +
  scale_x_discrete(breaks = scales::pretty_breaks()) +
  labs(title = "Black Women Enroll in Higher Numbers Than Men at HBCUs",
       subtitle = "Same Pattern as Overall Undergrad Enrollments in the US",
       caption = "Source: Tidy Tuesday data",
       x = "", y = "Total UG Enrollment") +
  facet_grid(ethnicity ~ sex, scales = "free_y") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"),
        plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

# sex - women as pct
tt_hbcu_enr_eth_sex %>%
  filter(year > 1989) %>%
  filter(sex == "women") %>%
  ggplot(aes(year, enroll_sex_pct, group = 1)) +
  geom_line(color = "#E69F00", size = 1) +
  scale_y_continuous(limits = c(.5, .8), 
                     labels = scales::percent_format()) +
  scale_x_discrete(breaks = scales::pretty_breaks()) +
  labs(title = "Women a Greater Percentage of those Enrolled at HBCUs",
       subtitle = "Slightly higher among Black students than not Black",
       caption = "Source: Tidy Tuesday data",
       x = "", y = "Pct Women enrolled") +
  facet_grid(ethnicity ~ ., scales = "free_y") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"),
        plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

# sector - 3 charts: 1) pub v private 2) 2yr v 4yr 3) individ sectors then patchwork together
tt_hbcu_enr_eth_sect %>%
  count(sector)

# note pct_sect_eth is by eth group by year, pct_eth_sect is pct eth w/in sector

tt_hbcu_enr_eth_sect %>%
  filter(year > 1989) %>%
  filter(ethnicity %in% c("Black", "Not Black")) %>%
  filter(sector %in% c("total_private", "total_public")) %>%
  ggplot(aes(year, pct_sect_eth, group = sector, color = sector)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#56B4E9", "#E69F00")) +
  scale_x_discrete(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1), breaks = c(0, .25, .5, .75, 1)) +
  labs(title = "Public HBCUs Enroll Greater Pct of Students ",
       subtitle = "Greater Pct Black students in Private HBCU than non-Black students; 
       <span style = 'color:#56B4E9;'>Blue = Public</span> 
       <span style = 'color:#E69F00;'>Orange = Private</span>",
       caption = "Source: Tidy Tuesday data",
       x = "", y = "Percent by Publc & Private HBCUs") +
  facet_grid(ethnicity ~ .) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"),
        plot.subtitle = element_markdown(size = 8, face = "italic"),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

tt_hbcu_enr_eth_sect %>%
  filter(year > 1989) %>%
  filter(ethnicity %in% c("Black", "Not Black")) %>%
  filter(sector %in% c("four_year_all", "two_year_all")) %>%
  ggplot(aes(year, pct_sect_eth, group = sector, color = sector)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("#56B4E9", "#E69F00")) +
  scale_x_discrete(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1), breaks = c(0, .25, .5, .75, 1)) +
  labs(title = "4-year HBCUs Enroll ~ 90% of all Students",
       subtitle = "Black students more likely to be in 4-year HBCU than non-Black students; 
       <span style = 'color:#56B4E9;'>Blue = 4-year</span> 
       <span style = 'color:#E69F00;'>Orange = 2-year</span>",
       caption = "Source: Tidy Tuesday data",
       x = "", y = "Percent by 4yr & 2yr HBCUs") +
  facet_grid(ethnicity ~ .) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"),
        plot.subtitle = element_markdown(size = 8, face = "italic"),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

tt_hbcu_enr_eth_sect %>%
  filter(year > 1989) %>%
  filter(ethnicity %in% c("Black", "Not Black")) %>%
  filter(sector %in% c("four_year_pub", "four_year_pri", "two_year_pri", "two_year_pub")) %>%
  mutate(sector = str_replace(sector, "four_year_pub", "4 Year Public")) %>%
  mutate(sector = str_replace(sector, "two_year_pub", "2 Year Public")) %>%
  mutate(sector = str_replace(sector, "four_year_pri", "4 Year Private")) %>%
  mutate(sector = str_replace(sector, "two_year_pri", "2 Year Private")) %>%
#  ggplot(aes(year, enroll_sect_pct, group = sector, color = sector)) +
  ggplot(aes(year, pct_sect_eth, group = 1)) +
  geom_line(color = "#E69F00", size = 1) +
  scale_x_discrete(breaks = scales::pretty_breaks()) +
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 1), breaks = c(0, .25, .5, .75, 1)) +
  labs(title = "Black Students most likely to be in 4-year HBCUs",
       subtitle = "Non-black students moving to 2-year HBCUs from 4-yr; 
       <span style = 'color:#E69F00;'>Percents sum to 100% across ethnic groups</span>",
       caption = "Source: Tidy Tuesday data",
       x = "", y = "Percent by Sector") +
  facet_grid(ethnicity ~ sector) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "none",
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"),
        plot.subtitle = element_markdown(face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

tuitenr_8715_agg %>%
  ggplot(aes(year, enr_tot)) +
  geom_line(color = "#E69F00", group = 1) +
  #  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "Enrollment Changes by HBCU Status",
             subtitle = "Steady growth at 4-year non-HBCUs",
       caption = "Source: Delta Cost Project",
       x = "", y = "Total UG enrollment") +
  facet_grid(hbcu_f ~ sector_desc, scales = "free") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"),
        plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

enr19902015hbcu <-
tuitenr_8715_agg %>%
  filter(hbcu == 1) %>% 
  ggplot(aes(year, enr_tot)) +
  geom_line(color = "#E69F00", size = 1) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "HBCU Enrollment Across Sector 1990-2015",
       #caption = "Source: Delta Cost Project",
    x = "", y = "Total UG enrollment") +
  facet_wrap(~ sector_desc, scales = "free") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"),
        #plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

# filter(!sector_desc == "Private for-profit 2yr" &
#          !sector_desc == "Private for-profit 4yr") %>%

enr19902015nothbcu <-
  tuitenr_8715_agg %>%
  filter(hbcu == 2) %>%
#  filter(level == "2yr") %>% 
  ggplot(aes(year, enr_tot)) +
  geom_line(color = "#56B4E9", group = 1) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(title = "non-HBCU Enrollment Across Sector 1990-2015",
       caption = "Source: Delta Cost Project",
       x = "", y = "Total UG enrollment") +
  facet_wrap(~ sector_desc, scales = "free") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#56B4E9"),
        #plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

enr19902015all <- enr19902015hbcu / enr19902015nothbcu
enr19902015all

tuitenr_8715_agg %>%
  filter(hbcu == 1) %>%
  #  filter(level == "2yr") %>% 
  ggplot(aes(year, mean_tuit_in, group = 1)) +
  geom_line() +
  facet_grid(sector_desc ~., scales = "free_y")

tuitenr_8715_agg %>%
  filter(hbcu == 2) %>%
  #  filter(level == "2yr") %>% 
  ggplot(aes(year, mean_tuit_in, group = 1)) +
  geom_line() +
  facet_grid(sector_desc ~., scales = "free_y")

# slope graph of tuition change in real $ from example https://ibecav.github.io/slopegraph/
tuitsect_hbcu <-
tuitenr_8715_agg %>%
  filter(hbcu == 1 & (year == 1990 | year == 2015)) %>%
  #  filter(level == "2yr") %>% 
  ggplot(aes(year, mean_tuit_in, group = sector_desc)) +
  geom_line(aes(color = sector_desc)) +
  geom_point(aes(color = sector_desc)) +
  scale_color_manual(values = c("Public 4yr" = "#999999", "Public 2yr" = "#E69F00", 
                                "Private nonprofit 2yr" = "#56B4E9", "Private nonprofit 4yr" = "#009E73")) +
  geom_text_repel(data = tuitenr_8715_agg %>%
                    filter(hbcu == 1 & year == 1990),
                  aes(label = sector_desc), hjust = "left", nudge_x = .5,
                  direction = "y") +
  annotate("text", x = 1991, y = 12000, label = "HBCUs", size = 5, fontface = "italic") +
  scale_x_continuous(breaks = c(1990, 2015)) +
  scale_y_continuous(labels = scales::dollar_format(),
                     limits = c(0, 15000),
                     breaks = c(0, 2500, 5000, 7500, 10000, 12500, 15000)) +
  labs(x = "", y = "Average Tuition & Fees") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.minor.y = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#56B4E9"),
        panel.border = element_blank(), legend.position = "none", 
        axis.ticks = element_blank(), axis.text.x = element_blank(), axis.title.y = element_text(hjust = -.07),
        #plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

tuitsect_nonhbcu <-
  tuitenr_8715_agg %>%
  filter(hbcu == 2 & (year == 1990 | year == 2015)) %>%
  #  filter(level == "2yr") %>% 
  ggplot(aes(year, mean_tuit_in, group = sector_desc)) +
  geom_line(aes(color = sector_desc)) +
  geom_point(aes(color = sector_desc)) +
  scale_color_manual(values = c("Public 4yr" = "#999999", "Public 2yr" = "#E69F00", 
                                "Private nonprofit 2yr" = "#56B4E9", "Private nonprofit 4yr" = "#009E73",
                                "Private for-profit 2yr" = "#0072B2", "Private for-profit 4yr" = "#CC79A7")) +
  geom_text_repel(data = tuitenr_8715_agg %>%
              filter(hbcu == 2 & year == 1990),
            aes(label = sector_desc), hjust = "left", nudge_x = .5,
            direction = "y") +
  annotate("text", x = 1992, y = 20000, label = "not HBCUs", size = 5, fontface = "italic") +
  scale_x_continuous(breaks = c(1990, 2015)) +
  scale_y_continuous(labels = scales::dollar_format(),
                     limits = c(0, 25200),
                     breaks = c(0, 5000, 10000, 15000, 20000, 25000)) +
  labs(x = "", y = "") +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.minor.y = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#56B4E9"),
        panel.border     = element_blank(), 
        legend.position = "none", axis.ticks       = element_blank(),
        #plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

tuitsec <- tuitsect_hbcu / tuitsect_nonhbcu  + plot_annotation(
  title = 'Tuition & Fees at HBCUs Lower by Sector than non-HBCUs',
  subtitle = 'Private non-profit 4-yr tuition increased more than other sectors',
  caption = "Source: Delta Cost Project", 
  theme = theme(plot.subtitle = element_text(face = "italic", size = 9)))
  tuitsec


tuit_8715_pctchgh %>%
  mutate(hbcu_f = case_when(hbcu == 1 ~ "HBCU", TRUE ~ "Not HBCU")) %>%
  #  filter(hbcu == 1 & year >= 1990) %>% 
  filter(year >= 1990) %>%
  filter(sector_desc %in% c("Public 4yr", "Private nonprofit 4yr", "Public 2yr")) %>%
  ggplot(aes(year, enr_tot)) +
  geom_line(color = "#E69F00", size = 1) +
  scale_color_manual(values = c("#56B4E9", "#E69F00")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Year-over-year Percent Changes to UG Enrollment",
       subtitle = "Every sector has positive & negative swings", 
       caption = "Source: Delta Cost Project", 
       x = "", y = "Pct Chnage in-state tuition & fees") +
  facet_grid(hbcu_f ~ sector_desc) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"),
        plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))

tuit_8715_pctchgh %>%
  mutate(hbcu_f = case_when(hbcu == 1 ~ "HBCU", TRUE ~ "Not HBCU")) %>%
  #  filter(hbcu == 1 & year >= 1990) %>% 
  filter(year >= 1990) %>%
  filter(sector_desc %in% c("Public 4yr", "Private nonprofit 4yr", "Public 2yr")) %>%
  ggplot(aes(year, mean_tuit_in)) +
  geom_line(color = "#E69F00", size = 1) +
  scale_color_manual(values = c("#56B4E9", "#E69F00")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Year-over-year Percent Changes to Tuition & Fees",
       subtitle = "More volatility in 2-year sector, steady increases in every sector", 
       caption = "Source: Delta Cost Project", 
       x = "", y = "Pct Chnage in-state tuition & fees") +
  facet_grid(hbcu_f ~ sector_desc) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"),
        plot.subtitle = element_text(color = "#E69F00", face = "italic", size = 9),
        plot.caption = element_text(color = "#999999", face = "italic", size = 7))




## plot tuition/fees by pct black students?

  
## unused plots
tt_hbcu_pctchg %>%
  ggplot(aes(year, enroll_n, group = 1)) +
  geom_line(color = "#E69F00") +
  scale_y_continuous(limits = c(-.1, .1),
                     labels = scales::percent_format()) +
  labs(title = "Percent  HBCUs",
       subtitle = "Same Pattern as Overall Undergrad Enrollments in the US",
       x = "", y = "UG Enrollment % Change") +
  facet_grid(ethnicity ~ .) +
  theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), strip.background = element_rect(fill = "#E69F00"))

ggplot(tuit_8715_pctchgh, aes(enr_tot, mean_tuit_in)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(limits = c(-.4, 1),
                     labels = scales::percent_format(accuracy = 1))



## spot check enroll by race hbcu vs not to account for drop in black enroll hbcu
## urban institute package
library(educationdata)

ugenroll_race_sex_2001 <- 
  get_education_data(
    level = "college-university", source = "ipeds", topic = "fall-enrollment",
    by = list("race", "sex"), 
    filters = list(year = 2001, 
                   level_of_study = 1, 
                   class_level = 99), 
    add_labels = TRUE, csv = TRUE)

ugenroll_race_sex_2001 <- ugenroll_race_sex_2001 %>% 
  filter(degree_seeking == "Yes") %>%
  filter(ftpt == "Total") %>%
  select(-level_of_study, -degree_seeking, -class_level)

saveRDS(ugenroll_race_sex_2001, "data/ugenroll_race_sex_2001.rds")

ugenroll_race_sex_2011 <- 
  get_education_data(
    level = "college-university", source = "ipeds", topic = "fall-enrollment",
    by = list("race", "sex"), 
    filters = list(year = 2011, 
                   level_of_study = 1, 
                   class_level = 99), 
    add_labels = TRUE, csv = TRUE) 

ugenroll_race_sex_2011 <- ugenroll_race_sex_2011 %>% 
  filter(degree_seeking == "Yes") %>%
  filter(ftpt == "Total") %>%
  select(-level_of_study, -degree_seeking, -class_level)

saveRDS(ugenroll_race_sex_2011, "data/ugenroll_race_sex_2011.rds")

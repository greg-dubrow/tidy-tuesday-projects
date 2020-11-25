library(tidytuesdayR)
library(tidyverse)
library(tidylog)
#library(tidygraph)
#library(ggraph)
library(gt)
library(reactable)
library(htmltools)
library(patchwork)

# reactable code-thru example at https://themockup.blog/posts/2020-05-13-reactable-tables-the-rest-of-the-owl

# read in object
tt_watrail <- tt_load("2020-11-24")

glimpse(tt_watrail$hike_data)
# loads readme
readme(tt_watrail)


### change object to dataframe, start cleaning & prepping
# 1) create columns for miles, direction, type from length
# 2) create specific location columns frolm location
# 3) change rafing, gain and highpoint to numeric
tt_watraildf <- tt_watrail$hike_data %>%
  mutate(length_miles = parse_number(length)) %>%
  mutate(across(gain:rating, as.numeric)) %>%
  mutate(rating_grp = case_when(rating == 0 ~ "0",
                                rating >0 & rating < 2 ~ "1",
                                rating >=2 & rating < 3 ~ "2",
                                rating >=3 & rating < 4 ~ "3",
                                rating >=4 & rating < 5 ~ "4",
                                rating == 5 ~ "5")) %>%
  mutate(trail_type = case_when(grepl("roundtrip", length) ~ "Round trip",
                          grepl("one-way", length) ~ "One Way",
                          grepl("of trails", length) ~ "Trails")) %>% 
  mutate(location_split = location) %>%
  separate(location_split, c("location_region","location_specific"), sep = ' -- ') %>%
  unnest(cols = c(features)) %>% 
  mutate(feature_init = case_when(features == "Dogs allowed on leash" ~ "DA",
                                  features == "Dogs not allowed" ~ "DN",
                                  features == "Wildlife" ~ "Wl",
                                  features == "Good for kids" ~ "GK",
                                  features == "Lakes" ~ "Lk",
                                  features == "Fall foliage" ~ "FF",
                                  features == "Ridges/passes" ~ "RP",
                                  features == "Established campsites" ~ "EC",
                                  features == "Mountain views" ~ "MV",
                                  features == "Old growth" ~ "OG",
                                  features == "Waterfalls" ~ "Wf",
                                  features == "Wildflowers/Meadows" ~ "WM",
                                  features == "Rivers" ~ "Ri",
                                  features == "Coast" ~ "Co",
                                  features == "Summits" ~ "Su")) %>%
  mutate(feature_type = if_else(feature_init %in% c("DA","DN","GK"), "Companion", "Feature")) %>%
  select(name, location_region, location_specific, trail_type, length_miles, 
         gain, highpoint, rating, rating_grp, features, feature_init, feature_type, description, location, length)

### other less optimal but worth saving approaches to some of the above fields
#  mutate(length_miles = as.numeric(str_extract(length, "^[^\\s]+"))) %>%
# mutate(length_txt = str_extract(length, "\\s.+")) %>%
# mutate(length_txt = str_trim(length_txt)) %>%
# mutate(length_dir = str_extract(length_txt, '\\b[^,]+$')) %>%
#mutate(location_region = str_extract(location, '\\b[^--]+$')) %>%
#mutate(location_region = gsub("(.*)\\s[-][-].*","\\1",location)) %>%


glimpse(tt_watraildf)

tt_watraildf %>%
  count(features)
  
tt_watraildf %>% 
  distinct(name, .keep_all = TRUE) %>%
  count(rating_grp, rating) %>%
  view()

# exploratory analysis - histograms & scatterplots. use patchwork to stich together plot panels
hist_length <-
tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  ggplot(aes(length_miles)) +
  geom_histogram(alpha = 0.8) +
  scale_x_log10() +
  labs(x = "Length (miles), log10")

hist_gain <-
  tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  ggplot(aes(gain)) +
  geom_histogram(alpha = 0.8) +
  scale_x_log10()

hist_high <-
  tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  ggplot(aes(highpoint)) +
  geom_histogram(alpha = 0.8) 

hist_rate <-
  tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  ggplot(aes(rating)) +
  geom_histogram(alpha = 0.8) 

(hist_length | hist_gain) /
  (hist_high | hist_rate)

tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  ggplot(aes(length_miles, gain)) +
  geom_point() +
  geom_smooth() +
  scale_x_log10() +
  labs(x = "Length (miles) log10", y = "Total Gain",
       title = "Length v Gain, by Rating Group") +
  facet_wrap(vars(rating_grp))

tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  ggplot(aes(length_miles, gain)) +
  geom_point() +
  geom_smooth() +
  scale_x_log10() +
  labs(x = "Length (miles) log 10", y = "Total Gain",
       title = "Length v Gain, by Region") +
  facet_wrap(vars(location_region))


### reactable tables
## create color palates for conidtional cell colors

make_color_pal <- function(colors, bias = 1) {
  get_color <- colorRamp(colors, bias = bias)
  function(x) rgb(get_color(x), maxColorValue = 255)
}
good_color <- make_color_pal(c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"), bias = 2)
# good_color(seq(0.1, 0.9, length.out = 12))
# 
# seq(0.1, 0.9, length.out = 12) %>%
#   good_color() %>%
#   scales::show_col()
  
byregion <-  tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  group_by(location_region) %>%
  summarise(n_region = n(),
    avglength = mean(length_miles),
    avgrating = mean(rating),
    avggain = mean(gain),
    avghigh = mean(highpoint),
    minhigh = min(highpoint),
    maxhigh = max(highpoint)) %>%
  mutate_at(vars(avglength:avgrating), round, 2) %>%
  mutate_at(vars(avggain:avghigh), round, 0) 

tbl_region <-
byregion %>%
  reactable(pagination = FALSE, compact = TRUE, 
            borderless = FALSE, striped = TRUE,
            columns = list(
              location_region = colDef(name = "Region"),
              n_region = colDef(name = "N"),
              avglength = colDef(
                name = "Avg Length (miles) ", align = "center",
                style = function(value) {
                  value
                  normalized <- (value - min(byregion$avglength)) /
                    (max(byregion$avglength) - min(byregion$avglength))
                  color <- good_color(normalized)
                  list(background = color)
                }), 
              avgrating = colDef(
                name = "Avg Rating", align = "center",
                style = function(value) {
                  value
                  normalized <- (value - min(byregion$avgrating)) /
                    (max(byregion$avgrating) - min(byregion$avgrating))
                  color <- good_color(normalized)
                  list(background = color)
                }),
              avggain = colDef(
                name = "Avg Gain", align = "center",
                format = colFormat(separators = TRUE),
                style = function(value) {
                  value
                  normalized <- (value - min(byregion$avggain)) /
                    (max(byregion$avggain) - min(byregion$avggain))
                  color <- good_color(normalized)
                  list(background = color)
                }),
              avghigh = colDef(
                name = "Avg High Point", align = "center",
                format = colFormat(separators = TRUE),
                style = function(value) {
                  value
                  normalized <- (value - min(byregion$avghigh)) /
                    (max(byregion$avghigh) - min(byregion$avghigh))
                  color <- good_color(normalized)
                  list(background = color)
                }),
              minhigh = colDef(
                name = "Min High Point", align = "center",
                format = colFormat(separators = TRUE),
                style = function(value) {
                  value
                  normalized <- (value - min(byregion$minhigh)) /
                    (max(byregion$minhigh) - min(byregion$minhigh))
                  color <- good_color(normalized)
                  list(background = color)
                }),
              maxhigh = colDef(
                name = "Max High Point", align = "center",
                format = colFormat(separators = TRUE),
                style = function(value) {
                  value
                  normalized <- (value - min(byregion$maxhigh)) /
                    (max(byregion$maxhigh) - min(byregion$maxhigh))
                  color <- good_color(normalized)
                  list(background = color)
                })))

div(class = "region",
    div(class = "example-header",
        div(class = "title", 
            "Averages by Trail Region"),
        tbl_region
    )
)

## reactable table of averages by rating group
tt_watraildf %>%
  group_by(rating_grp) %>%
  summarise(n_region = n(),
            avglength = mean(length_miles),
            avggain = mean(gain),
            medgain = median(gain),
            avghigh = mean(highpoint),
            medhigh = median(highpoint),
            minhigh = min(highpoint),
            maxhigh = max(highpoint)) %>%
  gt()

tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  ggplot(aes(length_miles, gain)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Length (miles)", y = "Total Gain",
       title = "Length v Gain, by Rating Group") +
  facet_wrap(vars(rating_grp))

tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  ggplot(aes(length_miles, gain)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Length (miles)", y = "Total Gain",
       title = "Length v Gain, by Region") +
  facet_wrap(vars(location_region))

tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  filter(gain < 10000) %>%
  ggplot(aes(length_miles, gain)) +
  geom_point() +
  geom_smooth() +
  labs(x = "Length (miles)", y = "Total Gain (feet)",
       title = "Length v Gain, by Rating Group",
       subtitle = "filter out gain > 10K ft (10 obs)") +
  facet_wrap(vars(rating_grp))

tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  # filter(location_region %in% c("Central Cascades" , "North Cascades",
  #                               "Southwest Washington")) %>%
  filter(gain > 15000) %>%
  filter(length_miles > 90) %>%
  select(location_region, name, length_miles, gain) %>%
  arrange(name) %>%
  view()

tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  ggplot(aes(gain, highpoint)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(vars(rating_grp))


tt_watraildf %>%
  select(rating, length_miles, gain, highpoint) %>%
  corrr::correlate() %>%
  corrr::rplot(print_cor = TRUE) 

tt_watraildf_dist <- tt_watraildf %>%
  distinct(name, .keep_all = TRUE)
  
wtmodel1 <- lm(rating ~ length_miles + gain + highpoint, data = tt_watraildf_dist)
summary(wtmodel1)

#wtmodel2 <- 
tt_watraildf %>%
  distinct(name, .keep_all = TRUE) %>%
  nest_by(location_region) %>%
  mutate(wtmodel1 = list(lm(rating ~ length_miles + gain + highpoint, data = data))) %>%
  pull(wtmodel1, name = location_region) %>%
  map_dfr(~broom::tidy(.x) %>%
            filter(term %in% c("length_miles", "gain", "highpoint")), .id = "location_region") %>%
 # mutate(estx100 = estimate * 100) %>%
  ggplot(aes(term, estimate)) + 
  ggalt::geom_lollipop() +
#  geom_text(aes(label = round(estx100, 2)), color = "white") +
  geom_text(aes(label = paste0("p = ", round(p.value, 3))), color = "black", size = 3,
            y = 0.01, vjust = 1.25) +
  labs(y = expression(beta), x = "Trail measurement") +
  coord_flip() +
  facet_wrap(vars(location_region))




## code from https://github.com/jack-davison/TidyTuesday/blob/master/R/2020_11_24_hiking.R 
# some elements incorporated in my code
data = tt_watrail$hike_data %>%
  mutate(across(gain:rating, as.numeric),
         length = parse_number(length)) %>%
  unnest(cols = c(features)) 

# %>%
#   filter(features != "Dogs not allowed")

inits = data.frame(name = (data %>% pull(features) %>% unique())) %>%
  mutate(init = case_when(name == "Dogs allowed on leash" ~ "DA",
                          name == "Dogs not allowed" ~ "DN",
                          name == "Wildlife" ~ "Wl",
                          name == "Good for kids" ~ "GK",
                          name == "Lakes" ~ "Lk",
                          name == "Fall foliage" ~ "FF",
                          name == "Ridges/passes" ~ "RP",
                          name == "Established campsites" ~ "EC",
                          name == "Mountain views" ~ "MV",
                          name == "Old growth" ~ "OG",
                          name == "Waterfalls" ~ "Wf",
                          name == "Wildflowers/Meadows" ~ "WM",
                          name == "Rivers" ~ "Ri",
                          name == "Coast" ~ "Co",
                          name == "Summits" ~ "Su"), 
         type = if_else(init %in% c("DA","DN","GK"), "Companion", "Feature"))

correlation = data %>% 
  widyr::pairwise_cor(item = features, feature = name) %>%
#  filter(abs(correlation) > 0.1) %>%
  as_tbl_graph() %>%
  activate(nodes) %>%
  left_join(inits) %>%
  arrange(name)

ggraph(correlation) +
  # geoms
  geom_edge_link(aes(color = correlation, width = abs(correlation))) +
  geom_node_point(aes(color = type), size = 10) +
  geom_node_text(aes(label = init), color = "#2a2a2aff") +
  # scales
  scale_edge_color_gradient2(high = "#8aab37ff", low = "#00557bff", name = "CORR.") +
  scale_edge_width(range = c(1,3), guide = F) +
  scale_color_manual(values = c("#8a8a8a","#bdbdbdff"), name = "CATEGORY") +
  scale_discrete_identity(aesthetics = "label",
                          name = "KEY",
                          breaks = activate(correlation, "nodes") %>% pull(init),
                          labels = paste0("—  ", activate(correlation, "nodes") %>% pull(name)),
                          guide = "legend") +
  # themes
  guides(edge_color = guide_edge_colorbar(direction = "horizontal")) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "#f3f3f3ff"),
        text = element_text(color = "#2a2a2aff", family = "Berlin Sans FB"), 
        legend.title = element_text(face = "bold", family = "Berlin Sans FB Demi"),
        plot.title = element_text(face = "bold", family = "Berlin Sans FB Demi", size = 30),
        plot.subtitle = element_text(family = "Berlin Sans FB"),
        plot.caption = element_text(hjust = 0, color = "#8a8a8a"),
        legend.box.background = element_rect(fill = "white", color = NA),
        legend.box.margin = margin(.5,.5,.5,.5,"cm")) +
  # labels
  labs(title = "HIKING IN WASHINGTON:\nWHAT YOU'LL SEE AND WHO YOU'LL SEE IT WITH",
       subtitle = "The Washington Trails Association helpfully provides a hiking guide written by local experts. 
       Each trail is flagged with different features one will encounter\n- rivers, mountains, waterfalls - 
       and the companions you can bring - children and/or dogs.\n\nThis graph explores the underlying relationship 
       between these two categories. It is seen that child-friendly routes are also commonly dog-friendly, and 
       that\nthe Trails Association discourages bringing children to mountainous routes with features like summits, 
       ridges and passes. Kids are more than welcome to\nenjoy wildlife and coastal routes, however, and your dog 
       will be happy to romp in the fall foliage!",
       caption = "Data from the Washington Trails Association (www.wta.org) | 
       Visualisation by Jack Davison (Twitter @JDavison_ | Github jack-davison)")
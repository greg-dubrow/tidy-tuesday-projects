library(tidytuesdayR)
library(tidyverse)
library(tidylog)
#library(tidygraph)
#library(ggraph)
library(gt)
library(reactable)
library(htmltools)
library(patchwork)
library(webshot)

library(wpp2019)

# read in object
tt_plastics <- tt_load("2021-01-26")

# audit report https://www.breakfreefromplastic.org/wp-content/uploads/2020/07/branded-2019.pdf
# On the occasion of World Clean Up Day on September 21, 2019, individuals and organizations around 
# the world mobilized their communities to conduct clean-ups and brand audits to hold corporations 
# accountable for the extensive use of single-use and throw-away plastic packaging in their products. 
# Thanks to our members and allies who led and organized efforts on the ground, Break Free From Plastic 
# engaged 72,541 volunteers in 51 countries to conduct 484 brand audits. These volunteers 
# collected 476,423 pieces of plastic waste, 43% of which was marked with a clear consumer brand.

tt_plastics_df <- as_tibble(tt_plastics$plastics)

glimpse(tt_plastics_df)

data(pop)
glimpse(pop)

data("UNlocations")



## top polluters by country

## gains/declines by company 2019 to 2020
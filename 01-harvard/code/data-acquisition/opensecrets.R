# load packages ----------------------------------------------------------------
library(tidyverse)
library(USAboundaries)
library(broom)
library(maptools)

# get NC congressional districts -----------------------------------------------
nc_congressional <- us_congressional(resolution = "low", states = "North Carolina")

# get NC candidate contributions -----------------------------------------------
nc_candidates <- read_csv("code/nc_candidates.csv")

nc_candidates <- nc_candidates %>%
  mutate(
    cd115fp = str_sub(race_link, start = nchar(race_link) - 1, end = nchar(race_link)),
    perc_spent = spent / raised
    )

# merge data -------------------------------------------------------------------
nc <- full_join(nc_congressional, nc_candidates)

# add up at district level for each type ---------------------------------------
district_boundaries <- nc_congressional %>%
  select(cd115fp, geometry)

# plot -------------------------------------------------------------------------
nc %>%
  ggplot() + 
    geom_sf(data = district_boundaries) +
    geom_sf(aes(fill = raised)) + 
    scale_fill_viridis_c(label = scales::dollar_format(scale = 1/1000000)) +
    facet_grid(party ~ status) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(fill = "Raised\n(in M)",
         title = "Political contributions for 2018 NC Congressional Races",
         subtitle = "as of 9/30/2018",
         caption = "Source: OpenSecrets.org")
  

# ------------------------------------------------------------------------------
# 03-scrape-pac-all.R: map scrape_pac() over all years
# ------------------------------------------------------------------------------

# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(here)

# list of urls -----------------------------------------------------------------

root <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs?cycle="
year <- seq(from = 1998, to = 2020, by = 2)
urls <- paste0(root, year)

# map --------------------------------------------------------------------------

pac_all <- map_dfr(urls, scrape_pac)

# write data -------------------------------------------------------------------

write_csv(pac_all, path = here::here("08-cerse/code/opensecrets/data/", "pac-all.csv"))

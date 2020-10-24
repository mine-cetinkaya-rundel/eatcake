# ------------------------------------------------------------------------------
# 01-scrape-pac-2020.R: scrape information for 2020 contributions
# ------------------------------------------------------------------------------

# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(here)

# define url -------------------------------------------------------------------

url_2020 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs?cycle=2020"

# read the page ----------------------------------------------------------------

page <- read_html(url_2020)

# exract the table -------------------------------------------------------------

pac_2020 <-  page %>%
  html_node(".DataTable") %>%
  html_table("td", header = TRUE, fill = TRUE) %>%
  as_tibble()

# rename variables -------------------------------------------------------------

pac_2020 <- pac_2020 %>%
  rename(
    name = `PAC Name (Affiliate)` ,
    country_parent = `Country of Origin/Parent Company`,
    total = Total,
    dems = Dems,
    repubs = Repubs
  )

# fix name ---------------------------------------------------------------------

pac_2020 <- pac_2020 %>%
  mutate(name = str_squish(name))

# write file -------------------------------------------------------------------

write_csv(pac_2020, here::here("09-rladies-ankara/code/opensecrets/data/", "pac-2020.csv"))

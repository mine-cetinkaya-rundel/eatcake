# ------------------------------------------------------------------------------
# 02-scrape-pac-function.R: function to scrape information for all contributions
# ------------------------------------------------------------------------------

# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(here)

# function: scrape_pac ---------------------------------------------------------

scrape_pac <- function(url) {
  
  # read the page
  page <- read_html(url)
  
  # exract the table
  pac <-  page %>%
    html_node(".DataTable") %>%
    html_table("td", header = TRUE, fill = TRUE) %>%
    as_tibble()
  
  # rename variables
  pac <- pac %>%
    rename(
      name = `PAC Name (Affiliate)` ,
      country_parent = `Country of Origin/Parent Company`,
      total = Total,
      dems = Dems,
      repubs = Repubs
    )
  
  # fix name
  pac <- pac %>%
    mutate(name = str_squish(name))
  
  # add year
  pac <- pac %>%
    mutate(year = str_sub(url, -4))
  
  # return data frame
  pac
  
}

# test function ----------------------------------------------------------------

url_2020 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs?cycle=2020"
pac_2020_fn <- scrape_pac(url_2020)

url_2018 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs?cycle=2018"
pac_2018 <- scrape_pac(url_2018)

url_1998 <- "https://www.opensecrets.org/political-action-committees-pacs/foreign-connected-pacs?cycle=1998"
pac_1998 <- scrape_pac(url_1998)

# write files -------------------------------------------------------------------

write_csv(pac_2020_fn, here::here("09-rladies-ankara/code/opensecrets/data/", "pac-2020-fn.csv"))
write_csv(pac_2018, here::here("09-rladies-ankara/code/opensecrets/data/", "pac-2018.csv"))
write_csv(pac_1998, here::here("09-rladies-ankara/code/opensecrets/data/", "pac-1998.csv"))

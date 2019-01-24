# load packages ----------------------------------------------------------------
library(tidyverse)
library(rvest)
library(robotstxt)
library(lubridate)

# scrape NC --------------------------------------------------------------------

state_page <- read_html("https://www.opensecrets.org/races/election?id=NC")

race <- state_page %>%
  html_nodes("strong a") %>%
  html_text()

race_link <- state_page %>%
  html_nodes("strong a") %>%
  html_attr("href") %>%
  paste0("https://www.opensecrets.org", .)

nc_races <- tibble(
  race = race,
  race_link = race_link
)

nc_races <- nc_races %>%
  mutate(
    race_type = ifelse(str_detect(race, "Senate"), "Senate", "House")
  )

# scrape district 1 race -------------------------------------------------------

race_page <- read_html(nc_races$race_link[1])

candidate_info <- race_page %>%
  html_nodes("td:nth-child(1)") %>%
  html_text() %>%
  str_trim()

raised <- race_page %>%
  html_nodes("td:nth-child(2)") %>%
  html_text() %>%
  str_trim() %>%
  str_remove("\\$") %>%
  str_remove_all("\\,") %>%
  as.numeric()

spent <- race_page %>%
  html_nodes("td:nth-child(3)") %>%
  html_text() %>%
  str_trim() %>%
  str_remove("\\$") %>%
  str_remove_all("\\,") %>%
  as.numeric()

cash_on_hand <- race_page %>%
  html_nodes("td:nth-child(4)") %>%
  html_text() %>%
  str_trim() %>%
  str_remove("\\$") %>%
  str_remove_all("\\,") %>%
  as.numeric()

last_report <- race_page %>%
  html_nodes("td:nth-child(5)") %>%
  html_text() %>%
  str_trim() %>%
  mdy()

race <- race_page %>%
  html_node(".Hero-title") %>%
  html_text() %>%
  str_remove(" 2018 Race")

candidates <- tibble(
  candidate_info = candidate_info,
  raised = raised,
  spent = spent, 
  cash_on_hand = cash_on_hand,
  last_report = last_report,
  race = race
)

candidates <- candidates %>%
  mutate(
    party = case_when(
      str_detect(candidate_info, "\\(R\\)") ~ "Republican",
      str_detect(candidate_info, "\\(D\\)") ~ "Democrat",
      TRUE                                  ~ "Third party"
    ),
    status = ifelse(str_detect(candidate_info, "Incumbent"), "Incumbent", "Challenger"),
    candidate_name = candidate_info %>% 
      str_remove_all("\\n") %>%
      str_remove("\\((.*)") %>%
      str_trim()
  ) %>%
  select(candidate_name, party, status, everything(), -candidate_info)

write_csv(candidates, path = here("static/hw/hw-06/data", "nc_candidates_dist01.csv"))

# scrape district 2 race -------------------------------------------------------

race_page <- read_html(nc_races$race_link[2])

candidate_info <- race_page %>%
  html_nodes("td:nth-child(1)") %>%
  html_text() %>%
  str_trim()

raised <- race_page %>%
  html_nodes("td:nth-child(2)") %>%
  html_text() %>%
  str_trim() %>%
  str_remove("\\$") %>%
  str_remove_all("\\,") %>%
  as.numeric()

spent <- race_page %>%
  html_nodes("td:nth-child(3)") %>%
  html_text() %>%
  str_trim() %>%
  str_remove("\\$") %>%
  str_remove_all("\\,") %>%
  as.numeric()

cash_on_hand <- race_page %>%
  html_nodes("td:nth-child(4)") %>%
  html_text() %>%
  str_trim() %>%
  str_remove("\\$") %>%
  str_remove_all("\\,") %>%
  as.numeric()

last_report <- race_page %>%
  html_nodes("td:nth-child(5)") %>%
  html_text() %>%
  str_trim() %>%
  mdy()

race <- race_page %>%
  html_node(".Hero-title") %>%
  html_text() %>%
  str_remove(" 2018 Race")

candidates <- tibble(
  candidate_info = candidate_info,
  raised = raised,
  spent = spent, 
  cash_on_hand = cash_on_hand,
  last_report = last_report,
  race = race
)

candidates <- candidates %>%
  mutate(
    party = case_when(
      str_detect(candidate_info, "\\(R\\)") ~ "Republican",
      str_detect(candidate_info, "\\(D\\)") ~ "Democrat",
      TRUE                                  ~ "Third party"
    ),
    status = ifelse(str_detect(candidate_info, "Incumbent"), "Incumbent", "Challenger"),
    candidate_name = candidate_info %>% 
      str_remove_all("\\n") %>%
      str_remove("\\((.*)") %>%
      str_trim()
  ) %>%
  select(candidate_name, race, everything(), -candidate_info)

write_csv(candidates, path = here("static/hw/hw-06/data", "nc_candidates_dist02.csv"))

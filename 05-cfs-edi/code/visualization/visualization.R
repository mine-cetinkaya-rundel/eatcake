# Load packages ----------------------------------------------------------------
library(tidyverse)
library(unvotes)
library(lubridate)

# Rename countries -------------------------------------------------------------

un_votes <- un_votes %>% 
  mutate(country =
           case_when(
             country == "United Kingdom of Great Britain and Northern Ireland" ~ "UK & NI",
             country == "United States of America"                             ~ "US",
             TRUE                                                              ~ country
             )
  )

# Make a plot ------------------------------------------------------------------
un_votes %>%
  filter(country %in% c("UK + NI", "US", "Turkey")) %>%
  inner_join(un_roll_calls, by = "rcid") %>%
  inner_join(un_roll_call_issues, by = "rcid") %>%
  group_by(country, year = year(date), issue) %>%
  summarize(
    votes = n(),
    percent_yes = mean(vote == "yes")
  ) %>%
  filter(votes > 5) %>%  # only use records where there are more than 5 votes
  ggplot(mapping = aes(x = year, y = percent_yes, color = country)) +
  #geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ issue) +
  labs(
    title = "Percentage of Yes votes in the UN General Assembly",
    subtitle = "1946 to 2015",
    y = "% Yes",
    x = "Year",
    color = "Country"
  )

# Save plot as png -------------------------------------------------------------
ggsave(filename = here("05-cfs-edi/code/visualization/unvotes-turkey.png"),
       height = 4, width = 7)

# Reprex for keynote -----------------------------------------------------------

options(
  reprex.highlight.hl_style  = "solarized-light",
  reprex.highlight.font      = "Fira Code Regular",
  reprex.highlight.font_size = 35,
  reprex.highlight.other     = ""
)

# Make another plot ------------------------------------------------------------------
un_votes %>%
  filter(country %in% c("UK + NI", "US", "France")) %>%
  inner_join(un_roll_calls, by = "rcid") %>%
  inner_join(un_roll_call_issues, by = "rcid") %>%
  group_by(country, year = year(date), issue) %>%
  summarize(
    votes = n(),
    percent_yes = mean(vote == "yes")
  ) %>%
  filter(votes > 5) %>%  # only use records where there are more than 5 votes
  ggplot(mapping = aes(x = year, y = percent_yes, color = country)) +
  #geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ issue) +
  labs(
    title = "Percentage of Yes votes in the UN General Assembly",
    subtitle = "1946 to 2015",
    y = "% Yes",
    x = "Year",
    color = "Country"
  )

# Save plot as png -------------------------------------------------------------
ggsave(filename = here("05-cfs-edi/code/visualization/unvotes-france.png"),
       height = 4, width = 7)

# Step by step -----------------------------------------------------------------

un_votes_joined <- un_votes %>%
  filter(country %in% c("UK + NI", "US", "Turkey")) %>%
  inner_join(un_roll_calls, by = "rcid") %>%
  inner_join(un_roll_call_issues, by = "rcid") %>%
  group_by(country, year = year(date), issue) %>%
  summarize(
    votes = n(),
    percent_yes = mean(vote == "yes")
  ) %>%
  filter(votes > 5) 

## step 1 ----

ggplot(data = un_votes_joined)

ggsave(filename = here("05-cfs-edi/code/visualization/unvotes-step1.png"),
       height = 4, width = 7)

## step 2 ----

ggplot(data = un_votes_joined, 
       mapping = aes(x = year, y = percent_yes))

ggsave(filename = here("05-cfs-edi/code/visualization/unvotes-step2.png"),
       height = 4, width = 7)

## step 3 ----

ggplot(data = un_votes_joined, 
       mapping = aes(x = year, y = percent_yes)) + 
  geom_point()

ggsave(filename = here("05-cfs-edi/code/visualization/unvotes-step3.png"),
       height = 4, width = 7)

## step 4 ----

ggplot(data = un_votes_joined, 
       mapping = aes(x = year, y = percent_yes, color = country)) + 
  geom_point()

ggsave(filename = here("05-cfs-edi/code/visualization/unvotes-step4.png"),
       height = 4, width = 7)

## step 5 ----

ggplot(data = un_votes_joined, 
       mapping = aes(x = year, y = percent_yes, color = country)) + 
  geom_smooth(method = "loess", se = FALSE)

ggsave(filename = here("05-cfs-edi/code/visualization/unvotes-step5.png"),
       height = 4, width = 7)

## step 6 ----

ggplot(data = un_votes_joined, 
       mapping = aes(x = year, y = percent_yes, color = country)) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ issue)

ggsave(filename = here("05-cfs-edi/code/visualization/unvotes-step6.png"),
       height = 4, width = 7)

## step 7 ----

ggplot(data = un_votes_joined, 
       mapping = aes(x = year, y = percent_yes, color = country)) + 
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ issue) +
  labs(
    title = "Percentage of Yes votes in the UN General Assembly",
    subtitle = "1946 to 2015",
    y = "% Yes",
    x = "Year",
    color = "Country"
  )

ggsave(filename = here("05-cfs-edi/code/visualization/unvotes-step7.png"),
       height = 4, width = 7)

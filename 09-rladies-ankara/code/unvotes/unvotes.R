# Load packages ----------------------------------------------------------------

library(tidyverse)
library(unvotes)
library(lubridate)

# Make a plot - Türkiye --------------------------------------------------------

un_votes %>%
  filter(country %in% c("United States of America", "Turkey")) %>%
  inner_join(un_roll_calls, by = "rcid") %>%
  inner_join(un_roll_call_issues, by = "rcid") %>%
  group_by(country, year = year(date), issue) %>%
  summarize(
    votes = n(),
    percent_yes = mean(vote == "yes")
  ) %>%
  filter(votes > 5) %>% 
  ungroup() %>%
  mutate(
    issue = case_when(
      issue == "Human rights"                         ~ "İnsan hakları",
      issue == "Economic development"                 ~ "Ekonomik gelişme",
      issue == "Colonialism"                          ~ "Sömürgecilik",
      issue == "Palestinian conflict"                 ~ "Filistin çatışması",
      issue == "Nuclear weapons and nuclear material" ~ "Nükleer silahlar ve malzemeler",
      issue == "Arms control and disarmament"         ~ "Silah kontrolü ve silahsızlanma"
      ),
    country = case_when(
      country == "United States of America" ~ "ABD",
      country == "Turkey" ~ "Türkiye"
    )
  ) %>%
  ggplot(mapping = aes(x = year, y = percent_yes, color = country)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ issue, labeller = label_wrap_gen(20)) +
  labs(
    title = "BM Genel Kurulu'nda 'Evet' oylarının yüzdesi",
    subtitle = "1946 - 2015",
    y = "'Evet' yüzdesi",
    x = "Yıl",
    color = "Ülke"
  )

ggsave(filename = "09-rladies-ankara/code/unvotes/unvotes-turkiye.png", 
       width = 8, height = 8*0.618)

# Make a plot - Fransa ---------------------------------------------------------

un_votes %>%
  filter(country %in% c("United States of America", "France")) %>%
  inner_join(un_roll_calls, by = "rcid") %>%
  inner_join(un_roll_call_issues, by = "rcid") %>%
  group_by(country, year = year(date), issue) %>%
  summarize(
    votes = n(),
    percent_yes = mean(vote == "yes")
  ) %>%
  filter(votes > 5) %>% 
  ungroup() %>%
  mutate(
    issue = case_when(
      issue == "Human rights"                         ~ "İnsan hakları",
      issue == "Economic development"                 ~ "Ekonomik gelişme",
      issue == "Colonialism"                          ~ "Sömürgecilik",
      issue == "Palestinian conflict"                 ~ "Filistin çatışması",
      issue == "Nuclear weapons and nuclear material" ~ "Nükleer silahlar ve malzemeler",
      issue == "Arms control and disarmament"         ~ "Silah kontrolü ve silahsızlanma"
    ),
    country = case_when(
      country == "United States of America" ~ "ABD",
      country == "France" ~ "Fransa"
    )
  ) %>%
  ggplot(mapping = aes(x = year, y = percent_yes, color = country)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ issue, labeller = label_wrap_gen(20)) +
  labs(
    title = "BM Genel Kurulu'nda 'Evet' oylarının yüzdesi",
    subtitle = "1946 - 2015",
    y = "'Evet' yüzdesi",
    x = "Yıl",
    color = "Ülke"
  )

ggsave(filename = "09-rladies-ankara/code/unvotes/unvotes-fransa.png", 
       width = 8, height = 8*0.618)

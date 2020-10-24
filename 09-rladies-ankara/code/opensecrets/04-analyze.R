# ------------------------------------------------------------------------------
# 04-analyze.R: clean and visualize
# ------------------------------------------------------------------------------

# load packages ----------------------------------------------------------------

library(tidyverse)
library(scales)

# load data --------------------------------------------------------------------

pac_all <- read_csv(here::here("09-rladies-ankara/code/opensecrets/data/", "pac-all.csv"))

# ------------------------------------------------------------------------------
# data cleaning
# ------------------------------------------------------------------------------

# fix country_parent -----------------------------------------------------------

pac_all <- pac_all %>%
  separate(country_parent, into = c("country", "parent"), sep = "/", extra = "merge")

# fix dollar amounts -----------------------------------------------------------

parse_currency <- function(x){
  x %>% 
    str_remove("\\$") %>%
    str_remove_all(",") %>%
    as.numeric()
}

pac_all <- pac_all %>%
  mutate(
    total = parse_currency(total),
    dems = parse_currency(dems),
    repubs = parse_currency(repubs)
  )

# write data -------------------------------------------------------------------

write_csv(pac_all, path = here::here("opensecrets/data/", "pac-all-clean.csv"))

# ------------------------------------------------------------------------------
# data visualization
# ------------------------------------------------------------------------------

# UK and Canada contributions --------------------------------------------------

pac_all %>%
  filter(country %in% c("Canada", "UK")) %>%
  group_by(country, year) %>%
  summarise(tot = sum(total)) %>%
  ggplot(aes(x = year, y = tot, group = country, color = country)) +
  geom_line()

# UK contributions to democrats and republicans --------------------------------

pac_all %>%
  filter(country %in% c("UK", "Germany")) %>%
  group_by(country, year) %>%
  summarise(
    Democrat = sum(dems),
    Republican = sum(repubs)
  ) %>%
  pivot_longer(cols = c(Democrat, Republican), names_to = "party", values_to = "amount") %>%
  mutate(party = if_else(party == "Democrat", "Demokrat", "Cumhuriyetçi")) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = amount, group = party, color = party)) +
  scale_color_manual(values = c("red", "blue")) +
  scale_y_continuous(labels = dollar_format(scale = 0.000001, suffix = "M")) +
  facet_wrap(~country) +
  labs(
    x = "Yıl",
    y = "Miktar",
    color = "Parti",
    title = "Yabancı Bağlantılı PAC'lerden ABD siyasetine katkı",
    subtitle = "İngiltere ve Almanya bağlantılı Siyasi Eylem Komiteleri",
    caption = "Source: OpenSecrets.org"
  ) +
  theme_minimal()

ggsave(filename = here::here("09-rladies-ankara/code/opensecrets/", "pac.png"), width = 7, height = 6*0.618)

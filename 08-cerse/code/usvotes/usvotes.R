library(unvotes)
library(tidyverse)

us_votes <- un_votes %>%
  filter(country %in% c("United States of America")) %>%
  inner_join(un_roll_calls, by = "rcid") %>%
  inner_join(un_roll_call_issues, by = "rcid") %>%
  mutate(
    importantvote = ifelse(importantvote == 0, "No", "Yes"),
    issue = ifelse(issue == "Nuclear weapons and nuclear material", "Nuclear weapons and materials", issue)
  )

ggplot(us_votes)
ggsave(filename = "08-cerse/code/usvotes/usvotes1.png", width = 4, height = 7)

ggplot(us_votes, aes(y = importantvote, fill = vote))
ggsave(filename = "08-cerse/code/usvotes/usvotes2.png", width = 5, height = 7, dpi = 300)

ggplot(us_votes, aes(y = importantvote, fill = vote)) +
  geom_bar(position = "fill")
ggsave(filename = "08-cerse/code/usvotes/usvotes3.png", width = 5, height = 7, dpi = 300)

ggplot(us_votes, aes(y = importantvote, fill = vote)) +
  geom_bar(position = "fill") +
  facet_wrap(~ issue, ncol = 1)
ggsave(filename = "08-cerse/code/usvotes/usvotes4.png", width = 5, height = 7, dpi = 300)

ggplot(us_votes, aes(y = importantvote, fill = vote)) +
  geom_bar(position = "fill") +
  facet_wrap(~ issue, ncol = 1) +
  labs(
    title = "How the US voted in the UN", 
    subtitle = "By issue and importance of vote",
    x = "Important vote", y = "", fill = "Vote"
  )
ggsave(filename = "08-cerse/code/usvotes/usvotes5.png", width = 5, height = 7, dpi = 300)

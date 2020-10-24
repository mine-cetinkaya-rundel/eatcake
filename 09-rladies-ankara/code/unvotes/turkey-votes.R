# Load packages ----------------------------------------------------------------

library(unvotes)
library(tidyverse)
library(scales)

# Değişiklikler ----------------------------------------------------------------

un_votes %>%
  filter(country %in% c("Turkey")) %>%
  inner_join(un_roll_calls, by = "rcid") %>%
  inner_join(un_roll_call_issues, by = "rcid") %>%
  ggplot(aes(x = amend)) +
  geom_bar() +
  labs(
    title = "Türkiye'nin karar değişiklikleri üzerindeki oylari",
    x = "0 - Hayir, 1 - Evet",
    y = "Sıklık"
  )

ggsave(filename = "09-rladies-ankara/code/unvotes/turkiye-degisiklik.png", 
       width = 8, height = 8*0.618)

# Turkey votes ---------------------------------------------------------------------

turkey_votes <- un_votes %>%
  filter(country %in% c("Turkey")) %>%
  inner_join(un_roll_calls, by = "rcid") %>%
  inner_join(un_roll_call_issues, by = "rcid") %>%
  mutate(
    importantvote = ifelse(importantvote == 0, "Hayır", "Evet"),
    issue = case_when(
      issue == "Human rights"                         ~ "İnsan hakları",
      issue == "Economic development"                 ~ "Ekonomik gelişme",
      issue == "Colonialism"                          ~ "Sömürgecilik",
      issue == "Palestinian conflict"                 ~ "Filistin çatışması",
      issue == "Nuclear weapons and nuclear material" ~ "Nükleer silahlar ve malzemeler",
      issue == "Arms control and disarmament"         ~ "Silah kontrolü ve silahsızlanma"
    ),
    vote = case_when(
      vote == "yes" ~ "Evet",
      vote == "abstain" ~ "Çekimser",
      vote == "no" ~ "Hayır"
    ),
    vote = fct_relevel(vote, "Evet", "Çekimser", "Hayır")
  )

ggplot(turkey_votes)
ggsave(filename = "09-rladies-ankara/code/unvotes/turkey-votes-1.png", width = 5, height = 7)

ggplot(turkey_votes, 
       aes(y = importantvote, fill = vote))
ggsave(filename = "09-rladies-ankara/code/unvotes/turkey-votes-2.png", width = 5, height = 7)

ggplot(turkey_votes, 
       aes(y = importantvote, fill = vote)) +
  geom_bar(position = "fill")
ggsave(filename = "09-rladies-ankara/code/unvotes/turkey-votes-3.png", width = 5, height = 7)

ggplot(turkey_votes, aes(y = importantvote, fill = vote)) +
  geom_bar(position = "fill") +
  facet_wrap(~ issue, ncol = 1)
ggsave(filename = "09-rladies-ankara/code/unvotes/turkey-votes-4.png", width = 5, height = 7)

ggplot(turkey_votes, aes(y = importantvote, fill = vote)) +
  geom_bar(position = "fill") +
  facet_wrap(~ issue, ncol = 1) +
  scale_fill_viridis_d(option = "E")
ggsave(filename = "09-rladies-ankara/code/unvotes/turkey-votes-5.png", width = 5, height = 7)

ggplot(turkey_votes, aes(y = importantvote, fill = vote)) +
  geom_bar(position = "fill") +
  facet_wrap(~ issue, ncol = 1) +
  scale_fill_viridis_d(option = "E") +
  scale_x_continuous(labels = label_percent())
ggsave(filename = "09-rladies-ankara/code/unvotes/turkey-votes-6.png", width = 5, height = 7)

ggplot(turkey_votes, aes(y = importantvote, fill = vote)) +
  geom_bar(position = "fill") +
  facet_wrap(~ issue, ncol = 1) +
  scale_fill_viridis_d(option = "E") +
  scale_x_continuous(labels = label_percent()) +
labs(
    title = "Türkiye BM'de nasıl oy kullandı?", 
    subtitle = "Oyun konusu ve önemi ile",
    y = "Önemli oy", x = NULL, fill = "Oy"
  )
ggsave(filename = "09-rladies-ankara/code/unvotes/turkey-votes-7.png", width = 5, height = 7)

ggsave(filename = "09-rladies-ankara/code/unvotes/turkey-votes-7-genis.png", width = 6, height = 7)

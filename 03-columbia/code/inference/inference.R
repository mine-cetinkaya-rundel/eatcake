# load packages ----------------------------------------------------------------
library(tidyverse)
library(infer)

# load data --------------------------------------------------------------------
evals <- read_csv("02-rice/code/inference/evals.csv")

# diff in scores bet genders ---------------------------------------------------

set.seed(12345)

d <- evals %>%
  specify(score ~ gender) %>%
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("male", "female"))

d_bounds <- d %>%
  summarise(l = quantile(stat, 0.025), u = quantile(stat, 0.975))

d_viz <- d %>%
  visualize(method = "simulation") +
  shade_confidence_interval(c(d_bounds$l, d_bounds$u), fill = "#496B8830", color = "#496B88") +
  labs(title = "", y = "") 

ggsave("score_gender_ci.png", d_viz, width = 5, height = 3)

# create below/above bty_avg ---------------------------------------------------
# note used, save for later

evals <- evals %>%
  mutate(bty_avg_mean = ifelse(bty_avg < mean(bty_avg), "below average", "at or above average"))

evals %>%
  specify(score ~ bty_avg_mean) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("at or above average", "below average")) %>%
  summarise(l = quantile(stat, 0.025), u = quantile(stat, 0.975))

evals %>%
  specify(score ~ bty_avg_mean) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("at or above average", "below average")) %>%
  visualize(method = "simulation") +
    shade_confidence_interval(c(0.0648, 0.256), fill = "#496B8830", color = "#496B88")

# load packages ----------------------------------------------------------------
library(tidyverse)
library(broom)

# load data --------------------------------------------------------------------
evals <- read_csv("code/modeling/evals.csv")

# build model ------------------------------------------------------------------
evals <- evals %>%
  select(score, rank, ethnicity, gender, bty_avg)

lm(score ~ rank + ethnicity + gender*bty_avg, data = evals) %>%
  tidy()

lm(score ~ rank + ethnicity + gender + bty_avg, data = evals) %>%
  tidy()

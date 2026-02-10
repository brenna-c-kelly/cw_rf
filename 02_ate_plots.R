
# ATE plots

library(tidyr)
library(ggplot2)

ate <- read.csv("/Users/brenna/Downloads/individual_ates_v11_37w_mainmodel.csv")

head(ate)

ate_l <- ate |>
  pivot_longer(cols = 1:36, names_to = "week") |>
  mutate(week = as.numeric(gsub("X", "", week)) + 1) |>
  rename(ate = value)

ggplot(ate_l, aes(x = ate)) +
  geom_histogram(position = "identity", bins = 20, fill = "black") +
  facet_wrap(~week) +
  theme_minimal() +
  ggtitle("Individual treatment effects across gestational weeks")

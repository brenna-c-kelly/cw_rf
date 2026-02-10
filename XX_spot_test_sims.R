
# spot testing cw sim results

sim <- read.csv("/Users/brenna/Downloads/sim482_res_moderate_smooth.csv")
sim <- read.csv("/Users/brenna/Downloads/sim482_res_narrow_smooth.csv")
sim <- read.csv("/Users/brenna/Downloads/sim482_res_wide_smooth.csv")

sim <- read.csv("/Users/brenna/Downloads/sim969_res_intx.csv")

# after 200 trees
sim <- read.csv("/Users/brenna/Downloads/sim557_res_wide_smooth.csv")
sim <- read.csv("/Users/brenna/Downloads/sim632_res_wide_smooth.csv")

true_effects <- read.csv("/Users/brenna/Documents/School/Research/causal_rf/cw_rf/true_cw_fx_-15.csv")

sim |> mutate(week = as.factor(gsub("tx_", "", treatment))) |> 
  ggplot(aes(y = week, colour = cw)) + 
  scale_color_manual(values=c("red", "black")) +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper)) +
  geom_point(aes(x = mean), shape = 15, size = 1) +
  geom_vline(xintercept = 0, linetype="dashed") +
  coord_flip() + 
  theme_classic()




true_fx <- mutate(week = 1:37) |>
  melt(id.vars = "week") |>
  mutate(cw_type = case_when(grepl("uni_narrow", variable) == TRUE ~ "narrow naive",
                             grepl("norm_narrow", variable) == TRUE ~ "narrow smooth",
                             grepl("uni_moderate", variable) == TRUE ~ "moderate naive",
                             grepl("norm_moderate", variable) == TRUE ~ "moderate smooth",
                             grepl("uni_wide", variable) == TRUE ~ "wide naive",
                             grepl("norm_wide", variable) == TRUE ~ "wide smooth"),
         ci_lower = NA, ci_upper = NA, cw = NA) |>
  rename(mean = value)

true_sim = sim_res |>
  mutate(cw_type = paste(cw_size, cw_timedep),
         week = as.factor(as.numeric(gsub("tx_", "", treatment))),
         true_pred = "pred") |>
  select(c(week, week, mean, cw_type, ci_lower, ci_upper, cw, true_pred)) |>
  rbind(true_fx |>
          select(c(week, week, mean, cw_type, ci_lower, ci_upper, cw)) |>
          mutate(true_pred = "true"))

# both true values and simulation results
true_sim |>
  mutate(cw_type = fct_relevel(cw_type, 
                               "wide smooth", "wide naive", 
                               "moderate naive", "moderate smooth",
                               "narrow naive", "narrow smooth")) |>
  ggplot(aes(y = week, color = cw, group = cw_type)) +
  scale_color_manual(values=c("#ff4b00", "black")) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_path(data = ~filter(.x, true_pred == "true"), 
            aes(y = week, x = mean, group = cw_type), lwd = 1, color = "gray80") +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), alpha = 0.35) +
  geom_point(aes(x = mean), shape = 15, size = 1, alpha = 0.1) +
  # geom_point(data = ~filter(.x, true_pred == "true"), 
  #            aes(y = week, x = mean, group = cw_type), color = "gray80") +
  labs(x = "Predicted Change in Birthweight Percentile (by sex, gestational age)", 
       y = "Week", title = "Simulation results (true effects in gray; critical windows in red)",
       subtitle = "~900 sims (5K samples each), e~N(0, 1)") +
  coord_flip() +
  guides(color = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~cw_type, nrow = 3)




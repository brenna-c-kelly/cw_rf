
library(purrr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(forcats)
library(Metrics)
library(data.table)

sim_files <- list()

for(i in 1:length(list.files("data/more_sims_results/"))) {
  
  sim_files[[i]] <- 
    fread(paste0(
      "data/more_sims_results/",
      list.files("data/more_sims_results/")[i]))
  
}

sim_res <- map_df(sim_files, ~as.data.frame(.))

# sim_by_cw <- sim_res |>
#   mutate(type = paste0(sigma, sample_size, sep = "_"))
# sim_by_cw <- split(sim_by_cw, sim_by_cw$type)


sim_res$type <- paste0(sim_res$sigma, "_", sim_res$sample_size)
sim_res$ci_width <- mean(sim_res$ci_lower - sim_res$ci_upper)

table(sim_res$type)

test <- sim_res |>
  filter(type == "1_5000")
prop.table(table(test$critical_sig_recovered))


library(stringr)

sim_res |>
  mutate(type = paste0(sigma, "_", str_pad(sample_size, pad = "0", width = 5, side = "left")),
         week = as.factor(as.numeric(gsub("tx_", "", treatment)))) |>
  ggplot(aes(y = week, color = cw)) +
  scale_color_manual(values=c("red", "black")) +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), alpha = 0.35) + #0.35
  geom_jitter(aes(x = mean), shape = 15, size = 1, alpha = 0.1) + #0.1
  labs(x = "Predicted Change in Birthweight (g)", 
       y = "Week", title = "Simulation results",
       subtitle = "100 sims (5K samples each), total fx = 166, e~N(0, 10)") +
  theme(axis.text.x = element_text(angle = 45)) +
  guides(color = FALSE) +
  geom_vline(xintercept = 0, linetype="dashed") +
  coord_flip() +
  theme_classic() +
  facet_wrap(~type, nrow = 4)

# the only scenario which doesn't recover the critical window effect as significant is 1000 + 5
sim_res[which(sim_res$critical_sig_recovered == "FALSE"), ]





####### may plot with true effects for supplement, for manuscript
## plot with the true effects
true_fx <- read.csv("data/true_cw_fx_-166.csv")

true_fx <- read.csv("data/true_cw_fx_-166.csv") |>
  mutate(week = 1:20) |>
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
  labs(x = "Predicted Change in Birthweight (g)", 
       y = "Week", title = "Simulation results (true effects in gray; critical windows in red)",
       subtitle = "100 sims per CW type (5K samples each), total fx = -166, e~N(0, 10)") +
  coord_flip() +
  guides(color = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~cw_type, nrow = 3)

# p values and effect size
true_fx_sig <- ggplot(sim_res, aes(x = c, y = p_value)) +
  geom_point(size = 2, alpha = 0.15) +
  geom_hline(yintercept = 0.05, linetype="dashed") +
  scale_x_continuous(breaks = seq(-150, 25, by = 25), limits = c(-135, 5)) +
  labs(x = "True coefficient for change in birthweight (g)", 
       y = "p-value obtained by causal RF", title = "True effect size and significance") +
  theme_classic()

pred_fx_sig <- ggplot(sim_res, aes(x = mean, y = p_value)) +
  geom_point(size = 2, alpha = 0.15) +
  geom_hline(yintercept = 0.05, linetype="dashed") +
  scale_x_continuous(breaks = seq(-150, 25, by = 25), limits = c(-135, 5)) +
  labs(x = "Predicted coefficient for change in birthweight (g)", 
       y = "p-value obtained by causal RF", title = "Predicted effect size and significance") +
  theme_classic()

ggarrange(true_fx_sig, pred_fx_sig)

# p-values by cw type; double-checking wide CWs
cw_res_df |>
  ggplot(aes(y = cw_type, x = critical_pval_mean, group = cw_type)) +
  geom_vline(xintercept = c(0, 0.05, 0.025, 0.075, 0.1, 0.125), 
             color = "gray80", linetype = "dashed") +
  geom_linerange(aes(xmin = critical_pval_min, xmax = critical_pval_max)) +
  geom_point(aes(x = critical_pval_mean), shape = 15, size = 1) +
  labs(x = "p-value", 
       y = "CW type", title = "p-values by CW type (mean, min, max)") +
  guides(color = FALSE) +
  theme_classic()

names(sim_by_cw)
names(sim_by_cw[[5]])
test <- sim_by_cw[[5]]
prop.table(table(test[which(test$cw == "critical"), "p_value"] < 0.05))
test <- sim_by_cw[[6]]
prop.table(table(test[which(test$cw == "critical"), "p_value"] < 0.05))



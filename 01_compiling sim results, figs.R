
library(purrr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(forcats)
library(Metrics)
library(data.table)

sim_files <- list()

for(i in 1:length(list.files("data/00_sim_results/", recursive = TRUE, include.dirs = TRUE, pattern = ".csv"))) {
  
  sim_files[[i]] <- 
    fread(paste0("data/00_sim_results/",
                 list.files("data/00_sim_results/", recursive = TRUE, include.dirs = TRUE, pattern = ".csv")[i]))
  
}

sim_res <- map_df(sim_files, ~as.data.frame(.))

sim_res <- sim_res |>
  mutate(cw_type = paste(cw_size, cw_timedep))
sim_by_cw <- split(sim_res, sim_by_cw$cw_type)

# mean(sim_by_cw[[1]]$ci_lower - sim_by_cw[[1]]$ci_upper)
# prop.table(table(sim_by_cw[[5]][which(sim_by_cw[[5]]$cw == "critical"), 
#                          "p_value"] < 0.05))[["TRUE"]]
# prop.table(table(sim_by_cw[[5]]$effect_recovered))
# 
# R = cor.test(sim_by_cw[[5]]$true_effect, sim_by_cw[[5]]$mean)$estimate[["cor"]]
# var_r = var(sim_by_cw[[5]]$mean)
# var_f = var(sim_by_cw[[5]]$true_effect)
# 
# var_f + var_r - 2*var_f*var_r*R
# rmse(sim_by_cw[[5]]$true_effect, sim_by_cw[[5]]$mean)
# 
# ## centering
# # subtract mean of y from y
# c_y = sim_by_cw[[5]]$true_effect - mean(sim_by_cw[[5]]$true_effect)
# # subtract mean of y_hat from y_hat
# c_yhat = sim_by_cw[[5]]$mean - mean(sim_by_cw[[5]]$mean)
# ## centered RMSE for pattern accuracy
# c_rmse = rmse(c_y, c_yhat)


## practice with moderate smooth

cw_results_fx <- function(x) {
  
  cw_type = names(x[1])
  cw_data = x[[1]]
  
  ## centering
  # subtract mean of y from y
  c_y = cw_data$true_effect - mean(cw_data$true_effect)
  # subtract mean of y_hat from y_hat
  c_yhat = cw_data$mean - mean(cw_data$mean)
  ## centered RMSE for pattern accuracy
  c_rmse = rmse(c_y, c_yhat)
  
  ## other variables
  data.frame("cw_type" = cw_type,
             "empirical_coverage" = prop.table(table(cw_data$effect_recovered))[["TRUE"]],
             "cw_empirical_coverage" = prop.table(table(cw_data[which(cw_data$cw == "critical"), 
                                                             "effect_recovered"]))[["TRUE"]],
             "centered_rmse" = c_rmse,
             # "trend_diff_mean" = summary(cw_data$trend_recovered_diff)[["Mean"]],
             # "critical_trend_diff_mean" = summary(cw_data[which(cw_data$cw == "critical"), 
             #                                              "trend_recovered_diff"])[["Mean"]],
             "ci_width_average" = mean(cw_data$ci_upper - cw_data$ci_lower),
             "true_peak_recovered" = ifelse(grepl("naive", cw_type) == TRUE, NA,
                                            prop.table(table(cw_data[which(cw_data$inflection_true == "peak"), 
                                                                     "inflection_recovered"]))[["TRUE"]]),
             "critical_effect_sig" = prop.table(table(cw_data[which(cw_data$cw == "critical"), 
                                                              "p_value"] < 0.05))[["TRUE"]],
             "critical_effect_sig" = prop.table(table(cw_data[which(cw_data$cw == "critical"), 
                                                              "p_value"] < 0.05))[["TRUE"]],
             "critical_pval_mean" = summary(cw_data[which(cw_data$cw == "critical"), "p_value"])[["Mean"]],
             "critical_pval_min" = summary(cw_data[which(cw_data$cw == "critical"), "p_value"])[["Min."]],
             "critical_pval_max" = summary(cw_data[which(cw_data$cw == "critical"), "p_value"])[["Max."]]
  )
}

cw_results = list()

for(i in 1:length(sim_by_cw)) {
  
  cw_results[[i]] = cw_results_fx(sim_by_cw[i])
  
}

cw_res_df <- map_df(cw_results, ~as.data.frame(.))
cw_res_df

cw_res_df_tidy <- cw_res_df |>
  mutate(cw_type = fct_relevel(cw_type, 
                               "wide naive", "wide smooth", 
                               "moderate naive", "moderate smooth",
                               "narrow naive", "narrow smooth")) |>
  arrange(cw_type) |>
  mutate(centered_rmse = round(centered_rmse, 3),
         ci_width_average = round(ci_width_average, 3),
         critical_effect_sig = round(critical_effect_sig, 3),
         critical_pval_mean = round(critical_pval_mean, 3),
         critical_pval_min = round(critical_pval_min, 3),
         critical_pval_max = round(critical_pval_max, 3)) |>
  select(!critical_effect_sig.1)

cw_res_df_tidy

write.csv(cw_res_df_tidy, "data/results/cw_res_df_tidy.csv", row.names = FALSE)

# is the RMSE lower than the SD in the outcome?
birth <- read.csv("data/birth_w_percentile_confounders_term_v2.csv")
sd(birth$bw_percentile)

head(sim_res)

sim_res$cw_type <- paste(sim_res$cw_size, sim_res$cw_timedep)

# sim_res |>
#   mutate(week = as.factor(as.numeric(gsub("tx_", "", treatment)))) |>
#   ggplot(aes(y = week, color = cw)) +
#   scale_color_manual(values=c("orangered", "black")) +
#   geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), alpha = 0.35) +
#   geom_jitter(aes(x = mean), shape = 15, size = 1, alpha = 0.1) +
#   labs(x = "Predicted Change in Birthweight Percentile (by sex, gestational age)", 
#        y = "Week", title = "Simulation results",
#        subtitle = "~900 sims (5K samples each), e~N(0, 1)") +
#   theme(axis.text.x = element_text(angle = 45)) +
#   guides(color = FALSE) +
#   geom_vline(xintercept = 0, linetype="dashed") +
#   coord_flip() +
#   theme_classic() +
#   # geom_text(aes(x = (ci95.hi + 0.5), y=variable, label=variable), hjust=0, fontface = "bold") +
#   # geom_text(aes(x = -1, y = variable, label=round(estimate, 2)), hjust=0, fontface = "plain") +
#   facet_wrap(~cw_type, nrow = 3)


## plot with the true effects
# true_fx <- read.csv("simulation output/true_cw_fx_-15.csv")

true_fx <- read.csv("data/true_cw_fx_-15_v2.csv") |>
  mutate(week = 1:36) |>
  melt(id.vars = "week") |>
  mutate(cw_type = case_when(grepl("uni_narrow", variable) == TRUE ~ "narrow naive",
                             grepl("norm_narrow", variable) == TRUE ~ "narrow smooth",
                             grepl("uni_moderate", variable) == TRUE ~ "moderate naive",
                             grepl("norm_moderate", variable) == TRUE ~ "moderate smooth",
                             grepl("uni_wide", variable) == TRUE ~ "wide naive",
                             grepl("norm_wide", variable) == TRUE ~ "wide smooth"),
         ci_lower = NA, ci_upper = NA, cw = NA) |>
  rename(mean = value)

sim_by_cw <- sim_res |>
  mutate(cw_type = paste(cw_size, cw_timedep))

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
  labs(x = "Predicted Change in Birthweight Percentile", 
       y = "Week", title = "Simulation results by critical window structure") +
  coord_flip() +
  guides(color = FALSE) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90)) +
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



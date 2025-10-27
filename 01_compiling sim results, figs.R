

library(purrr)
library(dplyr)
library(ggplot2)
library(forcats)
library(data.table)

sim_files <- list()

for(i in 1:length(list.files("data/sim_results/"))) {
  
  sim_files[[i]] <- 
    fread(paste0(
    "data/sim_results/",
    list.files("data/sim_results/")[i]))
  
}

sim_res <- map_df(sim_files, ~as.data.frame(.))

sim_by_cw <- sim_res |>
  mutate(cw_type = paste(cw_size, cw_timedep))
sim_by_cw <- split(sim_by_cw, sim_by_cw$cw_type)


## practice with moderate smooth

cw_results_fx <- function(x) {
  
  cw_type = names(x[1])
  cw_data = x[[1]]
  
  data.frame("cw_type" = cw_type,
             "effect_recovered" = prop.table(table(cw_data$effect_recovered))[[1]],
             "critical_effect_recovered" = prop.table(table(cw_data[which(cw_data$cw == "critical"), 
                                                                    "effect_recovered"]))[[1]],
             "trend_diff_mean" = summary(cw_data$trend_recovered_diff)[["Mean"]],
             "critical_trend_diff_mean" = summary(cw_data[which(cw_data$cw == "critical"), 
                                                          "trend_recovered_diff"])[["Mean"]],
             "true_peak_recovered" = ifelse(grepl("naive", cw_type) == TRUE, NA,
                                            prop.table(table(cw_data[which(cw_data$inflection_true == "peak"), 
                                                                     "inflection_recovered"]))[[1]]),
             "critical_effect_sig" = prop.table(table(cw_data[which(cw_data$cw == "critical"), 
                                                              "p_value"] < 0.05))[[1]],
             "critical_effect_sig" = prop.table(table(cw_data[which(cw_data$cw == "critical"), 
                                                              "p_value"] < 0.05))[[1]],
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


head(sim_res)

sim_res |>
  mutate(cw_type = paste(cw_size, cw_timedep),
         week = as.factor(as.numeric(gsub("tx_", "", treatment)))) |>
  ggplot(aes(y = week, color = cw)) +
  scale_color_manual(values=c("red", "black")) +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), alpha = 0.35) +
  geom_jitter(aes(x = mean), shape = 15, size = 1, alpha = 0.1) +
  labs(x = "Predicted Change in Birthweight (g)", 
       y = "Week", title = "Simulation results",
       subtitle = "100 sims (5K samples each), total fx = 166, e~N(0, 10)") +
  theme(axis.text.x = element_text(angle = 45)) +
  guides(color = FALSE) +
  geom_vline(xintercept = 0, linetype="dashed") +
  coord_flip() +
  theme_classic() +
  # geom_text(aes(x = (ci95.hi + 0.5), y=variable, label=variable), hjust=0, fontface = "bold") +
  # geom_text(aes(x = -1, y = variable, label=round(estimate, 2)), hjust=0, fontface = "plain") +
  facet_wrap(~cw_type, nrow = 3)


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
                            "wide naive", "wide smooth",
                            "moderate naive", "moderate smooth",
                            "narrow naive", "narrow smooth")) |>
  ggplot(aes(y = week, color = cw, group = cw_type)) +
  scale_color_manual(values=c("orangered", "black")) +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_path(data = ~filter(.x, true_pred == "true"), 
            aes(y = week, x = mean, group = cw_type), lwd = 1, color = "gray80") +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), alpha = 0.35) +
  geom_point(aes(x = mean), shape = 15, size = 1, alpha = 0.1) +
  # geom_point(data = ~filter(.x, true_pred == "true"), 
  #            aes(y = week, x = mean, group = cw_type), color = "gray80") +
  labs(x = "Predicted Change in Birthweight (g)", 
       y = "Week", title = "Simulation results",
       subtitle = "100 sims per CW type (5K samples each), total fx = -166, e~N(0, 10)") +
  theme(axis.text.x = element_text(angle = 45)) +
  guides(color = FALSE) +
  coord_flip() +
  theme_classic() +
  facet_wrap(~cw_type, nrow = 3)








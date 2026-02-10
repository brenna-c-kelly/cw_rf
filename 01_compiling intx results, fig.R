
library(purrr)
library(dplyr)
library(ggpubr)
library(ggplot2)
library(forcats)
library(Metrics)
library(data.table)
library(RColorBrewer)

sim_files <- list()

for(i in 1:1000){#length(list.files("data/interaction_results/", recursive = TRUE, include.dirs = TRUE, pattern = ".csv"))) {
  
  sim_files[[i]] <- 
    fread(paste0("data/00_interaction_results/",
                 list.files("data/00_interaction_results/", recursive = TRUE, include.dirs = TRUE, pattern = ".csv")[i]))
  
}

sim_res <- map_df(sim_files, ~as.data.frame(.))

# sim_by_cw <- sim_res |>
#   mutate(cw_type = paste(cw_size, cw_timedep))
# sim_by_cw <- split(sim_by_cw, sim_by_cw$cw_type)


# need to get true effect

true_fx <- read.csv("data/true_cw_fx_-15_v2.csv")

true_fx <- true_fx |>
  select(norm_moderate_fx) |>
  mutate(week = rownames(true_fx)) |>
  rbind(c("-12", "16_17")) |>
  rename(true_effect = norm_moderate_fx)

true_fx$true_effect <- as.numeric(true_fx$true_effect)

true_fx_long <- do.call("rbind", replicate((nrow(sim_res) / 37), true_fx, simplify = FALSE))

sim_res_true <- cbind(sim_res, true_fx_long)

sim_res_true$effect_recovered <- ifelse(sim_res_true$true_effect > sim_res_true$ci_lower &
                                          sim_res_true$true_effect < sim_res_true$ci_upper,
                                        "TRUE", "FALSE")

sim_res_true$cw = ifelse(sim_res_true$treatment %in% c("tx_16", "tx_17", "tx_16_17"), 
                         "critical", "not critical")

sim_res_true$true_effect <- as.numeric(sim_res_true$true_effect)


# label the interaction and components

# did the interaction exceed the sum of the components?
# true_intx = mean(ifelse(intx_effect < (w16_effect + w17_effect), 1, 0))

## compiling results

# subtract mean of y from y
c_y = sim_res_true$true_effect - mean(sim_res_true$true_effect)
# subtract mean of y_hat from y_hat
c_yhat = sim_res_true$mean - mean(sim_res_true$mean)
## centered RMSE for pattern accuracy
c_rmse = rmse(c_y, c_yhat)

## other variables
intx_res <- data.frame("cw_type" = "interaction",
                       "empirical_coverage" = prop.table(table(sim_res_true$effect_recovered))[["TRUE"]],
                       "cw_empirical_coverage" = prop.table(table(sim_res_true[which(sim_res_true$cw == "critical"), 
                                                                               "effect_recovered"]))[["TRUE"]],
                       "centered_rmse" = c_rmse,
                       "ci_width_average" = mean(sim_res_true$ci_lower - sim_res_true$ci_upper),
                       "critical_effect_sig" = prop.table(table(sim_res_true[which(sim_res_true$cw == "critical"), 
                                                                             "p_value"] < 0.05))[["TRUE"]],
                       "critical_effect_sig" = prop.table(table(sim_res_true[which(sim_res_true$cw == "critical"), 
                                                                             "p_value"] < 0.05))[["TRUE"]],
                       "critical_pval_mean" = summary(sim_res_true[which(sim_res_true$cw == "critical"), "p_value"])[["Mean"]],
                       "critical_pval_min" = summary(sim_res_true[which(sim_res_true$cw == "critical"), "p_value"])[["Min."]],
                       "critical_pval_max" = summary(sim_res_true[which(sim_res_true$cw == "critical"), "p_value"])[["Max."]])

intx_res

write.csv(intx_res, "data/results/cw_res_intx_df_tidy.csv", row.names = FALSE)

# always larger effect in interaction than in components
prop.table(table(sim_res_true[which(sim_res_true$treatment == "tx_16_17"), "mean"] <
             sim_res_true[which(sim_res_true$treatment == "tx_16"), "mean"]))
prop.table(table(sim_res_true[which(sim_res_true$treatment == "tx_16_17"), "mean"] <
                   sim_res_true[which(sim_res_true$treatment == "tx_17"), "mean"]))
prop.table(table(sim_res_true[which(sim_res_true$treatment == "tx_16_17"), "mean"] <
                   (sim_res_true[which(sim_res_true$treatment == "tx_17"), "mean"] +
                      sim_res_true[which(sim_res_true$treatment == "tx_16"), "mean"])))

# is the highest variable correct? (intx > 16 or 17)
# "true_peak_recovered" = ifelse(grepl("naive", cw_type) == TRUE, NA,
#                                prop.table(table(cw_data[which(cw_data$inflection_true == "peak"), 
#                                                         "inflection_recovered"]))[["TRUE"]])

## individual effects for different treatments

ci_width_combo <- sim_res_true |> 
  filter(treatment == "tx_16_17") |> 
  mutate(ci_width = ci_upper - ci_lower) |> 
  select(ci_width)

median(ci_width_combo$ci_width) # 6.397
quantile(ci_width_combo$ci_width, probs = c(0.05, 0.95)) # 5.588896 7.719929 

ci_width_16 <- sim_res_true |> 
  filter(treatment %in% c("tx_16")) |> 
  mutate(ci_width = ci_upper - ci_lower) |> 
  select(ci_width)

median(ci_width_16$ci_width) # 9.005716
quantile(ci_width_16$ci_width, probs = c(0.05, 0.95)) # 7.223107 15.409907

ci_width_17 <- sim_res_true |> 
  filter(treatment %in% c("tx_17")) |> 
  mutate(ci_width = ci_upper - ci_lower) |> 
  select(ci_width)

median(ci_width_17$ci_width) # 9.483512
quantile(ci_width_17$ci_width, probs = c(0.05, 0.95)) # 7.697712 14.619673


## plotting results

# sort
true_fx <- true_fx |>
  mutate(week = ifelse(week == "16_17", 16.5, week)) |>
  mutate(week = as.numeric(week)) |>
  arrange(week)

sim_res_true <- sim_res_true |>
  mutate(week = ifelse(week == "16_17", 16.5, week)) |>
  mutate(week = as.numeric(week)) |>
  arrange(week)

sim_res_true$cw <- ifelse(sim_res_true$true_effect < -0.5 & sim_res_true$cw != "critical",
                          "part of cw", sim_res_true$cw)
sim_res_true$cw <- ifelse(sim_res_true$treatment == "tx_16_17", "combination", 
                          sim_res_true$cw)

# store table
write.csv(sim_res_true, "data/results/intx_res_df_tidy.csv", row.names = FALSE)

# both true values and simulation results
sim_res_true |>
  arrange(week) |>
  ggplot(aes(y = week, color = cw)) +
  scale_color_manual(values = c("firebrick", "orangered", "black", "orange")) + ##ff4b00
  geom_path(data = true_fx, 
            aes(y = week, x = true_effect, group = 1), lwd = 1, color = "gray80") +
  geom_vline(xintercept = 0, linetype="dashed") +
  geom_linerange(aes(xmin = ci_lower, xmax = ci_upper), alpha = 0.1) +
  geom_point(aes(x = mean), shape = 15, size = 1, alpha = 0.1) +
  labs(x = "Predicted Change in Birthweight Percentile", 
       y = "Week", title = "Interaction simulation ATEs") +
  coord_flip() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45))


heatmap(as.matrix(sim_res_true$mean))


heat_dat <- data.frame(rowid = c("16", "16", "17", "17"),
                       colid = c("16", "17", "16", "17"),
                       true_effect = c(0,
                                       unique(sim_res_true[which(sim_res_true$week %in% c(16)), "true_effect"]),
                                       unique(sim_res_true[which(sim_res_true$week %in% c(17)), "true_effect"]),
                                       unique(sim_res_true[which(sim_res_true$week %in% c(16.5)), "true_effect"])),
                       ate = c(0, # nothin
                               mean(sim_res_true[which(sim_res_true$week %in% c(16)), "mean"]),
                               mean(sim_res_true[which(sim_res_true$week %in% c(17)), "mean"]),
                               mean(sim_res_true[which(sim_res_true$week %in% c(16.5)), "mean"])),
                       lo = c(0, # nothin
                               mean(sim_res_true[which(sim_res_true$week %in% c(16)), "ci_lower"]),
                               mean(sim_res_true[which(sim_res_true$week %in% c(17)), "ci_lower"]),
                               mean(sim_res_true[which(sim_res_true$week %in% c(16.5)), "ci_lower"])),
                       hi = c(0, # nothin
                               mean(sim_res_true[which(sim_res_true$week %in% c(16)), "ci_upper"]),
                               mean(sim_res_true[which(sim_res_true$week %in% c(17)), "ci_upper"]),
                               mean(sim_res_true[which(sim_res_true$week %in% c(16.5)), "ci_upper"])))

heat_dat$true_effect <- round(heat_dat$true_effect, digits = 2)
heat_dat$ate_round <- round(heat_dat$ate, digits = 2)
heat_dat$ci_lo_round <- round(heat_dat$lo, digits = 2)
heat_dat$ci_hi_round <- round(heat_dat$hi, digits = 2)

heat_dat$label <- paste0(heat_dat$ate_round, "\n 95% CI [",
                         heat_dat$ci_lo_round, ", ", 
                         heat_dat$ci_hi_round, "]", "\n",
                         "True effect: ", heat_dat$true_effect)

hm <- ggplot(heat_dat, aes(x = rowid, y = colid, fill = ate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "orangered", high = "white") + 
  coord_equal() +
  labs(x = "", y = "", fill = "Average \nATE",
       title = "Combined exposure ATE recovery") +
  theme_void()

hm +
  geom_text(aes(label = label), color = "black", size = 4.5)




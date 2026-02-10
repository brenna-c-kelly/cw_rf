
library(purrr)
library(dplyr)
library(tidyr)
library(Metrics)
library(ggplot2)
library(data.table)

sim_data_list <- list()

for(i in 1:length(list.files("data/00_interaction_results/"))){
  
  sim_data_list[[i]] <-
    fread(paste0("data/00_interaction_results/",
                 list.files("data/00_interaction_results/", recursive = TRUE, include.dirs = TRUE, pattern = ".csv")[i]),)
  
}


## generate the list of indices to test
indices_list <- list()

for(i in 1:1000) {
  
  j = 1
  indices_list[[i]] <- i:j
  j = j + 1
  
}

## for interactions, add the true effects

true_fx <- read.csv("data/true_cw_fx_-15_v2.csv")

true_fx <- true_fx |>
  select(norm_moderate_fx) |>
  mutate(week = rownames(true_fx)) |>
  rbind(c("-12", "16_17")) |>
  rename(true_effect = norm_moderate_fx) |>
  mutate(true_effect = as.numeric(true_effect))

# to add to existing results
true_bind <- function(data) {
  
  cbind(data, true_fx)
  
}

# apply across list of results
sim_intx_list <- lapply(sim_data_list, true_bind)

## add result info
res_add <- function(data) {
  
  # empirical coverage
  data$effect_recovered <- ifelse(data$true_effect > data$ci_lower &
                                    data$true_effect < data$ci_upper,
                                  "TRUE", "FALSE")
  # is it a critical effect?
  data$cw = ifelse(data$treatment %in% c("tx_16", "tx_17", "tx_16_17"), 
                   "critical", "not critical")
  # etc
  # data$cw_type = "interaction"
  # data$intx = case_when(data$treatment == "tx_16_17" ~ "intx",
  #                       data$treatment %in% c("tx_16", "tx_17") ~ "components")
  
  return(data)
  
}

# apply across list of results
sim_intx_list <- lapply(sim_intx_list, res_add)


# create a function to calculate the all metrics
recovery <- function(data) {
  
  # is the intx > sum(components)
  intx_effect = data[which(data$treatment == "tx_16_17"), "mean"]
  w16_effect = data[which(data$treatment == "tx_16"), "mean"]
  w17_effect = data[which(data$treatment == "tx_17"), "mean"]
  
  data$true_effect = as.numeric(data$true_effect)
  
  list(ci_width = mean(data$ci_upper - data$ci_lower),
       mean_ate = mean(data$mean),
       c_rmse_ate = rmse(
         (data$true_effect - mean(data$true_effect)), # centered y
         (data$mean - mean(data$mean))), # centered y-hat
       true_intx = mean(ifelse(intx_effect < (w16_effect + w17_effect), 1, 0)))
}

test <- sim_intx_list[1][[1]]
test <- do.call(rbind, sim_data_list[unlist(indices_list[2][[1]])])

rmse((test$true_effect - mean(test$true_effect)),
     (test$mean - mean(test$mean)))

# create a function to apply the function across all indices in the list
recovery_stability <- function(i) {
  list(ci_width = recovery(do.call(rbind, sim_intx_list[unlist(indices_list[i][[1]])]))$ci_width,
       mean_ate = recovery(do.call(rbind, sim_intx_list[unlist(indices_list[i][[1]])]))$mean,
       c_rmse_ate = recovery(do.call(rbind, sim_intx_list[unlist(indices_list[i][[1]])]))$c_rmse_ate,
       true_intx = recovery(do.call(rbind, sim_intx_list[unlist(indices_list[i][[1]])]))$true_intx)
}

recovery(do.call(rbind, sim_data_list[unlist(indices_list[1][[1]])]))

# apply across 1, 1:2, 1:3, etc.
sim_stability <- map_df(lapply(indices_list, recovery_stability), ~as.data.frame(.))
head(sim_stability)

sim_stability$n_sims <- 1:1000
# names(sim_stability) <- c("ate", "n_sims") #ci_width_average

sim_stability |>
  pivot_longer(cols = c("ci_width", "mean_ate", "c_rmse_ate"), names_to = "metric") |>
  ggplot(aes(x = n_sims, y = value)) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~metric, scales = "free", nrow = 3) +
  ggtitle("Interaction Stability and Convergence")


#####
# create a function for stability assessment
#####

# search, select, create new folder for each CW type

paths <- c("data/00_sim_results/moderate_naive/",
           "data/00_sim_results/moderate_smooth/",
           "data/00_sim_results/narrow_naive/",
           "data/00_sim_results/narrow_smooth/",
           "data/00_sim_results/wide_naive/",
           "data/00_sim_results/wide_smooth/")

stab <- function(path) {
  
  sim_data_list <- list()
  
  for(i in 1:length(list.files(path))){
    
    sim_data_list[[i]] <-
      fread(paste0(path,
                   list.files(path, recursive = TRUE, include.dirs = TRUE, pattern = ".csv")[i]),)
    
  }
  
  ## generate the list of indices to test
  indices_list <- list()
  
  for(i in 1:1000) {
    j = 1
    indices_list[[i]] <- i:j
    j = j + 1
  }
  
  # create a function to calculate the all metrics
  recovery <- function(data) {
    list(ci_width = mean(data$ci_upper - data$ci_lower),
         mean_ate = mean(data$mean),
         c_rmse_ate = rmse(
           (data$true_effect - mean(data$true_effect)), # centered y
           (data$mean - mean(data$mean))), # centered y-hat
         # true_peak_recovered = ifelse(grepl("smooth", path) == TRUE,
         #                              prop.table(table(data[which(data$inflection_true == "peak"),
         #                                                    "infection_recovered"]))[["TRUE"]],
         #                              NA),
         # critical_effect_sig = prop.table(table(data[which(data$cw == "critical"), 
         #                                             "p_value"] < 0.05))[["TRUE"]],
         cw_type = substr(path, start = 21, stop = (nchar(path) - 1)))}
  
  # create a function to apply the function across all indices in the list
  recovery_stability <- function(i) {
    list(
      ci_width = recovery(do.call(rbind, sim_data_list[unlist(indices_list[i][[1]])]))$ci_width,
      mean_ate = recovery(do.call(rbind, sim_data_list[unlist(indices_list[i][[1]])]))$mean,
      c_rmse_ate = recovery(do.call(rbind, sim_data_list[unlist(indices_list[i][[1]])]))$c_rmse_ate
      # true_peak_recovered = recovery(sim_data_list[unlist(indices_list[i])][[1]])$true_peak_recovered,
      # critical_effect_sig = recovery(sim_data_list[unlist(indices_list[i])][[1]])$critical_effect_sig,
      )
  }
  
  # apply across 1, 1:2, 1:3, etc.
  sim_stability <- map_df(lapply(indices_list, recovery_stability), ~as.data.frame(.))
  sim_stability$n_sims <- 1:1000
  # names(sim_stability) <- c("ate", "n_sims") #ci_width_average
  
  return(sim_stability)
  
}

# apply to scenarios
res_mod_naive <- stab(paths[1]) |>
  mutate(cw_type = "moderate, naive")
res_mod_smooth <- stab(paths[2]) |>
  mutate(cw_type = "moderate, smooth")
res_nar_naive <- stab(paths[3]) |>
  mutate(cw_type = "narrow, naive")
res_nar_smooth <- stab(paths[4]) |>
  mutate(cw_type = "narrow, smooth")
res_wid_naive <- stab(paths[5]) |>
  mutate(cw_type = "wide, naive")
res_wid_smooth <- stab(paths[6]) |>
  mutate(cw_type = "wide, smooth")

# plot
rbind(res_mod_naive,
      res_mod_smooth,
      res_nar_naive,
      res_nar_smooth,
      res_wid_naive,
      res_wid_smooth) |>
  pivot_longer(cols = c("ci_width", "mean_ate", "c_rmse_ate"), names_to = "metric") |>
  ggplot(aes(x = n_sims, y = value)) +
  geom_point() +
  theme_minimal() +
  facet_wrap(~cw_type+metric, scales = "free", nrow = 6) +
  ggtitle("Stability and convergence for CW scenarios")









library(grf)
library(dplyr)
library(hstats)

list.files("data/")


births = read.csv("data/birth_clean_wide.csv")
# births_long = pd.read_csv("data/birth_clean_long.csv")

# o3_mean = births_long['max_o3'].mean()
# o3_sd = births_long['max_o3'].std()

# bw_mean = births["birthweightgrams"].mean()
# bw_sd = births["birthweightgrams"].std()

births <- births[1:1000, ]

t = births |>
  select(!c("momid", "gestation", "birthweightgrams"))
    # max_o3_07,
    #      max_o3_08,
    #      max_o3_09)

c.forest <- causal_forest(t, 
                          births$birthweightgrams, births$gestation)


average_treatment_effect(c.forest)

## importance
imp <- sort(setNames(variable_importance(c.forest), names(t)))
par(mai = c(0.7, 2, 0.2, 0.2))
barplot(imp, horiz = TRUE, las = 1, col = "orange")

## pdp
pred_fun <- function(object, newdata) {
  predict(object, newdata)$predictions
}

for(i in names(t)) {
  partial_dep(c.forest, i, 
              X = t) |>
    plot()
  
}

partial_dep(c.forest, "max_o3_04", 
            X = t) |>
  plot()

# pdps <- lapply(names(t), function(v) plot(partial_dep(c.forest, v, X = T, pred_fun = pred_fun)))
# wrap_plots(pdps, guides = "collect", ncol = 3) &
#   ylim(c(-0.11, -0.06)) &
#   ylab("Treatment effect")

## H
H <- hstats(c.forest, X = t, #pred_fun = pred_fun, 
            verbose = FALSE)
plot(H)


# imp <- sort(setNames(variable_importance(fit), xvars))
# par(mai = c(0.7, 2, 0.2, 0.2))
# barplot(imp, horiz = TRUE, las = 1, col = "orange")

partial_dep(c.forest, v = c("max_o3_07",
                            "max_o3_09"), X = t,
            trim = c(0.01, 0.99),
            strategy = "quantile",
            na.rm = TRUE) |> 
  plot()


library(vivid)

vivi(
  data = cbind(c.forest$X.orig,
               birthweightgrams = births$birthweightgrams,
               gestation = c.forest$W.orig),
  fit = c.forest,
  response = "birthweightgrams",
  # gridSize = 50,
  # importanceType = "agnostic",
  # nmax = 500,
  # reorder = TRUE,
  # class = 1,
  # predictFun = NULL,
  # normalized = FALSE,
  # numPerm = 4,
  # showVimpError = FALSE,
  # vars = NULL
)



c.forest$X.orig






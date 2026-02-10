

# Install and load the "zyp" package
install.packages("funtimes")
library(zyp)
library(funtimes)

# Sample data representing annual temperature measurements over 10 years
# temperature <- c(15, 16, 17, 19, 18, 20, 21, 22)
dat <- read.csv("/Users/brenna/Downloads/results_v11_37w_mainmodel.csv")
dat <- dat |>
  select(mean)

# Perform the Mann-Kendall Trend Test
result <- MannKendall(dat$mean)

# Print the result
print(result)
# tau = -0.111, 2-sided pvalue =0.3473
# slight negative trend in data

# any trend
apply(dat$mean, 2, function(x) notrend_test(x, test = "WAVK", 
                                     factor.length = "adaptive.selection")$p.value)

notrend_test(dat$mean, test = "WAVK", Window = 4) # lowest p value at 4

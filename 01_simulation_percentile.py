######
# generating simulated data, running models, and storing results
######

## libraries
import os
import numpy as np
import pandas as pd
import seaborn as sns
import scipy.stats as stats
import multiprocessing as mp
import statsmodels.api as sm
from scipy.stats import norm
import matplotlib.pyplot as plt
from econml.dml import LinearDML
from econml.dml import CausalForestDML

wd = os.chdir("/Users/brenna/Documents/School/Research/causal_rf/cw_rf")

np.set_printoptions(suppress=True)

## critical window coefficient creation
study_period = np.linspace(1, 20, 20)
effect_size = -15
window_start = 6
window_center = 10
window_end = 14

uni_pdf = stats.Uniform(a = window_start, b = window_end)
uni_wide_fx  = uni_pdf.pdf(study_period) * effect_size

uni_pdf = stats.Uniform(a = window_start + 1, b = window_end - 1)
uni_moderate_fx  = uni_pdf.pdf(study_period) * effect_size

uni_pdf = stats.Uniform(a = window_start + 3, b = window_end - 3)
uni_narrow_fx  = uni_pdf.pdf(study_period) * effect_size

norm_pdf = stats.norm.pdf(study_period, window_center, 2.5)
norm_wide_fx  = norm_pdf * effect_size 

norm_pdf = stats.norm.pdf(study_period, window_center, 1)
norm_moderate_fx  = norm_pdf * effect_size 

norm_pdf = stats.norm.pdf(study_period, window_center, 0.5)
norm_narrow_fx = norm_pdf * effect_size

## combinations to be simulated
cw_coefs = pd.DataFrame({"norm_wide_fx": norm_wide_fx, 
              "norm_moderate_fx": norm_moderate_fx, 
              "norm_narrow_fx": norm_narrow_fx,
              "uni_wide_fx": uni_wide_fx, 
              "uni_moderate_fx": uni_moderate_fx, 
              "uni_narrow_fx": uni_narrow_fx})

cw_combos = pd.DataFrame({"coefficients": [cw_coefs["norm_wide_fx"], 
                                           cw_coefs["norm_moderate_fx"], 
                                           cw_coefs["norm_narrow_fx"],
                                           cw_coefs["uni_wide_fx"], 
                                           cw_coefs["uni_moderate_fx"], 
                                           cw_coefs["uni_narrow_fx"]],
                          "sizes": ["wide", "moderate", "narrow",
                                    "wide", "moderate", "narrow"],
                          "times": ["smooth", "smooth", "smooth",
                                    "naive", "naive", "naive"]})

### save, for plotting
cw_coefs.to_csv("/Users/brenna/Documents/School/Research/causal_rf/cw_rf/data/" + "true_cw_fx_" + str(effect_size) + ".csv", sep = ',', index = False)

## reading the reference data
births = pd.read_csv("data/birth_w_percentile_confounders.csv")
births.head()

o3_mean = births[['max_o3_01', 'max_o3_02', 'max_o3_03', 'max_o3_04', 
                  'max_o3_05', 'max_o3_06', 'max_o3_07', 'max_o3_08', 
                  'max_o3_09', 'max_o3_10', 'max_o3_11', 'max_o3_12', 
                  'max_o3_13', 'max_o3_14', 'max_o3_15', 'max_o3_16', 
                  'max_o3_17', 'max_o3_18', 'max_o3_19', 'max_o3_20']].values.mean()
o3_sd = births[['max_o3_01', 'max_o3_02', 'max_o3_03', 'max_o3_04', 
                'max_o3_05', 'max_o3_06', 'max_o3_07', 'max_o3_08', 
                'max_o3_09', 'max_o3_10', 'max_o3_11', 'max_o3_12', 
                'max_o3_13', 'max_o3_14', 'max_o3_15', 'max_o3_16', 
                'max_o3_17', 'max_o3_18', 'max_o3_19', 'max_o3_20']].values.std()

bwp_mean = births["bw_percentile"].mean()
bwp_sd = births["bw_percentile"].std()

## function — simulates data, runs model, stores results
def dml_sim_function(iteration, cw_window_type, window_size, window_time):

    #### Part I: data generation
    n_samples = 5000

    n_X = 1
    n_T = 20
    n_W = 1

    ## treatments / exposures; 10 ppb scale
    T_sample = births.sample(n = n_samples, replace = False)

    T_01 = (T_sample['max_o3_01'] - o3_mean) / 10
    T_03 = (T_sample['max_o3_03'] - o3_mean) / 10
    T_02 = (T_sample['max_o3_02'] - o3_mean) / 10
    T_04 = (T_sample['max_o3_04'] - o3_mean) / 10
    T_05 = (T_sample['max_o3_05'] - o3_mean) / 10
    T_06 = (T_sample['max_o3_06'] - o3_mean) / 10
    T_07 = (T_sample['max_o3_07'] - o3_mean) / 10
    T_08 = (T_sample['max_o3_08'] - o3_mean) / 10
    T_09 = (T_sample['max_o3_09'] - o3_mean) / 10
    T_10 = (T_sample['max_o3_10'] - o3_mean) / 10
    T_11 = (T_sample['max_o3_11'] - o3_mean) / 10
    T_12 = (T_sample['max_o3_12'] - o3_mean) / 10
    T_13 = (T_sample['max_o3_13'] - o3_mean) / 10
    T_14 = (T_sample['max_o3_14'] - o3_mean) / 10
    T_15 = (T_sample['max_o3_15'] - o3_mean) / 10
    T_16 = (T_sample['max_o3_16'] - o3_mean) / 10
    T_17 = (T_sample['max_o3_17'] - o3_mean) / 10
    T_18 = (T_sample['max_o3_18'] - o3_mean) / 10
    T_19 = (T_sample['max_o3_19'] - o3_mean) / 10
    T_20 = (T_sample['max_o3_20'] - o3_mean) / 10

    ## stack
    T_vars = np.vstack((T_01, T_02, T_03, T_04, T_05,
                        T_06, T_07, T_08, T_09, T_10,
                        T_11, T_12, T_13, T_14, T_15,
                        T_16, T_17, T_18, T_19, T_20))

    ## creating confounder coefficient
    critical_to_confound = [index for index, value in enumerate(cw_window_type) if value < -0.5]

    X = [sum(fx) for fx in zip(*T_vars[critical_to_confound]*1.25)] + np.random.normal(size = n_samples, loc = 0, scale = 1)
    # z scale
    X = X / X.std()
    X = pd.Series(X)
    X = X.to_numpy()

    ## creating the outcome

    # the coefficients and treatment variables
    tx_fx, tx = cw_window_type, T_vars
    # the vectors of each coefficient * treatment variable
    tx_fx_list = [tx[i] * tx_fx[i] for i in np.arange(0, 20)]
    # tx_fx_list = [tx[10] * tx_fx[10]]
    # total treatment effect by individual
    total_tx_fx = [sum(fx) for fx in zip(*tx_fx_list)]

    # the confounder effect (z-scaled, see above)
    b_W0y = 1.5
    # the vector of confounder effects
    wx_fx = X * b_W0y

    # bit of noise
    e = np.random.normal(size=n_samples, loc = 0, scale = 1)

    b_int = bwp_mean

    y = b_int + total_tx_fx + wx_fx + e

    sim_dat = pd.DataFrame({'sim_index': iteration, 'cw_size': window_size,
                            'cw_timedep': window_time, #"critical": critical_or_not,
                            'total_fx_size': effect_size,
                            'y_hat': y, 'true_y': T_sample["birthweightgrams"], 'x': X,
                            'tx_01': T_01, 'tx_02': T_02, 'tx_03': T_03, 'tx_04': T_04, 
                            'tx_05': T_05, 'tx_06': T_06, 'tx_07': T_07, 'tx_08': T_08, 
                            'tx_09': T_09, 'tx_10': T_10, 'tx_11': T_11, 'tx_12': T_12, 
                            'tx_13': T_13, 'tx_14': T_14, 'tx_15': T_15, 'tx_16': T_16, 
                            'tx_17': T_17, 'tx_18': T_18, 'tx_19': T_19, 'tx_20': T_20})


    #### Part II: model training
    # dir = "data/sims/" + "sim" + str(iteration).zfill(3) + "_" + window_size + "_" + window_time + ".csv"
    # sim_dat = pd.read_csv(dir)

    T = sim_dat.loc[:, "tx_01":"tx_20"]
    X = pd.DataFrame({"confounder": sim_dat["x"]})
    y = pd.DataFrame({"birthweight": sim_dat["y_hat"]})
    # convert to 1d array
    # y = y.to_numpy().flatten()

    est = CausalForestDML(model_t='forest',
                          model_y='forest',
                          discrete_treatment=False,
                          n_estimators=500,
                          n_jobs = 4)

    est.fit(y, T=T, X=X, W=X)

    # # extract results
    treatments = np.array(est.cate_treatment_names())
    res = est.marginal_ate_inference(T, X)
    means = res.mean_point #est.marginal_ate(T, X)
    ci_lower, ci_upper = res.conf_int_mean()
    p_vals = res.pvalue()

    # # create dataframe
    res_df = pd.DataFrame({
        'sim_index': iteration,
        'cw_size': window_size, 'cw_timedep': window_time,
        'treatment': treatments,
        'true_effect': cw_window_type,
        'mean': means[0],
        'ci_lower': ci_lower[0],
        'ci_upper': ci_upper[0],
        'p_value': p_vals[0]
    })
    
    # indicate if critical
    res_df["cw"] = ["critical" if x < -0.5 else "not critical" for x in res_df["true_effect"]]

    ## test results for critical window

    # 1. is the true effect recovered for each week?
    res_df["effect_recovered"] = res_df["true_effect"].between(res_df["ci_lower"], res_df["ci_upper"])

    # 2. did it get the trend? i.e., the difference in value over time
    # what the difference should be:
    diff_list_true = [np.nan]
    for i in range(1, len(cw_window_type)):
        diff_list_true.append(cw_window_type[i] - cw_window_type[i - 1])
    res_df["trend_true"] = diff_list_true

    diff_list_pred = [np.nan]
    for i in range(1, len(res_df["mean"])):
        diff_list_pred.append(res_df.loc[i, "mean"] - res_df.loc[i - 1, "mean"])
    res_df["trend_pred"] = diff_list_pred

    # how close is the true trend to the observed trend?
    res_df["trend_recovered_diff"] = (res_df["trend_true"] - res_df["trend_pred"])

    # 3. did it get the peak effect? i.e., the inflection point
    # what the inflection point should be:
    inflection = []
    for i in range(len(diff_list_true) - 1):
        if diff_list_true[i] < 0 and diff_list_true[i + 1] > 0:
            inflection.append("peak")
        else:
            inflection.append("not peak")
    inflection.append(np.nan) # no next value
    res_df["inflection_true"] = inflection

    # what was the predicted inflection point?
    inflection = []
    for i in range(len(diff_list_pred) - 1):
        if diff_list_pred[i] < 0 and diff_list_pred[i + 1] > 0:
            inflection.append("peak")
        else:
            inflection.append("not peak")
    inflection.append(np.nan) # no next value
    res_df["inflection_pred"] = inflection

    # is the peak effect recovered?
    res_df["inflection_recovered"] = (res_df["inflection_true"] == res_df["inflection_pred"])

    # 4. was the critical effect (>5) statistically significant at alpha = 0.05?
    critical_sig = []

    for index, item in enumerate(res_df["p_value"]):
        if index in [index for index, value in enumerate(cw_window_type) if value < -0.5]:
            if item < 0.05:
                critical_sig.append("TRUE")
            else:
                critical_sig.append("FALSE")
        else:
            critical_sig.append(np.nan)

    res_df["critical_sig_recovered"] = critical_sig

    res_df.to_csv("data/sim_results/" + "sim" + str(iteration).zfill(3) + "_res" + "_" + window_size + "_" + window_time + ".csv", sep = ',', index = False)

# nsims = 1

## apply to all cw structures
# for i in range(0, len(cw_combos)):
#     for x in range(1, nsims + 1):
#         dml_sim_function(x, cw_combos.loc[i, "coefficients"],
#                      cw_combos.loc[i, "sizes"], 
#                      cw_combos.loc[i, "times"])
        

# nsims = 2
def simple_function(x):
    
    for i in range(0, len(cw_combos)):
        # for x in range(1, nsims + 1):
        dml_sim_function(x, cw_combos.loc[i, "coefficients"],
                    cw_combos.loc[i, "sizes"], 
                    cw_combos.loc[i, "times"])

def main():
    nsims = range(5)

    with mp.Pool(processes = 4) as pool:
        results = pool.map(simple_function, nsims)
    
    print(results)
    

if __name__ == '__main__':
    main()

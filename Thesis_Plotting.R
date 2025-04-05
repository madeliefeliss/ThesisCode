library(metacart)

set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.3_cor0.7.csv")

y <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
           vi = vark, data = dat, c.pruning = 0)


plot.PV(y, iter = 10)

plot_pruning_tree(y, iter = 10)

results <- analyze_splits(y, iter = 10)
results


resultsM1 <- analyze_splits(y, iter = 100, var_of_interest = "m1")
results$threshold_plot        # View distribution of cutpoints for m1



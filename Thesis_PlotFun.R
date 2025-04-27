library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.3_cor0.7.csv")

y <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
           vi = vark, data = dat, c.pruning = 0)


plot.PV(y, iter = 10)
plot.PV.I(y, iter = 10)
plot.PV.TN(y, iter = 10)
warnings()


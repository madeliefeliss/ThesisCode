library(future)

# Parallel setup
plan(multisession, workers = 15)

# Seed:
set.seed(3568970)  
fixed_seeds <- sample(1:1e6, 1000)  
saveRDS(fixed_seeds, "fixed_seeds.rds")

# Loading the fixed seeds:
fixed_seeds <- readRDS("fixed_seeds.rds")

# Categorical:
# 1:
f1 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.3_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.3_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.3_c0.3_p0_cptable_list.rds")

"1 Done"
}, seed = TRUE)

# 2:
f2 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.3_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.3_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.3_c0.3_p0.5_cptable_list.rds")

"2 Done"
}, seed = TRUE)

# 3:
f3 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.3_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.3_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.3_c0.5_p0_cptable_list.rds")

"3 Done"
}, seed = TRUE)

# 4:
f4 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.3_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.3_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.3_c0.5_p0.5_cptable_list.rds")

"4 Done"
}, seed = TRUE)

# 5:
f5 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.3_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.3_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.3_c0.7_p0_cptable_list.rds")

"5 Done"
}, seed = TRUE)

# 6:
f6 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.3_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.3_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.3_c0.7_p0.5_cptable_list.rds")

"6 Done"
}, seed = TRUE)

# 7:
f7 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.5_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.5_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.5_c0.3_p0_cptable_list.rds")

"7 Done"
}, seed = TRUE)

# 8:
f8 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.5_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.5_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.5_c0.3_p0.5_cptable_list.rds")

"8 Done"
}, seed = TRUE)

# 9:
f9 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.5_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.5_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.5_c0.5_p0_cptable_list.rds")

"9 Done"
}, seed = TRUE)

# 10:
f10 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.5_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.5_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.5_c0.5_p0.5_cptable_list.rds")

"10 done"
}, seed = TRUE)

# 11:
f11 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.5_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.5_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.5_c0.7_p0_cptable_list.rds")

"11 done"
}, seed = TRUE)

# 12:
f12 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T1_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T1_d0.5_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T1_d0.5_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T1_d0.5_c0.7_p0.5_cptable_list.rds")

"12 done"

}, seed = TRUE)

# 13:
f13 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.3_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.3_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.3_c0.3_p0_cptable_list.rds")

"13 done"
}, seed = TRUE)

# 14:
f14 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.3_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.3_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.3_c0.3_p0.5_cptable_list.rds")

"14 done"
}, seed = TRUE)

# 15:
f15 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.3_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.3_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.3_c0.5_p0_cptable_list.rds")

"15 done"
}, seed = TRUE)

# 16:
f16 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.3_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.3_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.3_c0.5_p0.5_cptable_list.rds")

"16 done"
}, seed = TRUE)

# 17:
f17 <- future({
  
# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {
  
  # Different seed each run:
  set.seed(fixed_seeds[i])  
  
  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)
  
  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.3_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.3_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.3_c0.7_p0_cptable_list.rds")

"17 done"
}, seed = TRUE)
  
# 18:
f18 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.3_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.3_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.3_c0.7_p0.5_cptable_list.rds")

"18 done"
}, seed = TRUE)

# 19:
f19 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.5_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.5_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.5_c0.3_p0_cptable_list.rds")

"19 done"
}, seed = TRUE)

# 20:
f20 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.5_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.5_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.5_c0.3_p0.5_cptable_list.rds")

"20 done"
}, seed = TRUE)

# 21:
f21 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.5_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.5_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.5_c0.5_p0_cptable_list.rds")

"21 done"
}, seed = TRUE)

# 22:
f22 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.5_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.5_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.5_c0.5_p0.5_cptable_list.rds")

"22 done"
}, seed = TRUE)

# 23:
f23 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.5_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.5_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.5_c0.7_p0_cptable_list.rds")

"23 done"
}, seed = TRUE)

# 24:
f24 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCAT_T2_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CAT_T2_d0.5_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.5_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.5_c0.7_p0.5_cptable_list.rds")

"24 done"
}, seed = TRUE)

# Continuous:
# 25:
f25 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.3_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.3_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.3_c0.3_p0_cptable_list.rds")

"25 done"
}, seed = TRUE)

# 26:
f26 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.3_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.3_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.3_c0.3_p0.5_cptable_list.rds")

"26 done"
}, seed = TRUE)

# 27:
f27 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.3_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.3_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.3_c0.5_p0_cptable_list.rds")

"27 done"
}, seed = TRUE)

# 28:
f28 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.3_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.3_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.3_c0.5_p0.5_cptable_list.rds")

"28 done"
}, seed = TRUE)

# 29:
f29 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.3_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.3_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.3_c0.7_p0_cptable_list.rds")

"29 done"
}, seed = TRUE)

# 30:
f30 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.3_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.3_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.3_c0.7_p0.5_cptable_list.rds")

"30 done"
}, seed = TRUE)

# 31:
f31 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.5_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.5_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.5_c0.3_p0_cptable_list.rds")

"31 done"
}, seed = TRUE)

# 32:
f32 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.5_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.5_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.5_c0.3_p0.5_cptable_list.rds")

"32 done"
}, seed = TRUE)

# 33:
f33 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.5_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.5_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.5_c0.5_p0_cptable_list.rds")

"33 done"
}, seed = TRUE)

# 34:
f34 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.5_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.5_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.5_c0.5_p0.5_cptable_list.rds")

"34 done"
}, seed = TRUE)

# 35:
f35 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.5_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.5_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.5_c0.7_p0_cptable_list.rds")

"35 done"
}, seed = TRUE)

# 36:
f36 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T1_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T1_d0.5_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T1_d0.5_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T1_d0.5_c0.7_p0.5_cptable_list.rds")

"36 done"
}, seed = TRUE)

# 37:
f37 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.3_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.3_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.3_c0.3_p0_cptable_list.rds")

"37 done"
}, seed = TRUE)

# 38:
f38 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.3_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.3_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.3_c0.3_p0.5_cptable_list.rds")

"38 done"
}, seed = TRUE)

# 39:
f39 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.3_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.3_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.3_c0.5_p0_cptable_list.rds")

"39 done"
}, seed = TRUE)

# 40:
f40 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.3_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.3_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.3_c0.5_p0.5_cptable_list.rds")

"40 done"
}, seed = TRUE)

# 41:
f41 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.3_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.3_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.3_c0.7_p0_cptable_list.rds")

"41 done"
}, seed = TRUE)

# 42:
f42 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.3_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.3_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.3_c0.7_p0.5_cptable_list.rds")

"42 done"
}, seed = TRUE)

# 43:
f43 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.5_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.5_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.5_c0.3_p0_cptable_list.rds")

"43 done"
}, seed = TRUE)

# 44:
f44 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.5_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.5_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.5_c0.3_p0.5_cptable_list.rds")

"44 done"
}, seed = TRUE)

# 45:
f45 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.5_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.5_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.5_c0.5_p0_cptable_list.rds")

"45 done"
}, seed = TRUE)

# 46:
f46 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.5_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.5_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.5_c0.5_p0.5_cptable_list.rds")

"46 done"
}, seed = TRUE)

# 47:
f47 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.5_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.5_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.5_c0.7_p0_cptable_list.rds")

"47 done"
}, seed = TRUE)

# 48:
f48 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetCON_T2_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/CON_T2_d0.5_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CON_T2_d0.5_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CON_T2_d0.5_c0.7_p0.5_cptable_list.rds")

"48 done"
}, seed = TRUE)


# Unbalanced Categorical:
# 49:
f49 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.3_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.3_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.3_c0.3_p0_cptable_list.rds")

"49 done"
}, seed = TRUE)

# 50:
f50 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.3_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.3_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.3_c0.3_p0.5_cptable_list.rds")

"50 done"
}, seed = TRUE)

# 51:
f51 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.3_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.3_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.3_c0.5_p0_cptable_list.rds")

"51 done"
}, seed = TRUE)

# 52:
f52 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.3_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.3_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.3_c0.5_p0.5_cptable_list.rds")

"52 done"
}, seed = TRUE)

# 53:
f53 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.3_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.3_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.3_c0.7_p0_cptable_list.rds")

"53 done"
}, seed = TRUE)

# 54:
f54 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.3_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.3_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.3_c0.7_p0.5_cptable_list.rds")

"54 done"
}, seed = TRUE)

# 55:
f55 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.5_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.5_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.5_c0.3_p0_cptable_list.rds")

"55 done"
}, seed = TRUE)

# 56:
f56 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.5_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.5_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.5_c0.3_p0.5_cptable_list.rds")

"56 done"
}, seed = TRUE)

# 57:
f57 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.5_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.5_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.5_c0.5_p0_cptable_list.rds")

"57 done"
}, seed = TRUE)

# 58:
f58 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.5_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.5_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.5_c0.5_p0.5_cptable_list.rds")

"58 done"
}, seed = TRUE)

# 59:
f59 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.5_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.5_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.5_c0.7_p0_cptable_list.rds")

"59 done"
}, seed = TRUE)

# 60:
f60 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T1_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T1_d0.5_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T1_d0.5_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T1_d0.5_c0.7_p0.5_cptable_list.rds")

"60 done"
}, seed = TRUE)

# 61:
f61 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.3_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.3_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.3_c0.3_p0_cptable_list.rds")

"61 done"
}, seed = TRUE)

# 62:
f62 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.3_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.3_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.3_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.3_c0.3_p0.5_cptable_list.rds")

"62 done"
}, seed = TRUE)

# 63:
f63 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.3_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.3_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.3_c0.5_p0_cptable_list.rds")

"63 done"
}, seed = TRUE)

# 64:
f64 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.3_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.3_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.3_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.3_c0.5_p0.5_cptable_list.rds")

"64 done"
}, seed = TRUE)

# 65:
f65 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.3_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.3_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.3_c0.7_p0_cptable_list.rds")

"65 done"
}, seed = TRUE)

# 66:
f66 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.3_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.3_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.3_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.3_c0.7_p0.5_cptable_list.rds")

"66 done"
}, seed = TRUE)

# 67:
f67 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.5_c0.3_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.5_c0.3_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.5_c0.3_p0_cptable_list.rds")

"67 done"
}, seed = TRUE)

# 68:
f68 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.5_cor0.3.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.5_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.5_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.5_c0.3_p0.5_cptable_list.rds")

"68 done"
}, seed = TRUE)

# 69:
f69 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.5_c0.5_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.5_c0.5_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.5_c0.5_p0_cptable_list.rds")

"69 done"
}, seed = TRUE)

# 70:
f70 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.5_cor0.5.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.5_c0.5_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.5_c0.5_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.5_c0.5_p0.5_cptable_list.rds")

"70 done"
}, seed = TRUE)

# 71:
f71 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.5_c0.7_p0_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.5_c0.7_p0_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.5_c0.7_p0_cptable_list.rds")

"71 done"
}, seed = TRUE)

# 72:
f72 <- future({

# Preparation:
library(metacart)
set.seed(3568970)
dat <- read.csv("Datasets/datasetUBCAT_T2_delta0.5_cor0.7.csv")
num_runs <- 1000

# Lists to store results:
cptable_list <- vector("list", num_runs)
terminal_node_counts <- numeric(num_runs)
moderator_list <- vector("list", num_runs)

for (i in 1:num_runs) {

  # Different seed each run:
  set.seed(fixed_seeds[i])

  # Running the model:
  test_model <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
                      vi = vark, data = dat, c.pruning = 0.5)

  # Saving the results:
  cptable_list[[i]] <- test_model$cptable
  terminal_node_counts[i] <- length(test_model$g)
  moderator_list[[i]] <- test_model$moderators
}

# Counting:
moderator_counts <- table(unlist(moderator_list))
terminal_node_counts <- table(terminal_node_counts)
terminal_node_counts

# Saving the results as RDS files:
saveRDS(moderator_counts, "Results/UBCAT_T2_d0.5_c0.7_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/UBCAT_T2_d0.5_c0.7_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/UBCAT_T2_d0.5_c0.7_p0.5_cptable_list.rds")

"72 done"
}, seed = TRUE)



value(f1)
value(f2)
value(f3)
value(f4)
value(f5)
value(f6)
value(f7)
value(f8)
value(f9)
value(f10)
value(f11)
value(f12)
value(f13)
value(f14)
value(f15)
value(f16)
value(f17)
value(f18)
value(f19)
value(f20)
value(f21)
value(f22)
value(f23)
value(f24)
value(f25)
value(f26)
value(f27)
value(f28)
value(f29)
value(f30)
value(f31)
value(f32)
value(f33)
value(f34)
value(f35)
value(f36)
value(f37)
value(f38)
value(f39)
value(f40)
value(f41)
value(f42)
value(f43)
value(f44)
value(f45)
value(f46)
value(f47)
value(f48)
value(f49)
value(f50)
value(f51)
value(f52)
value(f53)
value(f54)
value(f55)
value(f56)
value(f57)
value(f58)
value(f59)
value(f60)
value(f61)
value(f62)
value(f63)
value(f64)
value(f65)
value(f66)
value(f67)
value(f68)
value(f69)
value(f70)
value(f71)
value(f72)

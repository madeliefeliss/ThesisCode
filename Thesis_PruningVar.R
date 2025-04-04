# Seed:
{
  
set.seed(3568970)  
fixed_seeds <- sample(1:1e6, 1000)  
saveRDS(fixed_seeds, "fixed_seeds.rds")

}

# Loading the fixed seeds:
fixed_seeds <- readRDS("fixed_seeds.rds")

# Categorical:
{
  
# 1:
{

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

}

# 2:
{
  
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

}

# 3:
{
  
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

}

# 4:
{
  
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

}

# 5:
{
  
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

}

# 6:
{
  
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

}

# 7:
{
  
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

}

# 8:
{
  
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

}

# 9:
{
  
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

}

# 10:
{
  
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

}

# 11:
{
  
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

}

# 12:
{
  
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

}

# 13:
{
  
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

}

# 14:
{
  
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
saveRDS(moderator_counts, "Results/CAT_T2_d0.3_c0.3_p0.5_moderator_counts.rds")
saveRDS(terminal_node_counts, "Results/CAT_T2_d0.3_c0.3_p0.5_terminal_node_counts.rds")
saveRDS(cptable_list, "Results/CAT_T2_d0.3_c0.3_p0.5_cptable_list.rds")

}

# 15:
{
  
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

}

# 16:
{
  
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

}

# 17:
{
  
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

}
  
# 18:
{
  
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

}

# 19:
{
  
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

}

# 20:
{
  
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

}

# 21:
{
  
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

}

# 22:
{
  
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

}

# 23:
{
  
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

}

# 24:
{
  
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

}

}
  
# Continuous:
{
  
# 25:
{
  
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

}

# 26:
{
  
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

}

# 27:
{
  
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

}

# 28:
{
  
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

}

# 29:
{
  
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

}

# 30:
{
  
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

}

# 31:
{
  
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

}

# 32:
{
  
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

}

# 33:
{
  
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

}

# 34:
{
  
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

}

# 35:
{
  
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

}

# 36:
{
  
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

}

# 37:
{
  
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

}

# 38:
{
  
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

}

# 39:
{
  
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

}

# 40:
{
  
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

}

# 41:
{
  
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

}

# 42:
{
  
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

}

# 43:
{
  
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

}

# 44:
{
  
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

}

# 45:
{
  
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

}

# 46:
{
  
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

}

# 47:
{
  
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

}

# 48:
{
  
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

}

}
  
# Unbalanced Categorical:
{
  
# 49:
{
  
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

}

# 50:
{
  
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

}

# 51:
{
  
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

}

# 52:
{
  
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

}

# 53:
{
  
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

}

# 54:
{
  
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

}

# 55:
{
  
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

}

# 56:
{
  
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

}

# 57:
{
  
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

}

# 58:
{
  
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

}
  
# 59:
{
  
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

}

# 60:
{
  
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

}

# 61:
{
  
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

}

# 62:
{
  
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

}

# 63:
{
  
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

}

# 64:
{
  
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

}

# 65:
{
  
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

}

# 66:
{
  
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

}

# 67:
{
  
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

}

# 68:
{
  
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

}

# 69:
{
  
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

}

# 70:
{
  
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

}

# 71:
{
  
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

}

# 72:
{
  
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

}

}





# Preparation:
{

set.seed(3568970) 
  
# Package:
library(metacart)

# Defining the parameters:
K <- 120  # Number of studies
Kt <- 2000  # Number of test data studies
n.bar <- 80  # Average sample size per study
tau <- sqrt(0.025)  # Between-study sd
mods <- 10  # Number of moderators
tree <- 1:2  # Tree structure options
delta <- c(0.3, 0.5)  # Effect size
r <- c(0.3, 0.5, 0.7)  # Correlation between moderators

# Creating all combinations of parameters:
cons <- expand.grid(K, n.bar, tau, mods, tree, delta, r)
colnames(cons) <- c("K", "n.bar", "tau", "mods", "tree", "delta", "cor")

# Defining true moderators for each tree structure:
truemods <- list(
  c("m1", "m5"),
  c("m2", "m3", "m4")
)

# Loading the required simulation functions:
source("proj4_SimFuncsSSSV2.R")

}

# Loop for continuous sets:
{
  
# Loop through each combination:
for (RowOfDesign in 1:nrow(cons)) {
  
  # Extracting parameter settings for the 
  # current design row:
  cons_row <- cons[RowOfDesign,]
  m <- cons_row$mods
  K <- cons_row$K
  tree <- cons_row$tree
  tau <- cons_row$tau
  n.bar <- cons_row$n.bar
  delta <- cons_row$delta
  r <- cons_row$cor
  
  # Setting seed for reproducibility:
  set.seed(3568970)  
  
  # Generating moderator data:
  ms.data <- gen.uni.mods(r = r, m = m, K = K)
  
  # Defining true effect sizes based on the tree structure:
  if (tree == 1) { 
    y.true <- delta * (ms.data[, "m1"] > 0.3333) * (ms.data[, "m5"] > 0.3333)  
  } else if (tree == 2) { 
    y.true <- delta * (ms.data[, "m2"] > 0.3333) +
      ((ms.data[, "m3"] <= 0.6666) * (ms.data[, "m4"] > 0.3333)) 
  } else if (tree == 3) { 
    y.true <- delta * (ms.data[, "m2"] > 0.6666) + delta *
      (((ms.data[, "m3"] < 0.3333) * (ms.data[, "m4"] < 0.3333)) * (ms.data[, "m2"] <= 0.6666))
  } 
  
  
  # Generating dataset with effect sizes:
  dat <- cbind(SimData(y.true, K, NumGen(n.bar, K), tau), ms.data)

  # Saving the dataset:
  filename <- paste0("datasetCON_T", tree, "_delta", delta, "_cor", r, ".csv")
  write.csv(dat, file = filename, row.names = FALSE)

  }

}

# Loop for categorical sets:
{
  
# Loop through each combination:
for (RowOfDesign in 1:nrow(cons)) {
  
  # Extracting parameter settings for the current 
  # design row:
  cons_row <- cons[RowOfDesign,]
  m <- cons_row$mods
  K <- cons_row$K
  tree <- cons_row$tree
  tau <- cons_row$tau
  n.bar <- cons_row$n.bar
  delta <- cons_row$delta
  r <- cons_row$cor
  
  # Setting seed for reproducibility:
  set.seed(3568970) 
  
  # Generating moderator data:
  ms.data <- gen.uni.mods(r = r, m = m, K = K)
  
  # Converting m1 to m5 into categorical variables:
  for (mod in c("m1", "m2", "m3", "m4", "m5")) {
    
    ms.data[, mod] <- ifelse(ms.data[, mod] <= 0.3333, "A", 
                             ifelse(ms.data[, mod] <= 0.6666, "B", "C"))
  }
  
  # Defining true effect sizes based on the tree structure:
  if (tree == 1) { 
    y.true <- delta * (ms.data[, "m1"] %in% c("B", "C")) * (ms.data[, "m5"] %in% c("B", "C"))
  } else if (tree == 2) { 
    y.true <- delta *  (ms.data[, "m2"] %in% c("B", "C")) + 
      ((ms.data[, "m3"] %in% c("A", "B")) * 
      (ms.data[, "m4"] %in% c("B", "C")))
  } 
  
  # Generating dataset with effect sizes:
  dat <- cbind(SimData(y.true, K, NumGen(n.bar, K), tau), ms.data)
  
  # Saving the dataset:
  filename <- paste0("datasetCAT_T", tree, "_delta", delta, "_cor", r, ".csv")
  write.csv(dat, file = filename, row.names = FALSE)

  }

}

# Loop for unbalanced categorical sets:
{
  
# Loop through each combination:
for (RowOfDesign in 1:nrow(cons)) {
  
  # Extracting parameter settings for the current 
  # design row:
  cons_row <- cons[RowOfDesign,]
  m <- cons_row$mods
  K <- cons_row$K
  tree <- cons_row$tree
  tau <- cons_row$tau
  n.bar <- cons_row$n.bar
  delta <- cons_row$delta
  r <- cons_row$cor
  
  # Setting seed for reproducibility:
  set.seed(3568970) 
  
  # Generating moderator data:
  ms.data <- gen.uni.mods(r = r, m = m, K = K)
  
  # Converting m1 to m5 into categorical variables:
  for (mod in c("m1", "m2", "m3", "m4", "m5")) {
    
    ms.data[, mod] <- ifelse(ms.data[, mod] <= 0.2222, "A", 
                             ifelse(ms.data[, mod] <= 0.8888, "B", "C"))
  }
  
  # Defining true effect sizes based on the tree structure:
  if (tree == 1) { 
    y.true <- delta * (ms.data[, "m1"] %in% c("B", "C")) * (ms.data[, "m5"] %in% c("B", "C"))
  } else if (tree == 2) { 
    y.true <- delta *  (ms.data[, "m2"] %in% c("B", "C")) + 
      ((ms.data[, "m3"] %in% c("A", "B")) * 
      (ms.data[, "m4"] %in% c("B", "C")))
  } 
  
  # Generating dataset with effect sizes:
  dat <- cbind(SimData(y.true, K, NumGen(n.bar, K), tau), ms.data)
  
  # Saving the dataset:
  filename <- paste0("datasetUBCAT_T", tree, "_delta", delta, "_cor", r, ".csv")
  write.csv(dat, file = filename, row.names = FALSE)

  }

}

# Analysis example:
{
  
# Performing RE meta-CART analysis:
library(metacart)
set.seed(3568970)
  
dat <- read.csv("Datasets/datasetCAT_T1_delta0.3_cor0.csv")

resRE1 <- REmrt(
  efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
  vi = vark, data = dat, c.pruning = 0)

# Evaluate the results from RE meta-CART with c = 0:
if (length(resRE1$g) > 1) { # resRE1$g = amount of terminal nodes
  
  # Extra info:
  {
    
  # 1. length(resRE1$moderators)
  # Counts the number of moderators (variables) selected by the meta-CART model.
  # It tells you how many variables the tree decided were important for 
  # explaining variability in effect sizes.
  
  # 2. sum(truemods[[tree]] %in% resRE1$moderators)
  # Checks how many of the true moderators (from your 
  # simulation) were identified by the tree model.
  # truemods[[tree]] contains the true moderators.
  # resRE1$moderators contains the moderators chosen by the model.
  # sum(... %in% ...) counts how many of the true moderators were found.
  # This tells you how well the tree "recovered" the actual moderators. 
  # If the number is high, it means the tree did a good job 
  # identifying the correct variables.
  
  # 3. length(resRE1$n)
  # Counts the number of terminal nodes in the tree.
  # Why it's important: Terminal nodes represent the final 
  # subgroups formed by the tree. More nodes usually mean the tree 
  # is dividing the data into smaller and more specific groups.
  
  # 4. resRE1$Qb
  # Represents the between-study heterogeneity 
  # explained by the tree's moderators.
  # Qb is a statistic measuring how much variability in the effect sizes 
  # is explained by the splits in the tree.
  # A higher Qb means the tree is good at explaining variability in the 
  # data, which is the goal of the meta-CART analysis.
  
  # 5. resRE1$pval.Qb
  # Tests whether the heterogeneity explained (Qb) is statistically significant.
  # A small p-value (e.g., < 0.05) suggests the moderators selected by 
  # the tree significantly explain variability.
  # It helps confirm whether the moderators are truly meaningful or if the splits are random.
  
  }
  
  res.recoveryRE1 <- c(
    length(resRE1$moderators),
    sum(truemods[[tree]] %in% resRE1$moderators),
    length(resRE1$n),
    resRE1$Qb,
    resRE1$pval.Qb)
  
  res.recoveryRE1
  
}

resRE2 <- REmrt(
  efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
  vi = vark, data = dat, c.pruning = 0.5)

plot(resRE2)

# Evaluate the results from RE meta-CART with c = 0.5:
if (length(resRE2$g) > 1) { # resRE1$g = amount of terminal nodes
  
  # Extra info:
  {
    
    # 1. length(resRE2$moderators)
    # Counts the number of moderators (variables) selected by the meta-CART model.
    # It tells you how many variables the tree decided were important for 
    # explaining variability in effect sizes.
    
    # 2. sum(truemods[[tree]] %in% resRE2$moderators)
    # Checks how many of the true moderators (from your 
    # simulation) were identified by the tree model.
    # truemods[[tree]] contains the true moderators.
    # resRE2$moderators contains the moderators chosen by the model.
    # sum(... %in% ...) counts how many of the true moderators were found.
    # This tells you how well the tree "recovered" the actual moderators. 
    # If the number is high, it means the tree did a good job 
    # identifying the correct variables.
    
    # 3. length(resRE2$n)
    # Counts the number of terminal nodes in the tree.
    # Why it's important: Terminal nodes represent the final 
    # subgroups formed by the tree. More nodes usually mean the tree 
    # is dividing the data into smaller and more specific groups.
    
    # 4. resRE2$Qb
    # Represents the between-study heterogeneity 
    # explained by the tree's moderators.
    # Qb is a statistic measuring how much variability in the effect sizes 
    # is explained by the splits in the tree.
    # A higher Qb means the tree is good at explaining variability in the 
    # data, which is the goal of the meta-CART analysis.
    
    # 5. resRE2$pval.Qb
    # Tests whether the heterogeneity explained (Qb) is statistically significant.
    # A small p-value (e.g., < 0.05) suggests the moderators selected by 
    # the tree significantly explain variability.
    # It helps confirm whether the moderators are truly meaningful or if the splits are random.
    
  }
  
  res.recoveryRE2 <- c(
    length(resRE2$moderators),
    sum(truemods[[tree]] %in% resRE2$moderators),
    length(resRE2$n),
    resRE2$Qb,
    resRE2$pval.Qb)
  
  res.recoveryRE2
  
}

# Control parameters for tree growth:
{
# c.pruning = 0: No pruning is applied during the tree construction.
# maxL = 20L: The maximum depth of the tree is 20 levels.
# minsplit = 6L: Minimum number of studies required to consider a split.
# cp = 1e-04: Complexity parameter that controls the minimum improvement in fit required for a split.
# minbucket = 3L: Minimum number of studies in a terminal node.
# xval = 10: Number of cross-validations.
# lookahead = FALSE: No lookahead splitting (only considers immediate splits).
# perm = 500: Number of permutations for hypothesis testing.
}

}


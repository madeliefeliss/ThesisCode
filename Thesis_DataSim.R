#Seed:
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

# Loading the required simulation functions:
source("proj4_SimFuncsSSSV2.R")

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
    y.true <- delta * (ms.data[, "m2"] > 0.6666) + delta *
      (((ms.data[, "m3"] > 0.3333) * (ms.data[, "m4"] > 0.3333)) * (ms.data[, "m2"] <= 0.6666))
  } 
  
  # Generating dataset with effect sizes:
  dat <- cbind(SimData(y.true, K, NumGen(n.bar, K), tau), ms.data, y.true)

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
    y.true <- delta * (ms.data[, "m2"] == "C") + delta *
      (ms.data[, "m3"] %in% c("B", "C")) * (ms.data[, "m4"]%in% c("B", "C")) * (ms.data[, "m2"] %in% c("A", "B"))
  }
  
  # Generating dataset with effect sizes:
  dat <- cbind(SimData(y.true, K, NumGen(n.bar, K), tau), ms.data, y.true)
  
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
    y.true <- delta * (ms.data[, "m2"] == "C") + delta *
      (ms.data[, "m3"] %in% c("B", "C")) * (ms.data[, "m4"]%in% c("B", "C")) * (ms.data[, "m2"] %in% c("A", "B"))
  }
  
  # Generating dataset with effect sizes:
  dat <- cbind(SimData(y.true, K, NumGen(n.bar, K), tau), ms.data, y.true)
  
  # Saving the dataset:
  filename <- paste0("datasetUBCAT_T", tree, "_delta", delta, "_cor", r, ".csv")
  write.csv(dat, file = filename, row.names = FALSE)

  }

}



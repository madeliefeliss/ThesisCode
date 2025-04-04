# Original:
{
  
args <- commandArgs(TRUE)
args <- as.numeric(args)


library(metacart)


num_iter <- 1000

RowOfDesign <- 1#ceiling(args[1]/num_iter) #i in Xinru's script
Replication <- args[1]#(args[1]-1)%%num_iter+1 #j in xinru's script


K <- c(40, 120)
Kt <- 2000 
n.bar <- 80
tau <- c(0, sqrt(0.025), sqrt(0.05))
mods <- 10
tree <- 1:5
delta <- 0.5
r <- 0.3
cons <- expand.grid(K, n.bar, tau, mods, tree, delta, r)
colnames(cons) <- c("K", "n.bar", "tau", "mods", "tree", "delta",
                    "cor")
truemods <- list()
truemods[[1]] <- "m101"
truemods[[2]] <- c("m1", "m5")
truemods[[3]] <- c("m1", "m5")
truemods[[4]] <- c("m1", "m5")
truemods[[5]] <- c("m1", "m2", "m3", "m4", "m5")

res.recovery <- rep(NA,7) #array(NA, dim = c(6, num_iter))

#set.seed(Replication + num_iter*RowOfDesign)

#Parameters

cons_row<-cons[RowOfDesign,]

m <- cons_row$mods
K <- cons_row$K
tree <- cons_row$tree
tau <- cons_row$tau
n.bar <- cons_row$n.bar
tr <- cons_row$tree
delta <- cons_row$delta
r <- cons_row$cor

#Generate data
source("proj4_SimFuncsSSSV2.R")
set.seed(Replication + num_iter*RowOfDesign)
ms.data <- gen.uni.mods(r = r, m = m, K = K)
if (tree == 1){ 
  y.true <- rep(delta, K)
}else if (tree == 2){ 
  y.true <- delta*(ms.data[ ,"m1"] > 0.5)* (ms.data[ ,"m5"] > 0.5)
}else if (tree == 3){ 
  y.true  <- (ms.data[,"m1"] <= 0.5) * (ms.data[,"m5"] > 0.5) * delta + (ms.data[,"m1"] > 0.5) * (ms.data[,"m5"] <= 0.5) * delta
}else if (tree == 4){ 
  y.true <- -delta + delta*ms.data[ ,"m1"] + delta*ms.data[ ,"m5"]
}else if (tree == 5){ 
  y.true <- (-15 + 10*sin(pi*ms.data[,'m1']*ms.data[,'m2']) + 20*(ms.data[,'m3']-0.5)^2 + 10*ms.data[,'m4'] + 5*ms.data[,'m5'])*0.1
}
set.seed(Replication + num_iter*RowOfDesign)
dat <- cbind(SimData(y.true, K, NumGen(n.bar,K), tau), ms.data)
#dat1000[[j]] <- dat

K_test = 2000
set.seed(Replication + num_iter*RowOfDesign)
ms.data <- gen.uni.mods(r = r, m = m, K = K_test)
if (tree == 1){ 
  y.true <- rep(delta, K_test)
}else if (tree == 2){ 
  y.true <- delta*(ms.data[ ,"m1"] > 0.5)* (ms.data[ ,"m5"] > 0.5)
}else if (tree == 3){ 
  y.true  <- (ms.data[,"m1"] <= 0.5) * (ms.data[,"m5"] > 0.5) * delta + (ms.data[,"m1"] > 0.5) * (ms.data[,"m5"] <= 0.5) * delta
}else if (tree == 4){ 
  y.true <- -delta + delta*ms.data[ ,"m1"] + delta*ms.data[ ,"m5"]
}else if (tree == 5){ 
  y.true <- (-15 + 10*sin(pi*ms.data[,'m1']*ms.data[,'m2']) + 20*(ms.data[,'m3']-0.5)^2 + 10*ms.data[,'m4'] + 5*ms.data[,'m5'])*0.1
}  
set.seed(Replication + num_iter*RowOfDesign)
dat_test <- cbind(SimData(y.true, K_test, NumGen(n.bar,K_test), tau), ms.data)



# Load data
# filename <- paste("./data/proj4_data", "K",K,"n",n.bar,"tau",tau,"mods",m,"tree",tree, "delta", delta,
#                   "cor", r, sep="")
# filename_test <-  paste("./data/proj4_test_data", "K",K,"n",n.bar,"tau",tau,"mods",m,"tree",tree, "delta", delta,
#                         "cor", r, sep="")
# load(filename)
# load(filename_test)
res.nodes <- c()#array(NA, dim = c(K, 1))

#dat <- dat1000[[1]] #dat1000[[Replication]]
set.seed(Replication + num_iter*RowOfDesign)
resRE1 <-REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10, 
               vi = vark, data = dat, c.pruning = 0, sss = TRUE,
               maxL = 20L, minsplit = 6L, cp = 1e-04,
               minbucket = 3L, xval = 10, lookahead = TRUE, perm = 500)


if(length(resRE1$g)>1){
predY3 <- predict(resRE1, dat_test)[,1]
pred.error3 <- sqrt(mean((predY3 -dat_test$efk)^2))

# the number of moderators
res.recovery[1] <- length(resRE1$moderators)
# the correct number of moderators
res.recovery[2] <- sum(truemods[[tree]]%in% resRE1$moderators)
# tree size
res.recovery[3] <- length(resRE1$n)
# Q!!!!!!!!!!
if(length(resRE1$n) > 1) res.recovery[4] <- resRE1$Qb
# pQm
if(length(resRE1$n) > 1) res.recovery[5] <- resRE1$pval.Qb
if(length(resRE1$n) > 1) res.recovery[6] <- resRE1$pval.perm
# prediction error
res.recovery[7] <- pred.error3
# the node membership
if(length(resRE1$n) > 1) res.nodes <- resRE1$data$term.node
}




res.nodes0 <- c()
res.recovery0 <- rep(NA,7)

set.seed(Replication + num_iter*RowOfDesign)
resRE0 <-REmrt(efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10, 
               vi = vark, data = dat, c.pruning = 0, sss = FALSE,
               maxL = 20L, minsplit = 6L, cp = 1e-04,
               minbucket = 3L, xval = 10, lookahead = FALSE, perm = 500)


if(length(resRE0$g)>1){
  predY30 <- predict(resRE0, dat_test)[,1]
  pred.error30 <- sqrt(mean((predY30 -dat_test$efk)^2))
  
  # the number of moderators
  res.recovery0[1] <- length(resRE0$moderators)
  # the correct number of moderators
  res.recovery0[2] <- sum(truemods[[tree]]%in% resRE0$moderators)
  # tree size
  res.recovery0[3] <- length(resRE0$n)
  # Q!!!!!!!!!!
  if(length(resRE0$n) > 1) res.recovery0[4] <- resRE0$Qb
  # pQm
  if(length(resRE0$n) > 1) res.recovery0[5] <- resRE0$pval.Qb
  if(length(resRE0$n) > 1) res.recovery0[6] <- resRE0$pval.perm
  # prediction error
  res.recovery0[7] <- pred.error30
  # the node membership
  if(length(resRE0$n) > 1) res.nodes0 <- resRE0$data$term.node
}


save(res.recovery, res.nodes, res.recovery0, res.nodes0, file = paste("./outputRRfinal/proj_4_fit_con", RowOfDesign, Replication, sep ="_"))

}

# Mine with comments:
{
  
# Preparation:
{
  
# Retrieving command-line arguments and convert them to numeric:
args <- commandArgs(TRUE)
args <- as.numeric(args)

# Package:
library(metacart)

# Defining the number of iterations per design:
num_iter <- 1000

# Row of design matrix (determined by input arguments):
RowOfDesign <- 8  # Design row index (could be derived from args[1] and num_iter)
Replication <- args[1]  # Replication index (from the command-line argument)
Replication <- 10

# Defining the simulation parameters:
K <- c(40, 120)  # Number of studies (small or large)
Kt <- 2000  # Number of test data studies
n.bar <- 80  # Average sample size per study
tau <- c(0, sqrt(0.025), sqrt(0.05))  # Between-study variance (heterogeneity)
mods <- 10  # Number of moderators
tree <- 1:5  # Tree structure options
delta <- 0.5  # Effect size
r <- 0.3  # Correlation between moderators

# Creating all combinations of parameters:
cons <- expand.grid(K, n.bar, tau, mods, tree, delta, r)
colnames(cons) <- c("K", "n.bar", "tau", "mods", "tree", "delta", "cor")

# Defining true moderators for each tree structure:
truemods <- list(
  "m101",
  c("m1", "m5"),
  c("m1", "m5"),
  c("m1", "m5"),
  c("m1", "m2", "m3", "m4", "m5")
)

truemods

# Tree 1: Constant Effect Size
# All studies have the same true effect size (delta).

# Tree 2: Interaction Between Two Moderators
# The true effect size (delta) is applied only when both 
# m1 and m5 are greater than 0.5.

# Tree 3: Conditional Interaction
# The true effect size depends on specific combinations of 
# the moderators m1 and m5:
# If m1 <= 0.5 and m5 > 0.5, effect size = delta.
# If m1 > 0.5 and m5 <= 0.5, effect size = delta.
# Otherwise, effect size = 0.

# Tree 4: Linear Effect
# The true effect size is a linear combination of moderators m1 and m5.
# Formula: y.true = -delta + delta * m1 + delta * m5

# Tree 5: Complex Nonlinear Relationship
# The true effect size is determined by a nonlinear combination of 
# multiple moderators (m1, m2, m3, m4, m5):
# Includes sinusoidal, polynomial, and linear terms.
# Formula: y.true <- (-15 + 10 * sin(pi * ms.data[, "m1"] * ms.data[, "m2"]) +
# 20 * (ms.data[, "m3"] - 0.5)^2 + 10 * ms.data[, "m4"] + 5 * ms.data[, "m5"]) * 0.1

}
  
# Training data:
{
  
# Initialize recovery results
res.recovery <- rep(NA, 7)

# Extract parameter settings for the current design row
cons_row <- cons[RowOfDesign,]
m <- cons_row$mods
K <- cons_row$K
tree <- cons_row$tree
tau <- cons_row$tau
n.bar <- cons_row$n.bar
tr <- cons_row$tree
delta <- cons_row$delta
r <- cons_row$cor

# Load the required simulation functions (defined externally)
source("proj4_SimFuncsSSSV2.R")

# Set seed for reproducibility
set.seed(Replication + num_iter * RowOfDesign)

# Generate moderator data for training
ms.data <- gen.uni.mods(r = r, m = m, K = K)

# Define true effect sizes based on the tree structure
if (tree == 1) { 
  y.true <- rep(delta, K)  # Constant effect size
} else if (tree == 2) { 
  y.true <- delta * (ms.data[, "m1"] > 0.5) * (ms.data[, "m5"] > 0.5)
} else if (tree == 3) { 
  y.true <- (ms.data[, "m1"] <= 0.5) * (ms.data[, "m5"] > 0.5) * delta +
    (ms.data[, "m1"] > 0.5) * (ms.data[, "m5"] <= 0.5) * delta
} else if (tree == 4) { 
  y.true <- -delta + delta * ms.data[, "m1"] + delta * ms.data[, "m5"]
} else if (tree == 5) { 
  y.true <- (-15 + 10 * sin(pi * ms.data[, "m1"] * ms.data[, "m2"]) +
               20 * (ms.data[, "m3"] - 0.5)^2 + 10 * ms.data[, "m4"] +
               5 * ms.data[, "m5"]) * 0.1
}

# Generate training dataset with effect sizes
dat <- cbind(SimData(y.true, K, NumGen(n.bar, K), tau), ms.data)

View(dat)

}

# Testing data:
{
  
# Generate moderator data for testing
K_test <- 2000
ms.data <- gen.uni.mods(r = r, m = m, K = K_test)

# Define true effect sizes for testing based on the tree structure
if (tree == 1) { 
  y.true <- rep(delta, K_test)
} else if (tree == 2) { 
  y.true <- delta * (ms.data[, "m1"] > 0.5) * (ms.data[, "m5"] > 0.5)
} else if (tree == 3) { 
  y.true <- (ms.data[, "m1"] <= 0.5) * (ms.data[, "m5"] > 0.5) * delta +
    (ms.data[, "m1"] > 0.5) * (ms.data[, "m5"] <= 0.5) * delta
} else if (tree == 4) { 
  y.true <- -delta + delta * ms.data[, "m1"] + delta * ms.data[, "m5"]
} else if (tree == 5) { 
  y.true <- (-15 + 10 * sin(pi * ms.data[, "m1"] * ms.data[, "m2"]) +
               20 * (ms.data[, "m3"] - 0.5)^2 + 10 * ms.data[, "m4"] +
               5 * ms.data[, "m5"]) * 0.1
}

# Generate testing dataset with effect sizes
dat_test <- cbind(SimData(y.true, K_test, NumGen(n.bar, K_test), tau), ms.data)

}

# Analysis:
{
  
# Perform meta-CART analysis with lookahead splitting
set.seed(Replication + num_iter * RowOfDesign)

resRE1 <- REmrt(
  efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
  vi = vark, data = dat, c.pruning = 0, sss = F,
  maxL = 20L, minsplit = 6L, cp = 1e-04,
  minbucket = 3L, xval = 10, lookahead = F, perm = 500
)
resRE1$cv.res
plot(resRE1)

# Evaluate the results from meta-CART with lookahead splitting
if (length(resRE1$g) > 1) {
  predY3 <- predict(resRE1, dat_test)[, 1]
  pred.error3 <- sqrt(mean((predY3 - dat_test$efk)^2))
  
  res.recovery[1] <- length(resRE1$moderators)  # Number of selected moderators
  res.recovery[2] <- sum(truemods[[tree]] %in% resRE1$moderators)  # Correct moderators
  res.recovery[3] <- length(resRE1$n)  # Tree size
  res.recovery[4] <- resRE1$Qb  # Q statistic for model
  res.recovery[5] <- resRE1$pval.Qb  # p-value for Qb
  res.recovery[6] <- resRE1$pval.perm # Permutation p-value
  res.recovery[7] <- pred.error3  # Prediction error
}

res.recovery

# Perform meta-CART analysis without lookahead splitting
set.seed(Replication + num_iter * RowOfDesign)

resRE0 <- REmrt(
  efk ~ m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10,
  vi = vark, data = dat, c.pruning = 0, sss = FALSE,
  maxL = 20L, minsplit = 6L, cp = 1e-04,
  minbucket = 3L, xval = 10, lookahead = FALSE, perm = 500
)

# Control parameters for tree growth:
# c.pruning = 0: No pruning is applied during the tree construction.
# maxL = 20L: The maximum depth of the tree is 20 levels.
# minsplit = 6L: Minimum number of studies required to consider a split.
# cp = 1e-04: Complexity parameter that controls the minimum improvement in fit required for a split.
# minbucket = 3L: Minimum number of studies in a terminal node.
# xval = 10: Number of cross-validations.
# lookahead = FALSE: No lookahead splitting (only considers immediate splits).
# perm = 500: Number of permutations for hypothesis testing.

# Evaluate the results from meta-CART without lookahead splitting
if (length(resRE0$g) > 1) { # resRE0$g = amount of terminal nodes
  predY30 <- predict(resRE0, dat_test)[, 1]
  pred.error30 <- sqrt(mean((predY30 - dat_test$efk)^2))
  
  # Breaking Down res.recovery0
  
  # 1. length(resRE0$moderators)
  # Counts the number of moderators (variables) selected by the meta-CART model.
  # It tells you how many variables the tree decided were important for 
  # explaining variability in effect sizes.

  # 2. sum(truemods[[tree]] %in% resRE0$moderators)
  # Checks how many of the true moderators (from your 
  # simulation) were identified by the tree model.
  # truemods[[tree]] contains the true moderators.
  # resRE0$moderators contains the moderators chosen by the model.
  # sum(... %in% ...) counts how many of the true moderators were found.
  # This tells you how well the tree "recovered" the actual moderators. 
  # If the number is high, it means the tree did a good job 
  # identifying the correct variables.
  
  # 3. length(resRE0$n)
  # Counts the number of terminal nodes in the tree.
  # Why it's important: Terminal nodes represent the final 
  # subgroups formed by the tree. More nodes usually mean the tree 
  # is dividing the data into smaller and more specific groups.

  # 4. resRE0$Qb
  # Represents the between-study heterogeneity 
  # explained by the tree's moderators.
  # Qb is a statistic measuring how much variability in the effect sizes 
  # is explained by the splits in the tree.
  # A higher Qb means the tree is good at explaining variability in the 
  # data, which is the goal of the meta-CART analysis.

  # 5. resRE0$pval.Qb
  # Tests whether the heterogeneity explained (Qb) is statistically significant.
  # A small p-value (e.g., < 0.05) suggests the moderators selected by 
  # the tree significantly explain variability.
  # It helps confirm whether the moderators are truly meaningful or if the splits are random.
  
  # 6. resRE0$pval.perm
  # A permutation test p-value to check whether the tree's splits are 
  # significant compared to random splits.
  # Lower values indicate the splits are meaningful.
  # This provides additional evidence that the tree's moderators are not due to chance.

  # 7. pred.error30
  # Calculates the prediction error (RMSE) on test data.
  # RMSE measures how far the predicted effect sizes (predY30) are from 
  # the actual effect sizes (dat_test$efk).
  # This tells you how well the tree model generalizes to new data. 
  # A lower RMSE means better predictions.
  
  res.recovery0 <- c(
    length(resRE0$moderators),
    sum(truemods[[tree]] %in% resRE0$moderators),
    length(resRE0$n),
    resRE0$Qb,
    resRE0$pval.Qb,
    resRE0$pval.perm,
    pred.error30
  )
  
res.recovery0

}

# Save results to a file
save(
  res.recovery, res.nodes, res.recovery0, res.nodes0,
  file = paste("./outputRRfinal/proj_4_fit_con", RowOfDesign, Replication, sep = "_")
)

}
  
}
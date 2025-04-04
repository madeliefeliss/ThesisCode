# Thesis

# Packages:
library(rpart)
library(rpart.plot)
library(rattle)
library(metacart)

# Seed:
set.seed(3568970)

# Data:    
data("car.test.frame")
car_data <- car.test.frame 
?car.test.frame
View(car.test.frame)

# Regression and classification and pruning examples:
{

# Regression tree:
{
  
car_tree <- rpart(Price ~ Country + Mileage + Weight + Disp. + HP + Reliability + Type, 
                  data = car.test.frame, cp = 0)

car_tree

summary(car_tree)

par(xpd = TRUE)
plot(car_tree, compress = T)
text(car_tree, splits = TRUE, label, FUN = text, all = T,
     pretty = NULL, digits = getOption("digits") - 3, use.n = T,
     fancy = T, fwidth = 0.8, fheight = 0.8, bg = par("bg"),
     minlength = 1L)

# Custom function to display MSE at each node:
custom_node <- function(x, labs, digits, varlen) {
  
  # Mean Squared Error = deviance / number of observations:
  mse_values <- round(x$frame$dev / x$frame$n, 2)
  
  # Append MSE to the default label:
  paste0(labs, "\nMSE: ", mse_values)  

  }

rpart.plot(
  car_tree,
  #node.fun = custom_node,
  type = 2,                  # Use type 2 for a split-label layout
  digits = -2,                # Show integers instead of scientific notation
  box.palette = "Blues",     # Apply a color gradient to the nodes
  fallen.leaves = TRUE,      # Spread terminal nodes at the bottom
  tweak = 1.2                # Adjust font size for better readability
  
  )

}
  
# Regression tree pruning:
{
  
car_tree$cptable
  
plotcp(car_tree, cex.axis = 0.7)

car_tree_pruned <- prune(car_tree, cp = 0.048)

rpart.plot(
  car_tree_pruned,
  type = 1,                  # Use type 2 for a split-label layout
  digits = -2,                # Show integers instead of scientific notation
  box.palette = "Blues",     # Apply a color gradient to the nodes
  fallen.leaves = TRUE,      # Spread terminal nodes at the bottom
  extra = 100,
  cex = 1.1)

}
  
# Classification tree:
{
  
car_tree2 <- rpart(Type ~ Country + Mileage + Weight + Disp. + HP + Price + Reliability,
                   data = car.test.frame)

car_tree2

summary(car_tree2)

# Custom function to display Gini index at each node:
custom_node2 <- function(x, labs, digits, varlen) {
  
  # Extract Gini index values from the frame:
  gini_values <- round(x$frame$dev / x$frame$wt, 3)
  
  # Append Gini index to node labels:
  paste0(labs, "\nGini: ", gini_values)  

  }

rpart.plot(
  car_tree2,
  node.fun = custom_node2,
  type = 1,                  # Use type 2 for a split-label layout
  digits = -2,                # Show integers instead of scientific notation
  box.palette = "Blues",     # Apply a color gradient to the nodes
  fallen.leaves = TRUE,      # Spread terminal nodes at the bottom
  extra = 100,
  cex = 1.1)

}
  
# Classification tree pruning: 
{
  
car_tree2$cptable
  
plotcp(car_tree2, cex.axis = 0.7)

car_tree3 <- prune(car_tree2, cp = 0.13)

rpart.plot(
  car_tree3,
  type = 1,                  # Use type 2 for a split-label layout
  digits = -2,                # Show integers instead of scientific notation
  box.palette = "Blues",     # Apply a color gradient to the nodes
  fallen.leaves = TRUE,      # Spread terminal nodes at the bottom
  extra = 100,
  cex = 1.1)

}
  
}

# MetaCART example:
{
  
data(SimData)  
View(SimData)
meta_results <- FEmrt(efk ~ m1 + m2 + m3 + m4 + m5, 
                      vi = vark, data = SimData, c.pruning = 0.3)

meta_results2 <- REmrt(efk ~ m1 + m2 + m3 + m4 + m5, 
                      vi = vark, data = SimData, c.pruning = 0.3,
                      lookahead = TRUE, SSS = F)

plot(meta_results2)
summary(meta_results2)
meta_results2$cptable
plotcp(meta_results2)

} 





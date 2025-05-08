#' Summarize Pruning Variability Metrics
#' 
#' Computes diagnostic metrics for pruning stability including entropy of terminal node distribution,
#' mean number of terminal nodes, and standard deviation across iterations.
#'
#' @param x A \code{REmrt} object from the metacart package
#' @param iter Number of pruning iterations to perform (default = 100)
#' @param c.pruning Pruning strictness parameter (default = 0)
#' @return Invisibly returns a list with entropy value, mean terminal nodes, SD of terminal nodes,
#'         and entropy classification. Prints formatted results to console.
#' @export
summary.PV <- function(x, iter = 100, c.pruning = 0) {
  
  # Extracting the model and setting the environment:
  test_model <- x
  environment(test_model$formula) <- environment()
  
  # Creating an empty vector:
  terminal_nodes <- numeric(iter)
  
  # Tracking the terminal node count across iterations:
  for (i in 1:iter) {
    
    # Building new models using the given model:
    new_model <- REmrt(test_model$formula, 
                       data = test_model$data, 
                       vi = test_model$data$`(vi)`, 
                       c.pruning = c.pruning)
    
    # Saving the amount of terminal nodes:
    terminal_nodes[i] <- length(new_model$g)
    
  }
  
  # Calculating the entropy values of the terminal node distribution:
  compute_entropy <- function(counts) { 
    
    # Converting counts to probabilities:
    probs <- counts / sum(counts) 
    
    # Computing the entropy:
    -sum(probs * log(probs, base=2))  
  }
  
  entropy <- compute_entropy(table(terminal_nodes))
    
  # Calculating the mean and the SD of the terminal node counts:
  mean_tn <- mean(terminal_nodes)
  sd_tn <- sd(terminal_nodes)
  
  # Classifying the entropy level:
  entropy_level <- if (entropy < 0.5) {
    "low"
  } else if (entropy <= 1.5) {
    "moderate"
  } else {
    "high"
  }
  
  # Output:
  cat("Pruning Variability Diagnostics\n")
  cat("-------------------------------\n")
  cat(sprintf("Entropy of terminal node distribution: %.2f (%s)\n", 
              entropy, entropy_level))
  cat(sprintf("Mean number of terminal nodes: %.1f\n", mean_tn))
  cat(sprintf("SD of terminal nodes across runs: %.1f\n", sd_tn))
  
}

# Within-study variance, σ2k, sampling variance, vark
# Between-study variance, σ2τ, variation introduced 
# by residual heterogeneity, tau

# Package:
library(mvtnorm)

# Step 1:
{
  
gen.uni.mods <- function(r, m, K) {
  
  # Generates a matrix of multivariate uniformly 
  # distributed random numbers to represent 
  # study-level moderators.
  
  # Inputs:
  # r: The correlation coefficient between moderators. 
  # Transformed into rho using the formula 2*sin(pi*r/6) 
  # which scales r to ensure valid correlation values.
  # m: The number of moderators.
  # K: The number of studies.
  
  rho = 2*sin(pi*r/6)
  
  # Creates a covariance matrix (P) with rho as the 
  # off-diagonal elements and 1s on the diagonal:
  P <- matrix(rho, nrow = m, ncol = m)
  diag(P) <- 1
  
  # Simulates a multivariate normal distribution (samp) 
  # with this covariance matrix:
  samp <- rmvnorm(K, sigma = P)
    
  # Transforms the normal distribution into uniform 
  # distributions using the cumulative probability 
  # function (pnorm):
  U <- pnorm(samp)
   
  # Returns a K * m matrix of uniform random numbers 
  # with column names like m1, m2, ..., m_m:
  colnames(U) <- paste("m", 1:m, sep="")
  
  U

  }

# Example:
x1 = gen.uni.mods(r = 1, m = 5, K = 5)
x1

}

# Step 2:
{
  
NumGen <- function(n, k) {
  
  # Generates realistic within-study sample sizes for 
  # a meta-analysis based on an average sample size.
  
  # Inputs:
  # n: Average sample size per study.
  # k: Number of studies.
  
  # Draws k random numbers from a normal distribution 
  # with a mean of n and a standard deviation of n/3:
  n <- n
  k <- k
  samp <- rnorm(k, n, n/3)
  
  # Rounds the numbers to the nearest integer and replaces 
  # values below 10 with 10 to ensure realistic study 
  # sample sizes:
  samp <- round(samp, 0)  
  samp[samp < 10] <- 10  
  
  # Returns a vector of k sample sizes:
  return(samp)
  
}
  
# Example:
x2 = NumGen(n = 100, k = 5)  
x2

}

# Step 3:
{
  
SimData <- function(y.true, K, n, tau) {
  
  # Simulates a dataset for a meta-analysis, including 
  # study-level effect sizes (efk) and sampling variances (vark).
  
  # Inputs:
  # y.true: The overall true effect size.
  # K: Number of studies.
  # n: Vector of sample sizes for each study.
  # tau: Standard deviation of the true effect sizes (between-study heterogeneity).
  
  # Generates K true effect sizes (deltak) for each study 
  # by sampling from a normal distribution with mean 
  # y.true and standard deviation tau:
  K <- K
  n <- n
  tau <- tau
  deltak <- rnorm(rep(1,K), y.true, tau)  
  
  # Computes a correction factor (cmk), approximating 
  # the scaling effect of the t-distribution:
  cmk <- 1 - 3/(8*n-9)  
  # Approximation for gamma(n - 1) / (sqrt(n - 1) * 
  # gamma((2 * n - 3) / 2))
  
  # Loops through each study:
  # Simulates the observed effect size (efk) using a 
  # non-central t-distribution, adjusted for n and deltak.
  # Computes the sampling variance (vark) using a formula 
  # accounting for the non-central t-distribution and the 
  # true effect size.
  
  efk <- vark <- numeric(K) 
 
  for (k in 1:K) {
    
    # Simulating the Test Statistic:
    # rt() generates a single value from a t-distribution:
    # df = 2 * n[k] - 2: Degrees of freedom for the t-distribution.
    # Each study has a sample size of n[k], so the total degrees of freedom 
    # for a two-group comparison is 2n[k]−2.
    # Non-centrality parameter (NCP) for the t-distribution accounts for the 
    # true effect size (deltak[k]) and sample size (n[k]).
    samp <- rt(1, df = 2*n[k]-2, ncp = deltak[k]*sqrt(n[k]/2))
    
    # Calculating the Effect Size (efk):
    # cmk[k]: Correction factor for the t-statistic (typically used to adjust 
    # for bias in small-sample estimates).
    # sqrt(n[k] / 2): Converts the t-statistic into an effect size (Cohen's d).
    efk[k] <- samp*cmk[k]/sqrt(n[k]/2)
    
    # Calculating the Sampling Variance (vark):
    # This formula calculates the variance of the effect size estimate for the 
    # k-th study.
    # cmk[k]^2: Squared correction factor for bias adjustment.
    # (2 * n[k] - 2): Degrees of freedom for the study.
    # (1 + n[k] * deltak[k]^2 / 2): Adjusts for the true effect size (deltak[k]).
    # ((2 * n[k] - 4) * n[k] / 2): Denominator that depends on sample size and degrees of freedom.
    # - deltak[k]^2: Subtracts the squared true effect size, as this is part of the 
    # variance structure but needs adjustment.
    vark[k] <- cmk[k]^2*(2*n[k]-2)*
      (1+n[k]*deltak[k]^2/2)/((2*n[k]-4)*n[k]/2)-deltak[k]^2  
    
    # Note the vark is the sampling variance but not the sampling error
  
    }
  
  # Returns a data frame with:
  # trail: Study number.
  # efk: Simulated observed effect size for each study.
  # vark: Sampling variance for each study.
  dat   <- data.frame(trail=1:K, efk=efk, vark=vark)
  return(dat)
  
}

# Example:
x3 = SimData(y.true = 5, K = 5, n = x2, tau = 2) 
x3

}


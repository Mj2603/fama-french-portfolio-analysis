# Covariance Matrix Estimation Functions

# Sample Covariance Matrix
estimate_sample_covariance <- function(returns) {
  # Remove rows with any missing values
  returns <- returns[complete.cases(returns), , drop = FALSE]
  
  if (nrow(returns) < 2) {
    stop("Need at least 2 observations to estimate covariance")
  }
  
  # Calculate sample mean
  mean_returns <- colMeans(returns, na.rm = TRUE)
  
  # Center the returns
  centered_returns <- sweep(returns, 2, mean_returns, "-")
  
  # Calculate sample covariance
  n <- nrow(centered_returns)
  cov_matrix <- (t(centered_returns) %*% centered_returns) / (n - 1)
  
  # Ensure symmetry (numerical stability)
  cov_matrix <- (cov_matrix + t(cov_matrix)) / 2
  
  return(cov_matrix)
}

# EWMA Covariance Matrix with Double Decay
estimate_ewma_covariance <- function(returns, corr_halflife = 252, vol_halflife = 126) {
  returns <- returns[complete.cases(returns), , drop = FALSE]
  n_obs <- nrow(returns)
  n_assets <- ncol(returns)
  
  if (n_obs < 2) {
    stop("Need at least 2 observations to estimate covariance")
  }
  
  lambda_corr <- 0.5^(1 / corr_halflife)
  lambda_vol <- 0.5^(1 / vol_halflife)
  
  # Initialize with sample covariance from a reasonable window
  initial_window <- min(n_obs, 252)
  if (initial_window < n_assets) {
    initial_window <- n_assets + 1
  }
  if (initial_window > n_obs) {
    stop("Insufficient observations for initial EWMA covariance estimate.")
  }
  
  ewma_cov <- estimate_sample_covariance(returns[1:initial_window, , drop = FALSE])
  
  # Recursive update for the rest of the observations
  for (t in (initial_window + 1):n_obs) {
    r_t <- as.numeric(returns[t, ])
    
    # Update the full covariance matrix using lambda_corr
    ewma_cov <- lambda_corr * ewma_cov + (1 - lambda_corr) * outer(r_t, r_t)
    
    # Adjust the diagonal elements (variances) using lambda_vol
    for (i in 1:n_assets) {
      ewma_cov[i, i] <- lambda_vol * ewma_cov[i, i] + (1 - lambda_vol) * r_t[i]^2
    }
    
    # Ensure symmetry (numerical stability)
    ewma_cov <- (ewma_cov + t(ewma_cov)) / 2
  }
  
  return(ewma_cov)
}

# Calculate Minimum Eigenvalue
get_min_eigenvalue <- function(cov_matrix) {
  eigenvals <- eigen(cov_matrix, only.values = TRUE)$values
  return(min(Re(eigenvals)))
}

# Calculate Maximum Eigenvalue
get_max_eigenvalue <- function(cov_matrix) {
  eigenvals <- eigen(cov_matrix, only.values = TRUE)$values
  return(max(Re(eigenvals)))
}

# Calculate Condition Number
get_condition_number <- function(cov_matrix) {
  eigenvals <- eigen(cov_matrix, only.values = TRUE)$values
  eigenvals <- Re(eigenvals)
  eigenvals <- eigenvals[eigenvals > 1e-10]  # Remove near-zero eigenvalues
  if (length(eigenvals) == 0) {
    return(Inf)
  }
  return(max(eigenvals) / min(eigenvals))
}


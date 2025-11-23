# Portfolio Optimization Functions

if (!requireNamespace("quadprog", quietly = TRUE)) {
  install.packages("quadprog", repos = "https://cran.r-project.org")
}
library(quadprog)

# Solve Minimum Variance Portfolio
solve_min_variance_portfolio <- function(cov_matrix, max_weight = NULL) {
  n_assets <- nrow(cov_matrix)
  Dmat <- 2 * cov_matrix
  dvec <- rep(0, n_assets)
  
  if (is.null(max_weight)) {
    Amat <- cbind(rep(1, n_assets), diag(n_assets))
    bvec <- c(1, rep(0, n_assets))
    meq <- 1
  } else {
    Amat <- cbind(
      rep(1, n_assets),
      diag(n_assets),
      -diag(n_assets)
    )
    bvec <- c(1, rep(0, n_assets), rep(-max_weight, n_assets))
    meq <- 1
  }
  
  tryCatch({
    result <- solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
    weights <- result$solution
    weights[weights < 1e-10] <- 0
    weights <- weights / sum(weights)
    return(weights)
  }, error = function(e) {
    warning("Portfolio optimization failed, using equal weights")
    return(rep(1/n_assets, n_assets))
  })
}

# Get Equal Weights
get_equal_weights <- function(n_assets) {
  return(rep(1/n_assets, n_assets))
}

# Calculate Portfolio Returns
calculate_portfolio_returns <- function(weights, returns) {
  # Handle NA values in returns
  returns[is.na(returns)] <- 0
  portfolio_returns <- as.numeric(returns %*% weights)
  return(portfolio_returns)
}

# Calculate Annualized Return
calculate_annualized_return <- function(returns, periods_per_year = 12) {
  if (length(returns) == 0) return(0)
  total_return <- prod(1 + returns, na.rm = TRUE) - 1
  n_periods <- length(returns[!is.na(returns)])
  if (n_periods == 0) return(0)
  years <- n_periods / periods_per_year
  if (years <= 0) return(0)
  annualized_return <- (1 + total_return)^(1/years) - 1
  return(annualized_return)
}

# Calculate Annualized Volatility
calculate_annualized_volatility <- function(returns, periods_per_year = 12) {
  if (length(returns) == 0) return(0)
  period_vol <- sd(returns, na.rm = TRUE)
  annualized_vol <- period_vol * sqrt(periods_per_year)
  return(annualized_vol)
}

# Calculate Sharpe Ratio (assuming zero risk-free rate)
calculate_sharpe_ratio <- function(returns, periods_per_year = 12, risk_free_rate = 0) {
  annualized_return <- calculate_annualized_return(returns, periods_per_year)
  annualized_vol <- calculate_annualized_volatility(returns, periods_per_year)
  if (annualized_vol == 0) return(0)
  sharpe_ratio <- (annualized_return - risk_free_rate) / annualized_vol
  return(sharpe_ratio)
}


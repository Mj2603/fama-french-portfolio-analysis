# Part B: Minimum Variance Portfolio Backtest

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cran.r-project.org")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate", repos = "https://cran.r-project.org")
}
library(ggplot2)
library(lubridate)

source("02_covariance_functions.R")
source("03_portfolio_functions.R")

part_b_backtest <- function(daily_data, monthly_data,
                            start_date, end_date,
                            window_length = 252,
                            max_weight = NULL) {
  cat("Part B: Minimum Variance Portfolio Backtest\n")
  cat("Period:", as.character(start_date), "to", as.character(end_date), "\n")
  cat("Window length:", window_length, "days\n")
  if (!is.null(max_weight)) {
    cat("Maximum weight constraint:", max_weight, "\n")
  } else {
    cat("No maximum weight constraint\n")
  }
  
  # Filter monthly data for analysis period
  monthly_analysis <- monthly_data[monthly_data$Date >= start_date & monthly_data$Date <= end_date, ]
  if (nrow(monthly_analysis) == 0) {
    stop("No monthly data available for the specified period")
  }
  
  # Get returns columns
  returns_cols <- setdiff(colnames(monthly_analysis), "Date")
  n_assets <- length(returns_cols)
  
  cat("Number of assets:", n_assets, "\n")
  cat("Number of rebalancing dates:", nrow(monthly_analysis), "\n")
  
  # Initialize storage
  n_months <- nrow(monthly_analysis)
  sample_returns <- numeric(n_months)
  ewma_returns <- numeric(n_months)
  eqw_returns <- numeric(n_months)
  
  sample_weights_list <- list()
  ewma_weights_list <- list()
  
  # Backtest loop
  for (i in 1:n_months) {
    if (i %% 12 == 0) cat("  Processing month", i, "of", n_months, "\n")
    
    month_end <- monthly_analysis$Date[i]
    
    # Get historical daily data up to this month-end
    historical_daily <- daily_data[daily_data$Date <= month_end, ]
    
    if (nrow(historical_daily) < window_length) {
      warning(paste("Insufficient data for", month_end, "- skipping"))
      sample_returns[i] <- 0
      ewma_returns[i] <- 0
      eqw_returns[i] <- 0
      next
    }
    
    # Get last window_length days
    window_data <- tail(historical_daily, window_length)
    returns_matrix <- as.matrix(window_data[, returns_cols])
    
    # Remove columns with all NA
    valid_cols <- colSums(!is.na(returns_matrix)) > 10
    if (sum(valid_cols) < 2) {
      warning(paste("Insufficient valid assets for", month_end, "- skipping"))
      sample_returns[i] <- 0
      ewma_returns[i] <- 0
      eqw_returns[i] <- 0
      next
    }
    
    returns_matrix <- returns_matrix[, valid_cols, drop = FALSE]
    valid_returns_cols <- returns_cols[valid_cols]
    
    # Estimate covariance matrices
    sample_cov <- estimate_sample_covariance(returns_matrix)
    ewma_cov <- estimate_ewma_covariance(returns_matrix, 
                                        corr_halflife = 252, 
                                        vol_halflife = 126)
    
    # Solve for optimal portfolios
    sample_weights <- solve_min_variance_portfolio(sample_cov, max_weight)
    ewma_weights <- solve_min_variance_portfolio(ewma_cov, max_weight)
    
    # Map weights back to full asset set
    sample_weights_full <- numeric(n_assets)
    names(sample_weights_full) <- returns_cols
    sample_weights_full[valid_returns_cols] <- sample_weights
    
    ewma_weights_full <- numeric(n_assets)
    names(ewma_weights_full) <- returns_cols
    ewma_weights_full[valid_returns_cols] <- ewma_weights
    
    # Store weights
    sample_weights_list[[i]] <- sample_weights_full
    ewma_weights_list[[i]] <- ewma_weights_full
    
    # Calculate portfolio returns for this month
    monthly_returns <- as.numeric(monthly_analysis[i, returns_cols, drop = FALSE])
    monthly_returns[is.na(monthly_returns)] <- 0
    
    sample_returns[i] <- sum(sample_weights_full * monthly_returns, na.rm = TRUE)
    ewma_returns[i] <- sum(ewma_weights_full * monthly_returns, na.rm = TRUE)
    
    eqw_w <- get_equal_weights(n_assets)
    eqw_returns[i] <- sum(eqw_w * monthly_returns, na.rm = TRUE)
  }
  
  # Create results dataframe
  results_df <- data.frame(
    Date = monthly_analysis$Date,
    Sample_Port = sample_returns,
    EWMA_Port = ewma_returns,
    EQW_Port = eqw_returns
  )
  
  # Calculate cumulative returns
  results_df$Sample_Port_Cum <- cumprod(1 + results_df$Sample_Port) - 1
  results_df$EWMA_Port_Cum <- cumprod(1 + results_df$EWMA_Port) - 1
  results_df$EQW_Port_Cum <- cumprod(1 + results_df$EQW_Port) - 1
  
  # Calculate performance metrics
  metrics <- data.frame(
    Portfolio = c("Sample.Port", "EWMA.Port", "EQW.Port"),
    Annualized_Return = c(
      calculate_annualized_return(sample_returns, 12),
      calculate_annualized_return(ewma_returns, 12),
      calculate_annualized_return(eqw_returns, 12)
    ),
    Annualized_Volatility = c(
      calculate_annualized_volatility(sample_returns, 12),
      calculate_annualized_volatility(ewma_returns, 12),
      calculate_annualized_volatility(eqw_returns, 12)
    ),
    Sharpe_Ratio = c(
      calculate_sharpe_ratio(sample_returns, 12, 0),
      calculate_sharpe_ratio(ewma_returns, 12, 0),
      calculate_sharpe_ratio(eqw_returns, 12, 0)
    )
  )
  
  cat("\nPerformance Metrics:\n")
  print(metrics)
  
  # Create output directory
  if (!dir.exists("output")) {
    dir.create("output")
  }
  
  # Plot 1: Cumulative Returns
  max_weight_suffix <- ifelse(is.null(max_weight), "", 
                              paste0("_maxweight", max_weight))
  
  p1 <- ggplot(results_df, aes(x = Date)) +
    geom_line(aes(y = Sample_Port_Cum, color = "Sample.Port"), size = 1) +
    geom_line(aes(y = EWMA_Port_Cum, color = "EWMA.Port"), size = 1) +
    geom_line(aes(y = EQW_Port_Cum, color = "EQW.Port"), size = 1) +
    labs(
      title = "Cumulative Returns - Minimum Variance Portfolios",
      subtitle = ifelse(is.null(max_weight), "Unconstrained", 
                       paste("Constrained (Max Weight:", max_weight, ")")),
      x = "Date",
      y = "Cumulative Return",
      color = "Portfolio"
    ) +
    scale_color_manual(values = c("Sample.Port" = "blue", 
                                  "EWMA.Port" = "red", 
                                  "EQW.Port" = "green")) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11),
      legend.position = "bottom"
    )
  
  ggsave(paste0("output/part_b_cumulative_returns", max_weight_suffix, ".png"), 
         p1, width = 12, height = 6, dpi = 300)
  cat("Saved: output/part_b_cumulative_returns", max_weight_suffix, ".png\n")
  
  # Plot 2: Average Portfolio Weights (Sample.Port only)
  if (length(sample_weights_list) > 0) {
    # Calculate average weights across all months
    avg_weights <- colMeans(do.call(rbind, sample_weights_list), na.rm = TRUE)
    avg_weights_df <- data.frame(
      Industry = names(avg_weights),
      Weight = avg_weights
    )
    avg_weights_df <- avg_weights_df[order(-avg_weights_df$Weight), ]
    
    p2 <- ggplot(avg_weights_df, aes(x = reorder(Industry, Weight), y = Weight)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Average Portfolio Weights - Sample.Port",
        subtitle = ifelse(is.null(max_weight), "Unconstrained", 
                         paste("Constrained (Max Weight:", max_weight, ")")),
        x = "Industry",
        y = "Average Weight"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        axis.text.y = element_text(size = 6)
      )
    
    ggsave(paste0("output/part_b_avg_weights", max_weight_suffix, ".png"), 
           p2, width = 12, height = 10, dpi = 300)
    cat("Saved: output/part_b_avg_weights", max_weight_suffix, ".png\n")
    
    # Plot 3: Portfolio Weights Through Time (Top 10 industries)
    top_10_industries <- head(avg_weights_df$Industry, 10)
    weights_time_list <- list()
    
    for (i in 1:length(sample_weights_list)) {
      weights <- sample_weights_list[[i]]
      weights_subset <- weights[top_10_industries]
      weights_time_list[[i]] <- data.frame(
        Date = monthly_analysis$Date[i],
        Industry = names(weights_subset),
        Weight = as.numeric(weights_subset)
      )
    }
    
    weights_time_df <- do.call(rbind, weights_time_list)
    
    p3 <- ggplot(weights_time_df, aes(x = Date, y = Weight, color = Industry)) +
      geom_line(size = 0.8) +
      labs(
        title = "Portfolio Weights Through Time - Sample.Port (Top 10 Industries)",
        subtitle = ifelse(is.null(max_weight), "Unconstrained", 
                         paste("Constrained (Max Weight:", max_weight, ")")),
        x = "Date",
        y = "Portfolio Weight",
        color = "Industry"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.title = element_text(size = 11),
        legend.position = "bottom"
      )
    
    ggsave(paste0("output/part_b_weights_time", max_weight_suffix, ".png"), 
           p3, width = 12, height = 6, dpi = 300)
    cat("Saved: output/part_b_weights_time", max_weight_suffix, ".png\n")
  }
  
  return(list(
    results = results_df,
    metrics = metrics,
    weights = list(sample = sample_weights_list, ewma = ewma_weights_list)
  ))
}


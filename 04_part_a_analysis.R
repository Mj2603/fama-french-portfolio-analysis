# Part A: Covariance Matrix Analysis

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cran.r-project.org")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate", repos = "https://cran.r-project.org")
}
library(ggplot2)
library(lubridate)

source("02_covariance_functions.R")

# Part A.1: Window Length Sensitivity Analysis
part_a1_analysis <- function(daily_data, target_date, 
                              window_min = 50, window_max = 2000, window_step = 25) {
  cat("Part A.1: Window Length Sensitivity Analysis\n")
  cat("Target date:", as.character(target_date), "\n")
  
  # Filter data up to target date
  data_until_date <- daily_data[daily_data$Date <= target_date, ]
  if (nrow(data_until_date) < window_max) {
    stop(paste("Insufficient data. Need at least", window_max, "observations before", target_date))
  }
  
  # Get returns columns (exclude Date)
  returns_cols <- setdiff(colnames(data_until_date), "Date")
  
  # Calculate eigenvalues and condition numbers for different window lengths
  window_lengths <- seq(window_min, window_max, by = window_step)
  min_eigenvalues <- numeric(length(window_lengths))
  condition_numbers <- numeric(length(window_lengths))
  
  cat("Calculating eigenvalues for", length(window_lengths), "window lengths...\n")
  for (i in seq_along(window_lengths)) {
    window_len <- window_lengths[i]
    if (i %% 10 == 0) cat("  Processing window length", window_len, "\n")
    
    # Get last window_len days of data
    window_data <- tail(data_until_date, window_len)
    returns_matrix <- as.matrix(window_data[, returns_cols])
    
    # Estimate covariance matrix
    cov_matrix <- estimate_sample_covariance(returns_matrix)
    
    # Calculate properties
    min_eigenvalues[i] <- get_min_eigenvalue(cov_matrix)
    condition_numbers[i] <- get_condition_number(cov_matrix)
  }
  
  # Create results dataframe
  results <- data.frame(
    WindowLength = window_lengths,
    MinEigenvalue = min_eigenvalues,
    ConditionNumber = condition_numbers
  )
  
  # Create output directory
  if (!dir.exists("output")) {
    dir.create("output")
  }
  
  # Plot 1: Minimum Eigenvalue
  p1 <- ggplot(results, aes(x = WindowLength, y = MinEigenvalue)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "blue", size = 0.5) +
    labs(
      title = "Minimum Eigenvalue vs. Window Length",
      subtitle = paste("Analysis Date:", as.character(target_date)),
      x = "Window Length (days)",
      y = "Minimum Eigenvalue"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11)
    )
  
  ggsave("output/part_a1_min_eigenvalue.png", p1, width = 10, height = 6, dpi = 300)
  cat("Saved: output/part_a1_min_eigenvalue.png\n")
  
  # Plot 2: Condition Number
  p2 <- ggplot(results, aes(x = WindowLength, y = ConditionNumber)) +
    geom_line(color = "red", size = 1) +
    geom_point(color = "red", size = 0.5) +
    labs(
      title = "Condition Number vs. Window Length",
      subtitle = paste("Analysis Date:", as.character(target_date)),
      x = "Window Length (days)",
      y = "Condition Number"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11)
    )
  
  ggsave("output/part_a1_condition_number.png", p2, width = 10, height = 6, dpi = 300)
  cat("Saved: output/part_a1_condition_number.png\n")
  
  return(results)
}

# Part A.2: Maximum Eigenvalue Through Time
part_a2_analysis <- function(daily_data, start_date, end_date, window_length = 252) {
  cat("Part A.2: Maximum Eigenvalue Through Time\n")
  cat("Period:", as.character(start_date), "to", as.character(end_date), "\n")
  cat("Window length:", window_length, "days\n")
  
  # Filter data
  data_subset <- daily_data[daily_data$Date >= start_date & daily_data$Date <= end_date, ]
  if (nrow(data_subset) == 0) {
    stop("No data available for the specified period")
  }
  
  # Get returns columns
  returns_cols <- setdiff(colnames(data_subset), "Date")
  
  # Get unique month-end dates within the analysis period
  all_month_ends <- seq(from = floor_date(start_date, "month"), 
                        to = ceiling_date(end_date, "month") - days(1), 
                        by = "month")
  
  # Filter for actual month-end dates present in the daily data
  analysis_dates <- c()
  for (m_end in all_month_ends) {
    if (m_end %in% daily_data$Date) {
      historical_data_for_window <- daily_data[daily_data$Date <= m_end, ]
      if (nrow(historical_data_for_window) >= window_length) {
        analysis_dates <- c(analysis_dates, m_end)
      }
    }
  }
  analysis_dates <- as.Date(analysis_dates, origin = "1970-01-01")
  
  cat("Found", length(analysis_dates), "month-end dates with sufficient data\n")
  
  # Calculate maximum eigenvalues
  max_eigenvalues <- numeric(length(analysis_dates))
  
  for (i in seq_along(analysis_dates)) {
    if (i %% 12 == 0) cat("  Processing date", i, "of", length(analysis_dates), "\n")
    
    date <- analysis_dates[i]
    
    # Get historical data up to this date
    historical_data <- daily_data[daily_data$Date <= date, ]
    
    # Get last window_length days
    window_data <- tail(historical_data, window_length)
    returns_matrix <- as.matrix(window_data[, returns_cols])
    
    # Estimate covariance matrix
    cov_matrix <- estimate_sample_covariance(returns_matrix)
    
    # Calculate maximum eigenvalue
    max_eigenvalues[i] <- get_max_eigenvalue(cov_matrix)
  }
  
  # Create results dataframe
  results <- data.frame(
    Date = analysis_dates,
    MaxEigenvalue = max_eigenvalues
  )
  
  # Create output directory
  if (!dir.exists("output")) {
    dir.create("output")
  }
  
  # Plot: Maximum Eigenvalue Through Time
  p <- ggplot(results, aes(x = Date, y = MaxEigenvalue)) +
    geom_line(color = "darkgreen", size = 1) +
    labs(
      title = "Maximum Eigenvalue Through Time",
      subtitle = paste("Window Length:", window_length, "days"),
      x = "Date",
      y = "Maximum Eigenvalue"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 11)
    )
  
  ggsave("output/part_a2_max_eigenvalue_time.png", p, width = 12, height = 6, dpi = 300)
  cat("Saved: output/part_a2_max_eigenvalue_time.png\n")
  
  return(results)
}


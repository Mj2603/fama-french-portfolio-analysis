# Main Script: Fama-French 49 Industry Portfolio Analysis

cat("Fama-French 49 Industry Portfolio Analysis\n\n")

# Load required packages
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "https://cran.r-project.org")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate", repos = "https://cran.r-project.org")
}

# Source configuration and functions
source("config.R")
source("01_download_data.R")
source("02_covariance_functions.R")
source("03_portfolio_functions.R")
source("04_part_a_analysis.R")
source("05_part_b_backtest.R")


# Adjust end date if it's in the future
current_date <- Sys.Date()
actual_end_date <- min(as.Date(ANALYSIS_END_DATE), current_date)
if (actual_end_date < as.Date(ANALYSIS_END_DATE)) {
  cat(paste0("Warning: Analysis end date adjusted to ", actual_end_date, 
             " as original end date is in the future.\n\n"))
}

# Load or download data
if (file.exists("data/daily_returns.rds") && file.exists("data/monthly_returns.rds")) {
  cat("Loading existing data files\n")
  daily_data <- readRDS("data/daily_returns.rds")
  monthly_data <- readRDS("data/monthly_returns.rds")
  cat("Data loaded successfully.\n\n")
} else {
  cat("Data files not found. Downloading and processing data\n")
  data_list <- download_fama_french_data()
  daily_data <- data_list$daily
  monthly_data <- data_list$monthly
  cat("Data download and processing complete.\n\n")
}

# Part A: Covariance Matrix Analysis
cat("Part A: Covariance Matrix Analysis\n\n")

# Part A.1: Window Length Sensitivity
cat("Part A.1: Window Length Sensitivity Analysis\n")
part_a1_results <- part_a1_analysis(
  daily_data, 
  PART_A1_TARGET_DATE,
  PART_A1_WINDOW_MIN,
  PART_A1_WINDOW_MAX,
  PART_A1_WINDOW_STEP
)
cat("Part A.1 complete.\n\n")

# Part A.2: Maximum Eigenvalue Through Time
cat("Part A.2: Maximum Eigenvalue Through Time\n")
part_a2_results <- part_a2_analysis(
  daily_data,
  ANALYSIS_START_DATE,
  actual_end_date,
  WINDOW_LENGTH
)
cat("Part A.2 complete.\n\n")

# Part B: Minimum Variance Portfolio Backtest
cat("Part B: Minimum Variance Portfolio Backtest\n\n")

# Unconstrained backtest
cat("Running unconstrained backtest...\n")
part_b_results_unconstrained <- part_b_backtest(
  daily_data,
  monthly_data,
  ANALYSIS_START_DATE,
  actual_end_date,
  WINDOW_LENGTH,
  NULL  # No maximum weight constraint
)
cat("Unconstrained backtest complete.\n\n")

# Constrained backtest (5% maximum weight)
cat("Running constrained backtest (5% max weight)...\n")
part_b_results_constrained <- part_b_backtest(
  daily_data,
  monthly_data,
  ANALYSIS_START_DATE,
  actual_end_date,
  WINDOW_LENGTH,
  MAX_WEIGHT_CONSTRAINT
)
cat("Constrained backtest complete.\n\n")

cat("\nAnalysis complete.\n")
cat("All results saved in the 'output' directory.\n")



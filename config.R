# Configuration File for Fama-French 49 Industry Portfolio Analysis

# Part A.1 Analysis Parameters
PART_A1_TARGET_DATE <- as.Date("2022-12-31")
PART_A1_WINDOW_MIN <- 50
PART_A1_WINDOW_MAX <- 2000
PART_A1_WINDOW_STEP <- 25

# Part A.2 Analysis Parameters
ANALYSIS_START_DATE <- as.Date("2005-03-31")
ANALYSIS_END_DATE <- as.Date("2025-07-31")

# Part B Backtest Parameters
WINDOW_LENGTH <- 252  # 1 trading year
EWMA_CORR_HALFLIFE <- 252  # Correlation half-life in days
EWMA_VOL_HALFLIFE <- 126   # Volatility half-life in days
MAX_WEIGHT_CONSTRAINT <- 0.05  # 5% maximum weight per asset

# Output Directory
OUTPUT_DIR <- "output"


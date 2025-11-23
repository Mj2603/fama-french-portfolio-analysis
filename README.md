# Fama-French 49 Industry Portfolio Analysis

## Project Overview

This project performs a comprehensive analysis of covariance matrix estimation methods and minimum variance portfolio optimization using the Fama-French 49 Industry Portfolios. The analysis includes:

- **Part A**: Covariance matrix properties analysis (eigenvalues, condition numbers)
- **Part B**: Minimum variance portfolio backtesting with different estimation methods

## Project Structure

```
.
├── config.R                      # Configuration parameters
├── 01_download_data.R            # Data download and processing
├── 02_covariance_functions.R     # Covariance estimation functions
├── 03_portfolio_functions.R     # Portfolio optimization functions
├── 04_part_a_analysis.R          # Part A analysis scripts
├── 05_part_b_backtest.R         # Part B backtest scripts
├── main.R                        # Main orchestrator script
├── data/                         # Data directory
│   ├── daily_returns.rds
│   └── monthly_returns.rds
└── output/                       # Output directory
    ├── *.png                     # Analysis plots

```

## Quick Start

1. **Run the complete analysis:**
   ```r
   source("main.R")
   ```

## Requirements

### R Packages
- `quadprog` - Portfolio optimization
- `ggplot2` - Plotting
- `lubridate` - Date handling

## Data

The project uses the Fama-French 49 Industry Portfolios:
- **Source**: https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
- **Type**: Value-weighted returns
- **Frequency**: Daily and monthly
- **Period**: 1926-2025

## Outputs

### Part A Analysis
- `part_a1_min_eigenvalue.png` - Minimum eigenvalue vs window length
- `part_a1_condition_number.png` - Condition number vs window length
- `part_a2_max_eigenvalue_time.png` - Maximum eigenvalue through time

### Part B Backtest
- `part_b_cumulative_returns.png` - Cumulative returns (unconstrained)
- `part_b_cumulative_returns_maxweight0.05.png` - Cumulative returns (constrained)
- `part_b_avg_weights.png` - Average portfolio weights (unconstrained)
- `part_b_avg_weights_maxweight0.05.png` - Average portfolio weights (constrained)
- `part_b_weights_time.png` - Portfolio weights through time (unconstrained)
- `part_b_weights_time_maxweight0.05.png` - Portfolio weights through time (constrained)


## Configuration

Edit `config.R` to modify analysis parameters:
- `PART_A1_TARGET_DATE` - Date for window length sensitivity analysis
- `ANALYSIS_START_DATE` - Backtest start date
- `ANALYSIS_END_DATE` - Backtest end date
- `WINDOW_LENGTH` - Historical window length (default: 252 days)
- `MAX_WEIGHT_CONSTRAINT` - Maximum weight per asset (default: 0.05 = 5%)

## Methodology

### Covariance Estimation
1. **Sample Covariance**: Standard unbiased estimator
2. **EWMA with Double Decay**: 
   - Correlation half-life: 252 days
   - Volatility half-life: 126 days

### Portfolio Optimization
- Objective: Minimize portfolio variance
- Constraints: Long-only, fully invested, optional maximum weight

### Performance Metrics
- Annualized return
- Annualized volatility
- Sharpe ratio (assuming zero risk-free rate)

## License

This project is for educational and research purposes.


# Download and Process Fama-French 49 Industry Portfolio Data

download_fama_french_data <- function() {
  cat("Downloading Fama-French 49 Industry Portfolio data...\n")
  
  daily_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/49_Industry_Portfolios_daily_CSV.zip"
  monthly_url <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/49_Industry_Portfolios_CSV.zip"
  
  if (!dir.exists("data")) {
    dir.create("data")
  }
  
  # Download daily data
  daily_zip <- "data/daily_returns.zip"
  if (!file.exists(daily_zip)) {
    cat("Downloading daily returns\n")
    download.file(daily_url, daily_zip, mode = "wb", quiet = TRUE)
  }
  
  # Download monthly data
  monthly_zip <- "data/monthly_returns.zip"
  if (!file.exists(monthly_zip)) {
    cat("Downloading monthly returns\n")
    download.file(monthly_url, monthly_zip, mode = "wb", quiet = TRUE)
  }
  
  # Extract daily data
  if (!file.exists("data/49_Industry_Portfolios_daily.csv")) {
    unzip(daily_zip, exdir = "data")
  }
  daily_file <- list.files("data", pattern = "49_Industry_Portfolios_daily\\.csv", full.names = TRUE)[1]
  
  # Extract monthly data
  if (!file.exists("data/49_Industry_Portfolios.csv")) {
    unzip(monthly_zip, exdir = "data")
  }
  monthly_file <- list.files("data", pattern = "^49_Industry_Portfolios\\.csv$", full.names = TRUE)[1]
  
  # Read daily data (Value Weighted)
  cat("Processing daily returns\n")
  daily_raw_lines <- readLines(daily_file, n = 30)
  daily_start_line <- which(grepl("Average Value Weighted Returns -- Daily", daily_raw_lines))[1] + 2
  daily_end_line <- which(grepl("Average Equal Weighted Returns -- Daily", daily_raw_lines))[1] - 1
  if (is.na(daily_end_line) || daily_end_line < daily_start_line) {
    daily_end_line <- length(readLines(daily_file))
  }
  
  daily_data <- read.csv(daily_file, skip = daily_start_line - 1, 
                         nrows = daily_end_line - daily_start_line + 1, 
                         stringsAsFactors = FALSE, header = TRUE)
  colnames(daily_data)[1] <- "Date"
  daily_data$Date <- as.Date(as.character(daily_data$Date), format = "%Y%m%d")
  daily_data <- daily_data[!is.na(daily_data$Date), ]
  
  # Convert missing values and percentages
  for (i in 2:ncol(daily_data)) {
    daily_data[[i]] <- as.numeric(gsub("-99.99", NA, as.character(daily_data[[i]])))
    daily_data[[i]] <- as.numeric(gsub("-999", NA, as.character(daily_data[[i]])))
  }
  daily_data[, -1] <- daily_data[, -1] / 100
  colnames(daily_data) <- trimws(colnames(daily_data))
  
  # Read monthly data (Value Weighted)
  cat("Processing monthly returns\n")
  monthly_raw_lines <- readLines(monthly_file, n = 30)
  monthly_start_line <- which(grepl("Average Value Weighted Returns -- Monthly", monthly_raw_lines))[1] + 2
  monthly_end_line <- which(grepl("Average Equal Weighted Returns -- Monthly", monthly_raw_lines))[1] - 1
  if (is.na(monthly_end_line) || monthly_end_line < monthly_start_line) {
    monthly_end_line <- length(readLines(monthly_file))
  }
  
  monthly_data <- read.csv(monthly_file, skip = monthly_start_line - 1, 
                           nrows = monthly_end_line - monthly_start_line + 1, 
                           stringsAsFactors = FALSE, header = TRUE)
  colnames(monthly_data)[1] <- "Date"
  
  # Convert YYYYMM to last day of month
  monthly_data$Date <- as.Date(paste0(monthly_data$Date, "01"), format = "%Y%m%d")
  monthly_data$Date <- sapply(monthly_data$Date, function(d) {
    if (is.na(d)) return(NA)
    last_day <- as.Date(format(d + 31, "%Y-%m-01")) - 1
    return(last_day)
  })
  monthly_data$Date <- as.Date(monthly_data$Date, origin = "1970-01-01")
  monthly_data <- monthly_data[!is.na(monthly_data$Date), ]
  
  # Convert missing values and percentages
  for (i in 2:ncol(monthly_data)) {
    monthly_data[[i]] <- as.numeric(gsub("-99.99", NA, as.character(monthly_data[[i]])))
    monthly_data[[i]] <- as.numeric(gsub("-999", NA, as.character(monthly_data[[i]])))
  }
  monthly_data[, -1] <- monthly_data[, -1] / 100
  colnames(monthly_data) <- trimws(colnames(monthly_data))
  
  # Save processed data
  saveRDS(daily_data, "data/daily_returns.rds")
  saveRDS(monthly_data, "data/monthly_returns.rds")
  
  cat("Data processing complete.\n")
  cat("Daily returns:", nrow(daily_data), "observations from", min(daily_data$Date), "to", max(daily_data$Date), "\n")
  cat("Monthly returns:", nrow(monthly_data), "observations from", min(monthly_data$Date), "to", max(monthly_data$Date), "\n")
  
  return(list(daily = daily_data, monthly = monthly_data))
}


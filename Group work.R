# Needed Packages
install.packages(c("readxl", "zoo", "ggplot2", "forecast", "tseries", "tsutils", "changepoint", "smooth"))


# Load required libraries
library(readxl)
library(ggplot2)
library(forecast)
library(tseries)
library(tsutils)
library(changepoint)
library(smooth)
library(zoo)

# Data Loading and Preprocessing
data <- read_excel("data.xlsx", sheet = "Sheet1")
data$date <- as.Date(data$date)

# Check for duplicate dates
duplicated_dates <- data$date[duplicated(data$date)]
if (length(duplicated_dates) > 0) {
  cat("The following duplicate dates were found and removed:\n")
  print(duplicated_dates)
  data <- data[!duplicated(data$date), ]
} else {
  cat("No duplicate dates were found.\n")
}

# Complete missing dates
full_dates <- seq(min(data$date, na.rm = TRUE), max(data$date, na.rm = TRUE), by = "day")
data <- merge(data.frame(date = full_dates), data, by = "date", all.x = TRUE)

# Fill missing values using linear interpolation
data$value <- na.approx(data$value, rule = 2)

# Create time series object
ts_data <- ts(data$value, frequency = 365, start = c(1996, as.numeric(format(min(data$date), "%j"))))

# 1. Data Visualization and Descriptive Statistics
plot(ts_data, main = "Time Series Plot", ylab = "Value", col = "blue", type = "l")
summary_stats <- data.frame(
  Mean = mean(data$value, na.rm = TRUE),
  Median = median(data$value, na.rm = TRUE),
  SD = sd(data$value, na.rm = TRUE),
  Min = min(data$value, na.rm = TRUE),
  Max = max(data$value, na.rm = TRUE)
)
print(summary_stats)

# 2. Dynamic Anomaly Detection with Moving Average and Thresholds
window_size <- 30  # Set a reasonable window size for rolling statistics
rolling_mean <- rollmean(ts_data, k = window_size, fill = NA)
rolling_sd <- rollapply(ts_data, width = window_size, FUN = sd, fill = NA)
upper_bound <- rolling_mean + 2 * rolling_sd
lower_bound <- rolling_mean - 2 * rolling_sd

# Plot original series with dynamic anomaly bounds
plot(ts_data, main = "Time Series with Dynamic Anomaly Detection", ylab = "Value", col = "black", type = "l")
lines(rolling_mean, col = "green", lwd = 1, lty = 1)
lines(upper_bound, col = "red", lwd = 1, lty = 1)
lines(lower_bound, col = "red", lwd = 1, lty = 1)

# Mark anomalies based on the dynamic threshold
anomalies <- which(ts_data > upper_bound | ts_data < lower_bound)
points(anomalies, ts_data[anomalies], col = "purple", pch = 19)
cat("Anomalies detected at indices:\n")
print(anomalies)

# 3. Structural Break Detection
cpt <- cpt.meanvar(ts_data, method = "BinSeg", Q = 5)  # Allow up to 5 breakpoints
plot(cpt, main = "Structural Breaks in Time Series")
cat("Detected breakpoints:\n")
print(cpts(cpt))

# Annotate the detected breakpoints on the plot
breakpoints <- cpts(cpt)
abline(v = breakpoints, col = "orange", lty = 2)

# 4. ACF and PACF Analysis
par(mfrow = c(2, 2))
# Original series ACF and PACF
acf(ts_data, main = "ACF of Original Series")
pacf(ts_data, main = "PACF of Original Series")

# Differenced series ACF and PACF
diff_ts <- diff(ts_data)
acf(diff_ts, main = "ACF of Differenced Series")
pacf(diff_ts, main = "PACF of Differenced Series")

# 5. Time Series Aggregation (Weekly, Monthly, Quarterly)
par(mfrow = c(1, 1))
# Weekly aggregation
weekly_data <- aggregate(data$value, by = list(format(data$date, "%Y-%U")), FUN = mean, na.rm = TRUE)
colnames(weekly_data) <- c("Week", "Value")
weekly_ts <- ts(weekly_data$Value, frequency = 52, start = c(1996, 1))
plot(weekly_ts, main = "Weekly Aggregated Series", ylab = "Value", col = "blue", type = "l")

# Monthly aggregation
monthly_data <- aggregate(data$value, by = list(format(data$date, "%Y-%m")), FUN = mean, na.rm = TRUE)
colnames(monthly_data) <- c("Month", "Value")
monthly_ts <- ts(monthly_data$Value, frequency = 12, start = c(1996, 1))
plot(monthly_ts, main = "Monthly Aggregated Series", ylab = "Value", col = "blue", type = "l")

# Quarterly aggregation
quarterly_data <- aggregate(data$value, by = list(as.yearqtr(data$date)), FUN = mean, na.rm = TRUE)
colnames(quarterly_data) <- c("Quarter", "Value")
quarterly_ts <- ts(quarterly_data$Value, frequency = 4, start = c(1996, 1))
plot(quarterly_ts, main = "Quarterly Aggregated Series", ylab = "Value", col = "blue", type = "l")

# Combined plots for aggregation
par(mfrow = c(2, 2))
plot(ts_data, main = "Original Time Series Plot", ylab = "Value", col = "blue", type = "l")
plot(weekly_ts, main = "Weekly Aggregated Series", ylab = "Value", col = "blue", type = "l")
plot(monthly_ts, main = "Monthly Aggregated Series", ylab = "Value", col = "blue", type = "l")
plot(quarterly_ts, main = "Quarterly Aggregated Series", ylab = "Value", col = "blue", type = "l")

# 6. Moving Average (Smoothing) with Various Window Sizes

# 7-day moving average
par(mfrow = c(1, 3))
ma_7 <- rollmean(ts_data, 7, fill = NA)
plot(ts_data, main = "7-day Moving Average", ylab = "Value", col = "blue", type = "l")
lines(ma_7, col = "red", lwd = 2)

# 30-day moving average
ma_30 <- rollmean(ts_data, 30, fill = NA)
plot(ts_data, main = "30-day Moving Average", ylab = "Value", col = "blue", type = "l")
lines(ma_30, col = "green", lwd = 2)

# 90-day moving average
ma_90 <- rollmean(ts_data, 90, fill = NA)
plot(ts_data, main = "90-day Moving Average", ylab = "Value", col = "blue", type = "l")
lines(ma_90, col = "purple", lwd = 2)
par(mfrow = c(1, 1))


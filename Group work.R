# Needed Packages
install.packages(c("readxl", "zoo", "forecast", "tseries", "changepoint", "smooth"))
library(readxl)
library(zoo)
library(forecast)
library(tseries)
library(changepoint)
library(smooth)

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
ts_data <- ts(data$value, frequency = 365, start = c(as.numeric(format(min(data$date), "%Y")), as.numeric(format(min(data$date), "%j"))))

# Plotting with proper date axis
plot(data$date, data$value, type = "l", col = "blue", main = "Time Series Plot", xlab = "Date", ylab = "Value")

# 2. Dynamic Anomaly Detection with Moving Average and Thresholds
window_size <- 30  # Set a reasonable window size for rolling statistics
rolling_mean <- rollmean(ts_data, k = window_size, fill = NA, align = "center")
rolling_sd <- rollapply(ts_data, width = window_size, FUN = sd, fill = NA, align = "center")
upper_bound <- rolling_mean + 2 * rolling_sd
lower_bound <- rolling_mean - 2 * rolling_sd

# Add dynamic bounds and highlight anomalies
lines(data$date, rolling_mean, col = "green", lwd = 2, lty = 2)
lines(data$date, upper_bound, col = "red", lwd = 2, lty = 3)
lines(data$date, lower_bound, col = "red", lwd = 2, lty = 3)

# Highlight anomalies
anomalies <- which(data$value > upper_bound | data$value < lower_bound)
points(data$date[anomalies], data$value[anomalies], col = "purple", pch = 19)

cat("Anomalies detected at indices:\n")
print(anomalies)

# 3. Structural Break Detection
cpt <- cpt.meanvar(ts_data, method = "BinSeg", Q = 20) 
breakpoints <- cpts(cpt)
cat("Detected breakpoints:\n")
print(breakpoints)

# Add breakpoints to the plot
abline(v = data$date[breakpoints], col = "orange", lty = 2)

# 4. ACF and PACF Analysis
par(mfrow = c(2, 2))
acf(ts_data, main = "ACF of Original Series")
pacf(ts_data, main = "PACF of Original Series")
diff_ts <- diff(ts_data)
acf(diff_ts, main = "ACF of Differenced Series")
pacf(diff_ts, main = "PACF of Differenced Series")
par(mfrow = c(1, 1))

# 5. Time Series Aggregation (Weekly, Monthly, Quarterly)
weekly_data <- aggregate(data$value, by = list(format(data$date, "%Y-%U")), FUN = mean, na.rm = TRUE)
colnames(weekly_data) <- c("Week", "Value")
weekly_data$Date <- as.Date(paste0(weekly_data$Week, "-1"), format = "%Y-%U-%u")

monthly_data <- aggregate(data$value, by = list(format(data$date, "%Y-%m")), FUN = mean, na.rm = TRUE)
colnames(monthly_data) <- c("Month", "Value")
monthly_data$Date <- as.Date(paste0(monthly_data$Month, "-01"))

quarterly_data <- aggregate(data$value, by = list(as.yearqtr(data$date)), FUN = mean, na.rm = TRUE)
colnames(quarterly_data) <- c("Quarter", "Value")
quarterly_data$Date <- as.Date(quarterly_data$Quarter)

# Aggregated plots
par(mfrow = c(2, 2))
plot(weekly_data$Date, weekly_data$Value, type = "l", col = "blue", main = "Weekly Aggregated Series", xlab = "Date", ylab = "Value")
plot(monthly_data$Date, monthly_data$Value, type = "l", col = "blue", main = "Monthly Aggregated Series", xlab = "Date", ylab = "Value")
plot(quarterly_data$Date, quarterly_data$Value, type = "l", col = "blue", main = "Quarterly Aggregated Series", xlab = "Date", ylab = "Value")
plot(data$date, data$value, type = "l", col = "blue", main = "Original Time Series", xlab = "Date", ylab = "Value")

# 6. Moving Average (Smoothing) with Various Window Sizes
par(mfrow = c(1, 3))
ma_7 <- rollmean(ts_data, 7, fill = NA, align = "center")
plot(data$date, data$value, type = "l", col = "blue", main = "7-day Moving Average", xlab = "Date", ylab = "Value")
lines(data$date, ma_7, col = "red", lwd = 2)

ma_30 <- rollmean(ts_data, 30, fill = NA, align = "center")
plot(data$date, data$value, type = "l", col = "blue", main = "30-day Moving Average", xlab = "Date", ylab = "Value")
lines(data$date, ma_30, col = "green", lwd = 2)

ma_90 <- rollmean(ts_data, 90, fill = NA, align = "center")
plot(data$date, data$value, type = "l", col = "blue", main = "90-day Moving Average", xlab = "Date", ylab = "Value")
lines(data$date, ma_90, col = "purple", lwd = 2)
par(mfrow = c(1, 1))

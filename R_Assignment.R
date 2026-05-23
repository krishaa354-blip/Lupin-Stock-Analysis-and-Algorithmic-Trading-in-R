# ============================================
# FINANCIAL DATA ANALYSIS OF LUPIN STOCK
# ============================================

# ============================================
# INSTALL REQUIRED PACKAGES
# ============================================

install.packages("readxl")
install.packages("ggplot2")
install.packages("forecast")
install.packages("TTR")

# ============================================
# LOAD LIBRARIES
# ============================================

library(readxl)
library(ggplot2)
library(forecast)
library(TTR)

# ============================================
# LOAD EXCEL FILE
# ============================================

lupin_data <- read_excel(file.choose())

# ============================================
# VIEW DATA
# ============================================

head(lupin_data)

colnames(lupin_data)

# ============================================
# CONVERT DATE FORMAT
# ============================================

lupin_data$Date <- as.Date(lupin_data$Date)

# ============================================
# REMOVE MISSING VALUES
# ============================================

lupin_data <- na.omit(lupin_data)

# ============================================
# CREATE SIMPLE COLUMN NAMES
# ============================================

lupin_data$Price  <- lupin_data$LUPIN.NS.Close
lupin_data$High   <- lupin_data$LUPIN.NS.High
lupin_data$Low    <- lupin_data$LUPIN.NS.Low
lupin_data$Open   <- lupin_data$LUPIN.NS.Open
lupin_data$Volume <- lupin_data$LUPIN.NS.Volume

# ============================================
# DATA VISUALISATION
# ============================================

# ============================================
# GRAPH 1
# LINE CHART - STOCK PRICE TREND
# ============================================

p1 <- ggplot(lupin_data, aes(Date, Price)) +
  geom_line(color = "blue") +
  ggtitle("Lupin Stock Price Trend") +
  xlab("Date") +
  ylab("Closing Price")

p1

# ============================================
# GRAPH 2
# BAR PLOT - TRADING VOLUME
# ============================================

p2 <- ggplot(lupin_data, aes(Date, Volume)) +
  geom_bar(
    stat = "identity",
    fill = "darkgreen"
  ) +
  ggtitle("Lupin Trading Volume") +
  xlab("Date") +
  ylab("Volume")

p2

# ============================================
# GRAPH 3
# HIGH VS LOW PRICE
# ============================================

p3 <- ggplot(lupin_data, aes(Date)) +
  geom_line(
    aes(y = High, color = "High")
  ) +
  geom_line(
    aes(y = Low, color = "Low")
  ) +
  ggtitle("High vs Low Price") +
  xlab("Date") +
  ylab("Price")

p3

# ============================================
# GRAPH 4
# OPEN VS CLOSE PRICE
# ============================================

p4 <- ggplot(
  lupin_data,
  aes(Open, Price)
) +
  geom_point(color = "red") +
  ggtitle("Open vs Closing Price") +
  xlab("Opening Price") +
  ylab("Closing Price")

p4

# ============================================
# GRAPH 5
# MOVING AVERAGE
# ============================================

lupin_data$MA20 <- SMA(
  lupin_data$Price,
  20
)

p5 <- ggplot(
  lupin_data,
  aes(Date, Price)
) +
  geom_line(color = "black") +
  geom_line(
    aes(y = MA20),
    color = "red"
  ) +
  ggtitle("20-Day Moving Average") +
  xlab("Date") +
  ylab("Price")

p5

# ============================================
# GRAPH 6
# TREND ANALYSIS
# ============================================

p6 <- ggplot(
  lupin_data,
  aes(Date, Price)
) +
  geom_line(color = "blue") +
  geom_smooth(
    method = "lm",
    color = "red"
  ) +
  ggtitle("Trend Analysis") +
  xlab("Date") +
  ylab("Price")

p6

# ============================================
# BASIC TIME SERIES ANALYSIS
# ============================================

# ============================================
# CREATE TIME SERIES
# ============================================

price_ts <- ts(
  lupin_data$Price,
  frequency = 252
)

# ============================================
# TIME SERIES PLOT
# ============================================

plot(
  price_ts,
  main = "Lupin Time Series",
  col = "blue"
)

# ============================================
# SEASONAL DECOMPOSITION
# ============================================

decomp_ts <- ts(
  lupin_data$Price,
  frequency = 30
)

decomposition <- decompose(
  decomp_ts
)

plot(decomposition)

# ============================================
# SIMPLE ARIMA MODEL
# ============================================

model <- auto.arima(
  price_ts,
  seasonal = FALSE,
  stepwise = TRUE,
  approximation = TRUE
)

summary(model)

# ============================================
# FORECASTING
# ============================================

forecast_values <- forecast(
  model,
  h = 30
)

plot(
  forecast_values,
  main = "Lupin 30-Day Forecast"
)

# ============================================
# ALGORITHMIC TRADING
# ============================================

# ============================================
# CREATE MOVING AVERAGES
# ============================================

lupin_data$MA50 <- SMA(
  lupin_data$Price,
  50
)

# ============================================
# REMOVE NA VALUES
# ============================================

trade_data <- na.omit(
  lupin_data[
    ,
    c(
      "Date",
      "Price",
      "MA20",
      "MA50"
    )
  ]
)

# ============================================
# BUY / SELL SIGNAL
# ============================================

trade_data$Signal <- ifelse(
  trade_data$MA20 >
    trade_data$MA50,
  "BUY",
  "SELL"
)

# ============================================
# PLOT ALGORITHMIC TRADING
# ============================================

plot(
  trade_data$Date,
  trade_data$Price,
  type = "l",
  col = "black",
  lwd = 2,
  main = "Lupin Algorithmic Trading",
  xlab = "Date",
  ylab = "Price"
)

# SMA20

lines(
  trade_data$Date,
  trade_data$MA20,
  col = "blue",
  lwd = 2
)

# SMA50

lines(
  trade_data$Date,
  trade_data$MA50,
  col = "red",
  lwd = 2
)

# LEGEND

legend(
  "topleft",
  legend = c(
    "Price",
    "SMA20",
    "SMA50"
  ),
  col = c(
    "black",
    "blue",
    "red"
  ),
  lty = 1,
  lwd = 2
)

# ============================================
# SHOW LAST BUY/SELL SIGNALS
# ============================================

tail(
  trade_data[
    ,
    c(
      "Date",
      "Price",
      "MA20",
      "MA50",
      "Signal"
    )
  ]
)

# ============================================
# PROJECT COMPLETED
# ============================================
install.packages(quantmod)
library(quantmod)
library(tidyverse)
library(tidyquant)
library(padr)
library(dplyr)
#####Problem 1#####
# Write the following functions:
# 1.1. A function which replicates the SMA function. It should calculate the SMA
# for a vector to the left:
# 1.2. A function, which calculates the correlation coefficient between two vectors.
# It should replicate the cor function from the base package.
#####Problem 1#####
SMA_fun = function(a,b){
  if(b == length(a)){
    sm = sum(a)/b
    na = rep(NA,(b-1))
    sm = c(na, sm)
  } else {
    c = cumsum(a)
    sm = c[(b+1):length(c)] - c[1:(length(c)-b)]
    sm = c(c[b], sm)
    sm = sm/b
    na = rep(NA,(b-1))
    sm = c(na, sm)
  }
  return(sm)
}

x = 1:10
SMA(x,1)
SMA_fun(x,1)

#no idea for 1.2

#####Problem 2#####
# Find all prime numbers less than 100, using for/while loops.
#####Problem 2#####
prm_num = function(n) {
  if (n >= 2) {
    x = seq(2, n)
    prime_nums = c()
    for (i in seq(2, n)) {
      if (any(x == i)) {
        prime_nums = c(prime_nums, i)
        x = c(x[(x %% i) != 0], i)
      }
    }
    return(prime_nums)
  }
  else 
  {
    stop("Invalid input")
  }
} 
prm_num(100)

#####Problem 3#####
# Read the wikipedia article and investopedia article on MACD:
# https://en.wikipedia.org/wiki/MACD
# https://www.investopedia.com/terms/m/macd.asp

# Download data for a stock of your choice and do the following:
# 1.Calculate the 26-period EMA(use the EMA function from tidyquant)
# 2.Calculate the 12-period EMA.
# 3.Calculate the MACD line(12-period EMA minus 26-period EMA)
# 4.Calculate the signal line - this is the 9-period EMA of the MACD.
# 5.Calculate the buy/sell signals. This means create a new column which tell
# us if we should buy or sell. When the MACD line crosses the signal line
# from above(MACD is above signal then MACD is below signal) this is a sell signal. 
# If it crosses from below (MACD is below signal then MACD is above signal) this is a buy signal.
# 6. Simulate how the strategy preforms and compare it to a benchmark strategy
# of just buying and holding the stock.
# In order to do this start with a portfolio of 100$ invested in the stock on the first day
# and see how it performs. Example:
# I start with 100$ and a stock which costs 100$ at the beginning of my time period.
# I get a buy signal when the stock price is 90. I buy the stock.
# I get a sell signal to sell the stock when the price is 110. I sell it and 
# and don't get any more signals.I end up with 100 * 110 / 90 = 122.22 
# The benchmark portfolio is I buy the stock at 100 at the beginning and at
# the end of the period the stock price is 120. I end up with 120.
# 122.22 > 120. so the MACD strategy was beating the market.
#####Problem 3#####

library(quantmod)
library(tidyquant)
library(tidyverse)

inital_data <- tq_get("AAPL") %>%
  mutate(EMA26 = EMA(adjusted, n =26),
         EMA12 = EMA(adjusted, n =12),
         MACDline = EMA12 - EMA26,
         SignalLine = EMA(MACDline, n=9)) %>%
  filter(!is.na(EMA26 & SignalLine)) %>%
  mutate(signal = case_when(lag(MACDline) > lag(SignalLine) & MACDline < SignalLine ~ "sell",
                            lag(MACDline) < lag(SignalLine) & MACDline > SignalLine ~ "buy",
                            TRUE ~ "hold"))

inital_data <- inital_data %>%
  mutate(BenchmarkMoney = 100,
         sss = adjusted / lag(adjusted),
         sss = ifelse(is.na(sss), 1, sss),
         BenchmarkMoney1 = cumprod(sss),
         StrategyMoney = 100,
         sss1 = case_when(signal = "do not have stocks" ~ 1,
                          signal = "have stock" ~ sss,
                          TRUE ~ 999999999),
         StrategyMoney1 = cumprod(sss1))

options(scipen = 999)

#####Problem 1#####
# 1.Download the stock prices for AMZN, FB, NFLX, stocks from 2019-01-01 
# to 2021-04-01. Keep only the symbol/date/adjusted columns.
# 2.Add all the missing dates(such as 2019-01-01), so that we have 
# observations for every single date. Fill in the missing values for adjusted 
# with the last non-missing observation.
# 3.Create a new data frame, which consist only of stocks from AMZN or FB and 
# has observations from 2019-01-01 to 2019-07-01 or 2020-04-01 to 2020-07-01. 
# Arrange the data frame first by the symbol name and by the date in 
# descending order.
# 4.Select the first and last observation of the aforementioned dataframe
# for each of the two stocks - AMZN and FB.
# 5.Select the last observation for each stock, for each month. 
# In order to do this, first create a new column, which will show you the 
# year and the month. You can do this using the functions substr() or floor_date.
#####Problem 1#####
library(tidyverse)
setwd("C:/Users/angel/Documents/RStudio")
AMAZON=read.csv("AMZN.csv")
Meta=read.csv("FB.csv")
Netflix=read.csv("NFLX.csv")

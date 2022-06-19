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
install.packages("tidyquant")
install.packages("padr")
library(tidyverse)
library(tidyquant)
library(padr)
library(dplyr)
library(lubridate)

stocks = tidyquant::tq_get(c("NFLX","FB","AMZN"), get = "stock.prices", from = "2019-01-01", to  = "2021-04-01") %>%
  select(c(symbol,date,adjusted));

dates <- data.frame(Date = rep(seq.Date(from = ymd('2019-01-01'), 
                                    to = ymd('2021-04-01'), 
                                    by = 'days'), 3),
                    Symbol = c(rep("AMZN", 822),rep("FB", 822),rep("NFLX", 822)))

joint_together <- dates %>%
  dplyr::left_join(stocks, by = c("Date" = "date", "Symbol" = "symbol")) %>%
  group_by(Symbol) %>%
  fill(adjusted, .direction = "downup")

DF <- joint_together %>%
  filter(Symbol %in% c("AMZN", "FB"),
         between(Date, ymd("2019-01-01"), ymd("2019-07-01")) | 
         between(Date, ymd("2020-04-01"), ymd("2020-07-01"))) %>%
  dplyr::arrange(Symbol, desc(Date))
  
x <- DF %>%
 filter((Date == min(ymd(Date)) | Date == max(ymd(Date))) &
          (Symbol == "AMZN" | Symbol == "FB"))

a <- NULL
a <- DataFrame %>%
  mutate(year = substring(Date, 1, 4),
         month = substring(Date, 6, 7),
         day = substring(Date, 9, 10)) %>%
  group_by(year, month) %>%
  filter(day == max(day)) %>%
  arrange(year, month)
view(a)

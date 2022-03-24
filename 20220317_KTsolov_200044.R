#####Problem 1#####
#Write a loop which simulates 1000 times a martingale strategy based on a coin flip
#Martingale is a gambling strategy where you multiply your next bet twice if
#you have lost your previous one. You bet 1 and you win. Because you won you bet 1
# again. You lose. Then you bet 2, you lose again. You bet four and you win.
#Because you won, you go back to betting one etc. You start with 100 USD and you
#base bet is one. 
#If the coin flip is biased and you have 48.60% chance to win, when do you
#go broke on average(out of those 1000 simulations)? Look at the help for sample,
#to figure out how to pick incorporate the 48.6% probability.
#You can use a while loop for simulating when you go broke. A while loop
#loops until a condition is TRUE. Example:
# i <- 1
# while (i < 6) {
#   print(i)
#   i <- i + 1
# } 
#In your case you want to loop until your budget is > 0.
# Budget <- 100
# while (Budget > 0) {
#   Do something
# } 
#Pay attention to the fact that you can't bet more money than you have.
#If you lose 1, 2, 4, 8, 16, 32. Then your remaining money will be 
#100-32-16-8-4-2-1 = 37, so you can bet max 37 USD.
#####Problem 1#####

bet = 1
money = 100

for (i in 1:1000) {
if(money >= 0) { 
if(bet <= money) {
  
  res = rbinom(1, 1, .49)
  
  print(res)
  
  if (res == 1) {
    
    money = money + bet
    cat("yes"," ",money," ", bet,"\n")
    bet = 1
   
  } else {
    
    money = money - bet
    bet = 2 * bet
    cat("no"," ",money," ", bet,"\n")
  }
} else {break}
} else {break}
}

#####Problem 2#####
# Read everything from https://r4ds.had.co.nz/transform.html, up until
# 5.6 Grouped summaries with summarise(). If you want to, you can
# read everything and then https://r4ds.had.co.nz/relational-data.html

#Do all the exercises:
# 5.2.4 Exercises 
# 5.3.1 Exercises 
# 5.4.1 Exercises 
# 5.5.2 Exercises 

#You can also read the official dplyr site.
#https://dplyr.tidyverse.org/index.html
#https://dplyr.tidyverse.org/articles/dplyr.html
#####Problem 2#####

library(nycflights13)
library(tidyverse)

flights = nycflights13::flights
#5.2.4#

#1#

filter(flights, arr_delay >= 120)
filter(flights, dest == "IAH" | dest == "HOU" )
filter(flights, carrier %in% c("AA", "DL", "UA"))
filter(flights, month >= 7, month <= 9)
filter(flights, arr_delay > 120, dep_delay <= 0)
filter(flights, dep_delay >= 60, dep_delay - arr_delay > 30)
filter(flights, dep_time <= 600 | dep_time == 2400)

#2#

filter(flights, between(month, 7, 9))

#3#

filter(flights, is.na(dep_time))

#4#
#NA ^ 0 == 1, because anything by the power of 0 is 1
#NA | TRUE == TRUE because anything that is "OR TRUE" is TRUE
#NA & FALSE is FALSE, because anything "AND FALSE" is FALSE
#NA * 0 is NAN (not a number) and is undefined in the the world of R, because we just don't know what infinity * 0 is

#5.3.1#

#1#

arrange(flights, desc(is.na(dep_time)), dep_time)

#2#

arrange(flights, desc(dep_delay)) 
arrange(flights, dep_delay)

#3#

head(arrange(flights, air_time))

#4#

arrange(flights, desc(distance))
arrange(flights, distance)

#5.4.1

#1#

select(flights, dep_time, dep_delay, arr_time, arr_delay)
select(flights, "dep_time", "dep_delay", "arr_time", "arr_delay")
select(flights, 4, 6, 7, 9)

#2#

#select ignores diplicates without an error, so effectively nothing happens

#3#

#any_of() selects variables from a vector insted of random variable names and can be
#useful because we can select the variables from this vector
vars <- c("year", "month", "day", "dep_delay", "arr_delay")
select(flights, one_of(vars))

#4#

#it did surprise me but after a bit of reading - contains() ignores case and we can turn that off with 
#ignore.case = FALSE

#5.5.2#

#1#

#Still thinking about these questions



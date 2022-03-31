#####Problem 1#####
# Write a function, which uses a loop to calculate factorial.
# The base R function is called factorial and you should replicate its result.
# This is a function, which takes two numbers, multiplies them and returns
# the result as output:
# MultiplicationFunction <- function(inputNumber1, inputNumber2){
#   Result <- inputNumber1 * inputNumber2
#   return(Result)
# }
# MultiplicationFunction(5, 3)
# 
# Write a factorial function:

# FactorialFunction <- function(inputNumber){
#   ???
#     return(Result)
# }
#####Problem 1#####

FactorialFunction = function(x){
  
  Result = 1
  
  if ((x==0)|(x==1))
    Result <- 1
  
  else{
    for( i in 1:x)
      Result = Result * i
  }
  return (Result)
}

FactorialFunction(4)

#####Problem 2#####
#Write a function which takes a vector and returns its standard deviation.
#You should get the same results as the sd() function.
# SDFunction <- function(inputVector){
#   ???
#     return(Result)
# }
# ??? is not Result <- sd(inputVector)
#####Problem 2#####
vec = 1:5

SDFunction <- function(x){
  Result = sqrt(sum((x - mean(x)) ^ 2 / (length(x) - 1)))
    return(Result)
}
SDFunction(vec)
#####Problem 3#####
# Read everything from https://r4ds.had.co.nz/transform.html, 
# in particular chapters 5.6/5.7

#Do all the exercises:
# 5.6.7 Exercises 
# 5.7.1 Exercises
#####Problem 3#####
library(nycflights13)
library(tidyverse)
#5.6.1#
#I think delays in arrival are far worst then departure delay because if the trip of a passenger includes a few flights one
# after another it could ruin their entire planner trip and put them in a tough, costly position.

#5.6.2#
not_cancelled %>%
  group_by(tailnum) %>%
  tally()

#5.6.3#
#Because a flight might not arrive for many other reasons then just being cancelled - it might have crashed as an example.
#arr_delay is the most important column here.

#5.6.4#
#The more flights there are per day, the more cancelled flights there are and the proportion of cancelled flights increases
# with the average delay of flights.

#5.6.5#
flights %>%
group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))
#Frontier Ariline Inc. has the worst delay

#5.6.6#
#it orders the result from count() and we can use it any time we use count followed by the arrange function

#5.7.1#
#lead,lag,mean,min_rank and row_number work in all groups when group_by is used in mutate or filter
#+,-,<,==,%%,%/% and log are not affected by group_by

#5.7.2#
flights %>%
  filter(!is.na(tailnum)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), n = n()) %>%
  filter(min_rank(on_time) == 1)
#Too many outputs, don't know how to sort

#5.7.3#
flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)
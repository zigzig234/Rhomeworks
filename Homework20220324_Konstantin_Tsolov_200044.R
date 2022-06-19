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
flights %>%
  count(dest, sort = TRUE)
#5.7.1#
#lead,lag,mean,min_rank and row_number work in all groups when group_by is used in mutate or filter
#+,-,<,==,%%,%/% and log are not affected by group_by

#5.7.2#
flights %>%
  filter(!is.na(tailnum), is.na(arr_time) | !is.na(arr_delay)) %>%
  mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
  group_by(tailnum) %>%
  summarise(on_time = mean(on_time), n = n()) %>%
  filter(n >= 20) %>%
  filter(min_rank(on_time) == 1)
#Too many outputs, don't know how to sort(update - problem solved)

#5.7.3#
flights %>%
  group_by(hour) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(arr_delay)

#5.7.4#
flights %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_total = sum(arr_delay),
    arr_delay_prop = arr_delay / arr_delay_total
  ) %>%
  select(dest, month, day, dep_time, carrier, flight,
         arr_delay, arr_delay_prop) %>%
  arrange(dest, desc(arr_delay_prop))

#5.7.5#
delayed_flights <- flights %>%
  arrange(origin, month, day, dep_time) %>%
  group_by(origin) %>%
  mutate(dep_delay_lag = lag(dep_delay)) %>%
  filter(!is.na(dep_delay), !is.na(dep_delay_lag))

#5.7.6#
normal_flights <- flights %>%
  filter(!is.na(air_time)) %>%
  group_by(dest, origin) %>%
  mutate(
    air_time_mean = mean(air_time),
    air_time_sd = sd(air_time),
    n = n()
  ) %>%
  ungroup() %>%
  mutate(air_time_standard = (air_time - air_time_mean) / (air_time_sd + 1)
#5.7.7#
flights %>%
  group_by(dest) %>%
  dplyr::filter(n_distinct(carrier) > 2) %>%
  group_by(carrier) %>%
  dplyr::summarise(n = n_distinct(dest)) %>%
  arrange(-n)
 #5.7.8#
 not_cancelled %>%
  group_by(origin, tailnum) %>%
  dplyr::summarise(
    count = n(),
    agg_dep_delay = sum(cumsum(dep_delay > 60) < 1)
  )
#####Problem 4##### ----
#Find the following:
#4.1 For each carrier what is the most common destination?
#4.2 For each carrier what is the biggest delay?
#4.3 Which are the three plane which have flown the most/least miles?
#4.4 What are the first/last flights for each day in February 2013?
#4.5 Which company flew the most miles in March 2013? Which flew the least?
#4.6 Which month had the most delays over 60 minutes?
#4.7 What is the average time between two consecutive flights?
#4.8 Use the SDFunction function from exercise 2 to calculate the standard deviation
#of the flight delays for each month and for each destination.
#####Problem 4#####


library(tidyverse)
library(tidyquant)
library(nycflights13)
View(flights)

#4.1#
common_dest <- flights %>% 
  group_by(carrier, dest) %>%
----това трябва ли?----  select(carrier, dest) %>%
  summarise(NrOfFlights = n()) %>%
  arrange(desc(NrOfFlights)) %>%
  slice_head()

#4.2#
biggest_delay <- flights %>%
  arrange(desc(arr_delay)) %>%
  group_by(carrier) %>%
  slice_head()

#4.3#
most/least_miles <- not_cancelled %>%
  dplyr::group_by(tailnum) %>%
  dplyr::summarise(TotalDistance = base::sum(distance))  %>%
  dplyr::arrange(dplyr::desc(TotalDistance))%>%
  dplyr::slice(1:3, (n()-2):n())%>%
  dplyr::ungroup()

#4.4#
sss <- flights %>%
  filter(year == 2013,
         month == 2,
         !is.na(dep_time)) %>%
  arrange(day, dep_time) %>%
  group_by(day) %>%
  filter(row_number() == 1 |
         row_number() == n()) %>%
  ungroup()

#4.5#
most/least_miles_company <- not_cancelled %>%
  dplyr::filter(month == 3) %>%
  dplyr::group_by(carrier)%>%
  dplyr::mutate(TotalMiles = base::sum(distance))%>%
  dplyr::ungroup()%>%
  dplyr::arrange(dplyr::desc(TotalMiles))%>%
  dplyr::slice(1, n())

#4.6#
most_delays_month <-not_cancelled %>%
  dplyr::group_by(month)%>%
  dplyr::summarise(TotalDelay = base::sum(arr_delay>60))%>%
  dplyr::ungroup()%>%
  dplyr::slice_max(TotalDelay)

#4.7#
average_time <- not_cancelled %>%
  dplyr::summarise(TimebetweenFlights = base::mean(dep_time - dplyr::lag(dep_time), na.rm = TRUE))%>%
  base::round(digits = 3)
  
#4.8#
SDFunction <- function(inputVector){
  Denominator = sum(inputVector)/length(inputVector)
  Nominator = sum((inputVector - Denominator)^2)
  Result <- sqrt(sum((inputVector - Denominator)^2)/(length(inputVector)-1))
  return(Result)
}  

n <- not_cancelled %>%
  dplyr::group_by(month,dest) %>%
  dplyr::summarise(sdDelay = SDFunction(arr_delay))%>%
  dplyr::ungroup()          

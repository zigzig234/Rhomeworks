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
prime_numbers <- function(n) {
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
prime_numbers(150)

#No time for problem 3, will try to do it later

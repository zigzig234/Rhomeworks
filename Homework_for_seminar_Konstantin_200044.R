#####Problem 1#####
# Write a loop which iterates over all the numbers from 1 to 10 and prints
# them multiplied by 3
#####Problem 1#####

for (x in 1:10) {
  x <- x * 3
  print (x)
}

#####Problem 2#####
# Write a loop which chooses 10 random numbers, one at a time from a normal
# distribution (use rnorm and see the help ?rnorm) and prints the number
# if it is bigger than 1.
#####Problem 2#####

for (i in rnorm(10)) {
  if (i > 1)
  print(i)
}

#####Problem 3#####
# What is the probability that out of a group of 6 men and 8 women, if we pick
# 5 people at random, exactly 3 will be men?
# Use a for loop, which simulates the picking.
#####Problem 3#####
a = c(rep("Men",6), rep("Women", 8))
b = NULL
for (i in 1:10000) {
  c = sample(people, size = 5, replace = FALSE)
  
  if (sum(choose=="Men")==3) {
    b = c(b,1)
    } else {
    b = c(b,0)
  }
#####Problem 4#####
# Calculate the price of a european style option with strike price of 120, with an
# expiration date in 100 days.
# The underlying stock has a starting price of 100, which will change every 
# day based on a random number coming from a normal distribution with 
# mean = 0 and standard deviation = 7. This is rnorm(1, mean = 0, sd = 5) 
# The starting price was 100 on day 0.
# On day 1 it is 100 + rnorm(1, mean = 0, sd = 7). 
# On day 2 it is price from day 1 + rnorm(1, mean = 0, sd = 7) etc.
# On day 100 it is price from day 99 + rnorm(1, mean = 0, sd = 7).
# 
# European style options can only be exercised at expiry date. 
# So you must calculate the price of the underlying asset at day 100 and 
# compare it to the strike price. Then calculate the potential profit. 
# Because it is a call option the profit will range from 0 to infinity.
# Make 1000 simulations of the profit and the price of the call option,
# will be equal to the sum of all the profits divided by the number of 
# simulations(in this case 1000).
#####Problem 4#####
Prce = 100
Prf = 0
Stri = 120
for (j in 1:1000) {
for (i in 1:100) {
    Prce = c(Prce + rnorm(1, mean = 0, sd = 7))
  }
if(Prce > 120){
  Prf = Prf + Prce - 120
}
}
ans = Prce/1000



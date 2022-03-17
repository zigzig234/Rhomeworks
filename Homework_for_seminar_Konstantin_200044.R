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
x = NULL
for (i in 1:5) {
  x = c(x,sample(c("Man","Man","Man","Man","Man","Man","Wonan","Wonan","Wonan","Wonan","Wonan","Wonan","Wonan","Wonan"),1))
}
#This is as far as i could get#
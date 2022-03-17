# 6 types of vectors are numeric, integer, factor, ordered, character and raw.
a=c(10,20,30)
b=c("I","love","pizza")
c=1:5
d=c(TRUE, FALSE)
e=logical(1)
#Selecting elements of a vector
a[1]
a[1:2]
a[c(1,2,3)]
a[-1]
a[a>15]
#making a list
Food=list(b)
Food
Numbers=list(a,c)
Numbers[[2]]
#Generating a data.frame
x1=1:100
x2=100:200
data=data.frame(x1=x1[x1>50],x2=x2[x2<150])
View(data)

#***Getting around***

#get help when you know the function name
?data.frame

#get help when you know the subject area
help.search("data input")

#find which package the function is in, the package must be loaded
find("glm")

#find all possible items which atleast partially matches the search item
apropos("lm")

#worked examples of functions
example("lm")

#demo of R code
demo(graphics)

#use a package
library(glmnet)

#contents of a package
library(help="glmnet")

#install package
install.packages("car")

#to see variables created in a session
objects()

#to see which packages are currently attached
search()

#remove an object called 'a'
rm(a)

#remove all objects
rm(list=ls())

#turn off scientific notation
options(scipen=999)

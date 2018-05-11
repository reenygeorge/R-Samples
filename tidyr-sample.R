#**** tidyr help ****#

library(tidyr)
library(dplyr)

#tidyr provides three main functions for tidying your messy data: gather(), separate() and spread().

#e.g. for gather

messy <- data.frame(
  name = c("Wilbur", "Petunia", "Gregory"),
  a = c(67, 80, 64),
  b = c(56, 90, 50)
)
messy
#>      name  a  b
#> 1  Wilbur 67 56
#> 2 Petunia 80 90
#> 3 Gregory 64 50

#We have three variables (name, drug and heartrate), but only name is currently in a column. 
# We use gather() to gather the a and b columns into key-value pairs of drug and heartrate:
# gather() is like melt() in reshape2
messy %>%
  gather(drug, heartrate, a:b)
#>      name drug heartrate
#> 1  Wilbur    a        67
#> 2 Petunia    a        80
#> 3 Gregory    a        64
#> 4  Wilbur    b        56
#> 5 Petunia    b        90
#> 6 Gregory    b        50

#Sometimes two variables are clumped together in one column. separate() allows you to tease them apart 
#In the below e.g. We have some measurements of how much time people spend on their phones, 
#   measured at two locations (work and home), at two times. Each person has been randomly 
#   assigned to either treatment or control.
set.seed(10)
messy <- data.frame(
  id = 1:4,
  trt = sample(rep(c('control', 'treatment'), each = 2)),
  work.T1 = runif(4),
  home.T1 = runif(4),
  work.T2 = runif(4),
  home.T2 = runif(4)
)
#id       trt    work.T1   home.T1   work.T2    home.T2
#  1 treatment 0.08513597 0.6158293 0.1135090 0.05190332
#  2   control 0.22543662 0.4296715 0.5959253 0.26417767
#  3 treatment 0.27453052 0.6516557 0.3580500 0.39879073
#  4   control 0.27230507 0.5677378 0.4288094 0.83613414

#To tidy this data, we first use gather() to turn columns work.T1, home.T1, work.T2 and home.T2 into a key-value pair.
tidier <- messy %>%
  gather(key, time, -id, -trt)
tidier %>% head(8)
#>   id       trt     key    time
#> 1  1 treatment work.T1 0.08514
#> 2  2   control work.T1 0.22544
#> 3  3 treatment work.T1 0.27453
#> 4  4   control work.T1 0.27231
#> 5  1 treatment home.T1 0.61583
#> 6  2   control home.T1 0.42967
#> 7  3 treatment home.T1 0.65166
#> 8  4   control home.T1 0.56774

#Next we use separate() to split the key into location and time, using a regular expression to describe 
#     the character that separates them.
tidy <- tidier %>%
        separate(key, into = c("location", "time"), sep = "\\.")
tidy %>% head(8)
#>   id       trt location time    time
#> 1  1 treatment     work   T1 0.08514
#> 2  2   control     work   T1 0.22544
#> 3  3 treatment     work   T1 0.27453
#> 4  4   control     work   T1 0.27231
#> 5  1 treatment     home   T1 0.61583
#> 6  2   control     home   T1 0.42967
#> 7  3 treatment     home   T1 0.65166
#> 8  4   control     home   T1 0.56774

#The last tool, spread(), takes two columns (a key-value pair) and spreads them in to multiple columns, making “long” data wider.
# spread() is like cast() in reshape2

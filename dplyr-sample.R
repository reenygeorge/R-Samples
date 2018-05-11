# Help code for dplyr package #

library(dplyr)

#Important dplyr functions
# select(): select columns
# filter(): filter rows
# group_by(): allows for group operations in the split-apply-combine concept
# summarise(): summarise values
# arrange(): re-order or arrange rows
# join(): 
# mutate(): create new columns
#top_n(): choose n first/last rows


data = data.frame(gender = c("M", "M", "F"),
                  age = c(20, 60, 30),
                  height = c(180, 200, 150))

##   gender age height
## 1      M  20    180
## 2      M  60    200
## 3      F  30    150

select(data, age)
##   age
## 1  20
## 2  60
## 3  30

select(data, -height)
##   gender age
## 1      M  20
## 2      M  60
## 3      F  30

filter(data, height > 160)
##   gender age height
## 1      M  20    180
## 2      M  60    200

arrange(data, height)
##   gender age height
## 1      F  30    150
## 2      M  20    180
## 3      M  60    200

mutate(data, height2 = height / 100)
##   gender age height height2
## 1      M  20    180     1.8
## 2      M  60    200     2.0
## 3      F  30    150     1.5


summarise(data, average_height = mean(height))
##   average_height
## 1       176.6667

grouped_data = group_by(data, gender)
summarise(grouped_data, average_height = mean(height))
## # A tibble: 2 <U+00D7> 2
##   gender average_height
##   <fctr>          <dbl>
## 1      F            150
## 2      M            190

summarise(grouped_data,
          average_height = mean(height),
          nr_of_people = n())
## # A tibble: 2 <U+00D7> 3
##   gender average_height nr_of_people
##   <fctr>          <dbl>        <int>
## 1      F            150            1
## 2      M            190            2

#useful functions
# distinct(): separate unique values
# sample_n(): draw n random values from the selected column
# n(): count the number of rows
# n_distinct(): count the number of unique values

#top 1 by height
top_n(data, 1, height)
##   gender age height
## 1      M  60    200

# bottom 2 by height
top_n(data, 2, -height)
##   gender age height
## 1      M  20    180
## 2      F  30    150

#sort the data by height and select only the rows where gender == “M”.
sorted = arrange(data, height)
filter(sorted, gender == "M")






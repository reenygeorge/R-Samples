# Help code for ggplot2 package #

library(ggplot2)

#The principal components of every plot can be defined as follow:
  
#data: is a data frame
#Aesthetics: is used to indicate x and y variables. It can also be used to control the color, the size or the shape of points, the height of bars, etc…..
#Geometry: corresponds to the type of graphics (histogram, box plot, line plot, density plot, dot plot, ….)

mpg
#> # A tibble: 234 × 11
#>   manufacturer model displ  year   cyl      trans   drv   cty   hwy    fl
#>          <chr> <chr> <dbl> <int> <int>      <chr> <chr> <int> <int> <chr>
#> 1         audi    a4   1.8  1999     4   auto(l5)     f    18    29     p
#> 2         audi    a4   1.8  1999     4 manual(m5)     f    21    29     p
#> 3         audi    a4   2.0  2008     4 manual(m6)     f    20    31     p
#> 4         audi    a4   2.0  2008     4   auto(av)     f    21    30     p
#> 5         audi    a4   2.8  1999     6   auto(l5)     f    16    26     p
#> 6         audi    a4   2.8  1999     6 manual(m5)     f    18    26     p
#> # ... with 228 more rows, and 1 more variables: class <chr>

# With ggplot2, you begin a plot with the function ggplot(). 
# ggplot() creates a coordinate system that you can add layers to. 
# The first argument of ggplot() is the dataset to use in the graph. 
# So ggplot(data = mpg) creates an empty graph.
# The function geom_point() adds a layer of points to your plot, which creates a scatterplot. 
# ggplot2 comes with many geom functions that each add a different type of layer to a plot.
# Each geom function in ggplot2 takes a mapping argument. This defines how variables in your dataset are mapped to visual properties. 
# The mapping argument is always paired with aes(), and the x and y arguments of aes() 
# specify which variables to map to the x and y axes.

#ggplot(data = <DATA>) + 
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))
p <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
print(p)

p <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
print(p)

#other aesthetics one can use here would be shape (will only display 6 shapes, any more categories will go unplotted), alpha(transparency)
p <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
print(p)

#to make all points coloured blue
p <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
print(p)


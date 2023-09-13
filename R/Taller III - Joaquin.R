library(tidyverse)

iris[,1:4] %>% 
  apply(2,mean) %>% 
  sum() %>% 
  exp()

iris %>% 
  split(iris$Species) %>% 
  lapply( FUN = function(x) apply(x[,1:4],2,mean) 
            )
x = 3:5
y <- 3:5

class(x)

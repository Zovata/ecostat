library(tidyverse)
data("iris")
hist(iris$Sepal.Length)

head(iris)
ggplot(iris, aes(Sepal.Length, Petal.Width)) +
  geom_point()

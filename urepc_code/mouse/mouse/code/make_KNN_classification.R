data(iris)
library(class)

fitKmeans <- kmeans(
  x = iris[,-5],
  centers= 5,
  nstart = 20
)



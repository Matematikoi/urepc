library("mixtools")
library("tidyverse")

faithful_vector <- matrix(unlist(faithful, use.names=FALSE),ncol = 2)

centers <- matrix(c(4,80,1,50), 2, 2, byrow = TRUE)

clusters_parametric <- mvnormalmixEM(faithful_vector, lambda = c(.5,.5))#, mu = centers)

plot(clusters_parametric,whichplots = 2)
library("mixtools")
library("tidyverse")
###### Section 4:  Nonparametric and Semiparametric Methods
#attach(faithful)
faithful_vector <- matrix(unlist(faithful, use.names=FALSE),ncol = 2)
centers <- matrix(c(4,80,1,50), 2, 2, byrow = TRUE)
wait1 <- npEM(faithful_vector,  centers)

#wait1 <- npEM(waiting, centers, stochastic = TRUE, maxiter = 500)
#wait1 <- multmixEM(faithful_vector, lambda = c(.5,.5), theta = centers)

plot(wait1)

#density_object <- density(wait1)

#ggplot(faithful,aes(waiting,eruptions))+
#  geom_jitter()+
#  labs(
#    title = "Faithful data",
#    subtitle = "Data is clearly separated into two clusters"
#    )
  


library("mixtools")
library("tidyverse")


faithful_vector <- matrix(unlist(faithful, use.names=FALSE),ncol = 2)

centers <- matrix(c(4,80,1,50), 2, 2, byrow = TRUE)

clusters_non_parametric <- npEM(
  faithful_vector,  
  mu0 = 2, 
  samebw = FALSE,
  eps = 1e-8,
  maxiter = 5000)

plot(clusters_non_parametric,whichplots = 2)


df = data.frame(
  data1 = clusters_non_parametric[["data"]][,1],
  data2 = clusters_non_parametric[["data"]][,2],
  prob1 = clusters_non_parametric[["posteriors"]][,1],
  prob2 = clusters_non_parametric[["posteriors"]][,2]
)


df %>%
  mutate(
    label = (prob1 > prob2)+0
  )%>%
  ggplot(mapping = aes(x = data1, y=data2))+
  geom_jitter(aes(color = label))









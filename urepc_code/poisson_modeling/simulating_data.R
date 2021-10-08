library("ggplot2")


#lambda <- c(2000,1000,2,1,50,30,200,100)
#lambda <- matrix(lambda, nrow =2 , byrow = FALSE)
#pi <- c(0.1,0.3,0.3,0.3)
dimensions <- 2
clusters <- 9

lambda <- matrix(rexp(dimensions*clusters, rate = 0.01), dimensions)
pi <- runif(clusters)
pi <- pi/sum(pi)


make_poisson_sim <- function(pi, lambda, n) {
  dimension <- dim(lambda)[1]
  clusters <- dim(lambda)[2]
  replicates <- 2
  data <- data.frame(matrix(ncol = dimension*replicates, nrow = 0))
  labels <- c()
  
  for (cluster in 1:clusters){
    size  <-  round(n*pi[cluster])
    data_aux <- data.frame(matrix(ncol = dimension*replicates, nrow = size))
    labels <- append( labels, integer(size) + cluster )
    print(size)
    for (i in 1:dimension){
      for (j in 1:replicates){
        data_aux[(i-1)*dimension + j] <- rpois(size, lambda[i,cluster])
      }
    }
    data <- rbind(data, data_aux)
  }
  
  return(list(points = data, labels = labels))
}

data = make_poisson_sim(pi = pi , lambda = lambda, n = 15000)

write.csv(data$points,file = "./data/simulated_data_points.csv")
write.csv(data$labels,file = "./data/simulated_data_labels.csv")
write.csv(lambda,file = "./data/simulated_data_lambda.csv")
write.csv(pi,file = "./data/simulated_data_mixing_proportions.csv")

ggplot(data, aes(X1))+
  geom_histogram(binwidth = 1.0);

ggplot(data, aes(X2))+
  geom_histogram(binwidth = 1.0)

ggplot(data, aes(X3))+
  geom_histogram(binwidth = 1.0)













pi <- c(0.10610058, 
        0.03684517, 
        0.04976060, 
        0.01842075, 
        0.09419650, 
        0.02010680, 
        0.09981937, 
        0.03915697, 
        0.07753327, 
        0.04751063, 
        0.09836298, 
        0.09953579, 
        0.11170035, 
        0.10095024)
lambda <- c(
  1.773777654 ,
  0.010078354 ,
  0.005137889 ,
  2.272762962 ,
  1.638826746 ,
  0.182725915 ,
  0.091246787 ,
  2.162600745 ,
  1.232976397 ,
  0.701944896 ,
  0.236632677 ,
  1.976603335 ,
  1.390032557 ,
  0.501017289 ,
  0.409600005 ,
  1.755319996 ,
  0.719334606 ,
  1.359065357 ,
  0.590920170 ,
  1.523350574 ,
  1.125176224 ,
  0.839857544 ,
  1.023280700 ,
  0.970216161 ,
  0.943437052 ,
  1.072363018 ,
  0.833770083 ,
  1.212663926
)
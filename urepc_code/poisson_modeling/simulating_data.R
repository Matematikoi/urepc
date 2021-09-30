library("ggplot2")
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


lambda <- c(20.0,20.0,2.0,2.0)
pi <- c(0.5,0.5)
lambda <- matrix(lambda, nrow =2 , byrow = FALSE)

make_poisson_sim <- function(pi, lambda, n) {
  dimension = dim(lambda)[1]
  clusters = dim(lambda)[2]
  data <- data.frame(matrix(ncol = dimension, nrow = 0))
  
  for (cluster in 1:clusters){
    data_aux <- data.frame(matrix(ncol = dimension, nrow = n))
    size = round(n*pi[cluster])
    for (i in 1:dimension){
      data_aux[i] <- rpois(n, lambda[i,cluster])
    }
    data <- rbind(data, data_aux)
  }
  
  return(data)
}

data = make_poisson_sim(pi = pi , lambda = lambda, n = 1000)

ggplot(data, aes(X1))+
  geom_histogram()

ggplot(data, aes(X2))+
  geom_histogram(binwidth = 1.0)

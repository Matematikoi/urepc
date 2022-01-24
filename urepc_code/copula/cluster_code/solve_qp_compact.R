library(quadprog)
library(parallel)
library(foreach)
library(doParallel)



input_name <- 'cluster_in_data/parameter_1_1_.RDS'
output_name <- 'cluster_out_data/opt_1_.RDS'


invisible(eval(parse(text=commandArgs(TRUE))))



data <- readRDS(input_name)

registerDoParallel(data$cores)

Dmat <- diag(2,data[['n']])
Amat <- cbind(t(data[['M']]),diag(1,data[['n']]))
n <- data$n
cols_n <- n+3

Aind <- matrix(nrow = n+1, ncol = cols_n <- n+3)
Aind[,] <- -8
Aind[1,4:cols_n] <- 1
Aind[2,4:cols_n] <- seq(cols_n-3)
Aind[1,1:3] <- n
Aind[1:n+1,1:3] <- seq(n)
Amat[2,1:n+3] <- 1
Amat[1,1:n+3] <- 1
kweightshat <- solve.QP.compact(
  Dmat,
  data[['dvec']],
  Amat,
  Aind,
  data[['bvec']],
  data[['meq']])$solution

#add cores integer
#add mean --- initial sampleOfG --- later xtilde[,j]
#add h
#add sigma
#add mu
#add x 
sigma <- data$sigma
mu <- data$mu
mean <- data$mean
j <- data$j
z <- data$z
h <- data$h
x <- data$x

g <- foreach( i = 1:n, .combine = c )%do%{
  (1/sigma[j,z])*t(kweightshat)%*%
    dnorm(mean, mean=(x[i,j]-mu[j,z])/sigma[j,z], sd=h)
}
G <-foreach(i = 1:n, .combine = c)%dopar%{
  t(kweightshat)%*%
    pnorm((x[i,j]-mu[j,z])/sigma[j,z], mean=mean, sd=h)
}


saveRDS(list(g=g,G=G), output_name)

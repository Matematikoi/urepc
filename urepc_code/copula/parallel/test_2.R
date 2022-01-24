library(parallel)
# library(doParallel)
# library(foreach)
library(copula)
library(quadprog)
# library(abind)

# Get the number of cores to use
# no_cores <- max(1, detectCores()-1)
# # Make cluster object using no_cores
# cl <- makeCluster(no_cores)
# # Initialize cluster for parallel computing
# registerDoParallel(cl)
# mbind <- function(...) abind(..., along=3)
# numCores <- detectCores()
numCores <- 15
numCores_2 <- 4



x <- readRDS(file = 'data_for_tests/x.RDS')
mu <- readRDS(file = 'data_for_tests/mu.RDS')
sigma <- readRDS(file = 'data_for_tests/sigma.RDS')
d <- readRDS(file = 'data_for_tests/d.RDS')
K <- readRDS(file = 'data_for_tests/K.RDS')
n <- readRDS(file = 'data_for_tests/n.RDS')
km <- readRDS(file = 'data_for_tests/km.RDS')
G <- array(dim=c(n,d,K))
g <- array(dim=c(n,d,K))


# hacer una funcion con mclappply



# time_serial <- system.time(
# for(j in 1:d){
#   print("current dimension")
#   print (j)
#   sampleOfG <- NA
#   for(z in 1:K){
#     sigma[j,z] <- sd(x[km$cluster==z,j])
#     sampleOfG <- c(sampleOfG, (x[km$cluster==z,j]-mu[j,z])/sigma[j,z])
#   }
#   sampleOfG <- sampleOfG[-1]
# 
#   for (z in 1:K){
#     print('serial #')
#     print(z)
#     res <- solve_column_g(K,n,x,z,sampleOfG,mu,sigma,j)
#     G[,j,z] <-  res[,1]
#     g[,j,z] <- res[,2]
#   }
# 
# }
# )
# print('serial time')
# print(time_serial)
# print(g[503,,])
solve_column_g <- function(K,n,x,z,sampleOfG,mu,sigma,j){
  solution <- array(dim = c(n,2))
  h <- bw.nrd(sampleOfG)
  M <- rbind(rep(1,length(sampleOfG)), sampleOfG, sampleOfG^2)
  Amat <- cbind(t(M),diag(1,length(sampleOfG)))
  bvec <- c(1,0,1-h^2,rep(0,length(sampleOfG)))
  dvec <- rep(0,length(sampleOfG))
  Dmat <- diag(2,length(sampleOfG))
  kweightshat <- solve.QP(Dmat, dvec, Amat, bvec, meq=3)$solution # 'k' like kernel
  
  for(i in 1:n){
    for(l in 1:2){
      if ( l == 2){
        solution[i,l] <- (1/sigma[j,z])*t(kweightshat)%*%
          dnorm(sampleOfG, mean=(x[i,j]-mu[j,z])/sigma[j,z], sd=h)
      }else{
        solution[i,l] <- t(kweightshat)%*%
          pnorm((x[i,j]-mu[j,z])/sigma[j,z], mean=sampleOfG, sd=h)
      }
    }
  }
  return( solution)
}
across_dimensions <- function(j,K,n,x,mu,sigma,km){
  sampleOfG <- NA
  for(z in 1:K){
    sigma[j,z] <- sd(x[km$cluster==z,j])
    sampleOfG <- c(sampleOfG, (x[km$cluster==z,j]-mu[j,z])/sigma[j,z])
  }
  sampleOfG <- sampleOfG[-1]
  
  # res <- mclapply(
  #   1:K, 
  #   function(para) {solve_column_g(K,n,x,para,sampleOfG,mu,sigma,j)}, 
  #   mc.cores = numCores
  # )
  res <- mclapply(
    1:K,
    solve_column_g,
    mc.cores = 17,
    K= K,
    n = n,
    x = x,
    sampleOfG = sampleOfG,
    mu = mu,
    sigma = sigma,
    j = j
  )
  res_format <- array(dim = c(K,n,2))
  for (z in 1:K){
    res_format[z,,] <- res[[z]]
  }
  return(res_format)
}

across_dimensions_aux <- function (param){across_dimensions(param,K,n,x,mu,sigma,km)}

time_sup_parallel <- system.time(
  result <- mclapply(1:d,across_dimensions_aux, mc.cores = numCores_2)
)
for(i in 1:d){
  G[,i,] <- result[[i]][,,1]
  g[,i,] <- result[[i]][,,2]
}


# time_parallel <- system.time(
#   for(j in 1:d){
#     print("current dimension")
#     print (j)
#     sampleOfG <- NA
#     for(z in 1:K){
#       sigma[j,z] <- sd(x[km$cluster==z,j])
#       sampleOfG <- c(sampleOfG, (x[km$cluster==z,j]-mu[j,z])/sigma[j,z])
#     }
#     sampleOfG <- sampleOfG[-1]
# 
#     aux_fun <- function(para) {solve_column_g(K,n,x,para,sampleOfG,mu,sigma,j)}
#     res <- mclapply(1:K, aux_fun, mc.cores = numCores)
# 
#     for (z in 1:K){
#       G[,j,z] <- res[[z]][,1]
#       g[,j,z] <- res[[z]][,2]
#     }
#   }
# )
# print('parallel time, super parallel')
# print(time_parallel)
# print(time_sup_parallel)
# print(g[503,,])

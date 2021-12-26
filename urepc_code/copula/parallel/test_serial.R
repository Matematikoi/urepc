library(copula)
library(quadprog)

x <- readRDS(file = 'data_for_tests/x.RDS')
mu <- readRDS(file = 'data_for_tests/mu.RDS')
sigma <- readRDS(file = 'data_for_tests/sigma.RDS')
d <- readRDS(file = 'data_for_tests/d.RDS')
K <- readRDS(file = 'data_for_tests/K.RDS')
n <- readRDS(file = 'data_for_tests/n.RDS')
km <- readRDS(file = 'data_for_tests/km.RDS')
G <- array(dim=c(n,d,K))
g <- array(dim=c(n,d,K))

time_taken <- system.time({
  for(j in 1:d){
  sampleOfG <- NA
  for(z in 1:K){
    sigma[j,z] <- sd(x[km$cluster==z,j])
    sampleOfG <- c(sampleOfG, (x[km$cluster==z,j]-mu[j,z])/sigma[j,z])
  }
  sampleOfG <- sampleOfG[-1]
  # xtildetrack[,j,1] <- sampleOfG
  for(z in 1:K){
    cat("\n Initializing G&g.\n K: ",z,"\n d: ",j,'\n')
    h <- bw.nrd(sampleOfG)
    M <- rbind(rep(1,length(sampleOfG)), sampleOfG, sampleOfG^2)
    ## MMtinv <- solve(M%*%t(M),diag(rep(1,3))) # explicit solution whenever
    ## kweightshat <- t(M)%*%MMtinv%*%c(1,0,1-h^2) # p>=0 not taken into account
    Amat <- cbind(t(M),diag(1,length(sampleOfG)))
    bvec <- c(1,0,1-h^2,rep(0,length(sampleOfG)))
    dvec <- rep(0,length(sampleOfG))
    Dmat <- diag(2,length(sampleOfG))
    kweightshat <- solve.QP(Dmat, dvec, Amat, bvec, meq=3)$solution # 'k' like kernel
    for(i in 1:n){
      g[i,j,z] <- (1/sigma[j,z])*t(kweightshat)%*%
        dnorm(sampleOfG, mean=(x[i,j]-mu[j,z])/sigma[j,z], sd=h)
      G[i,j,z] <- t(kweightshat)%*%
        pnorm((x[i,j]-mu[j,z])/sigma[j,z], mean=sampleOfG, sd=h)
    }
  }
  }
})

print(time_taken)
  
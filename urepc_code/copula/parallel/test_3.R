parallel_fun_1 <- function (xtilde,n,j,z,x,mu,sigma){
  solution <- array(dim = c(n,2))
  h <- bw.nrd(xtilde[,j])
  M <- rbind(rep(1,length(xtilde[,j])), xtilde[,j], xtilde[,j]^2)
  Amat <- cbind(t(M),diag(1,n))
  bvec <- c(1,0,1-h^2,rep(1e-17,n)) 
  dvec <- rep(0,n)
  Dmat <- diag(2,n)
  
  kweightshat <- solve.QP(Dmat, dvec, Amat, bvec, meq=3)$solution
  
  for(i in 1:n) solution[i,2] <- (1/sigma[j,z])*t(kweightshat)%*%
    dnorm(xtilde[,j], mean=(x[i,j]-mu[j,z])/sigma[j,z], sd=h)
  for(i in 1:n) solution[i,1] <- t(kweightshat)%*%
    pnorm((x[i,j]-mu[j,z])/sigma[j,z], mean=xtilde[,j], sd=h)

  return( solution)
}

parallel_fun_2 <- function (xtilde,n,x,mu,sigma,z){
  res <- mclapply(
    1:d,
    parallel_fun_1,
    mc.cores = 17,
    n = n,
    x = x,
    xtilde = xtilde,
    mu = mu,
    sigma = sigma,
    z = z
  )
  res_format <- array(dim = c(K,n,2))
  for (z in 1:K){
    res_format[z,,] <- res[[z]]
  }
  return(res_format)
}

result <- mclapply(
  1:K,
  parallel_fun_2, 
  mc.cores = numCores_2,
  xtilde = xtilde,
  n = n,
  x = x,
  mu = mu,
  sigma = sigma
  )
for(i in 1:d){
  newG[,i,] <- result[[i]][,,1]
  newg[,i,] <- result[[i]][,,2]
}


for(z in 1:K){
  for(j in 1:d){
    h <- bw.nrd(xtilde[,j])
    M <- rbind(rep(1,length(xtilde[,j])), xtilde[,j], xtilde[,j]^2)
    Amat <- cbind(t(M),diag(1,n))
    bvec <- c(1,0,1-h^2,rep(1e-17,n)) 
    dvec <- rep(0,n)
    Dmat <- diag(2,n)
    
    kweightshat <- solve.QP(Dmat, dvec, Amat, bvec, meq=3)$solution
    
    
    for(i in 1:n) newg[i,j,z] <- (1/sigma[j,z])*t(kweightshat)%*%
      dnorm(xtilde[,j], mean=(x[i,j]-mu[j,z])/sigma[j,z], sd=h)
    for(i in 1:n) newG[i,j,z] <- t(kweightshat)%*%
      pnorm((x[i,j]-mu[j,z])/sigma[j,z], mean=xtilde[,j], sd=h)
  
  }
}
## Returns the rows of 'data' whose at least one element is below 'tol'
findOutliers <- function(data,tol){
  where <- (abs(as.matrix(data))<=tol)
  is.outlier <- (apply(where,1,sum)>=1)
  is.outlier
}
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

parallel_fun_2 <- function (xtilde,n,x,mu,sigma,z,d,K){
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
  res_format <- array(dim = c(d,n,2))
  for (z in 1:d){
    res_format[z,,] <- res[[z]]
  }
  return(res_format)
}
## It is assumed that the support of G is (-\infty,+\infty) and that
## G is centered at 0. The copulas must have only one parameter.
## effInterval is the effective support of G (from a computer
## point of view).
## Q is either a list of the quantile functions G^{-1} either
## a list of (n*1)-vectors : (\tilde x_1,...,\tilde x_n) where the
## \tilde x_j are a sample of G_j.
simul <- function(
  n, 
  Q, 
  mu,
  sigma,
  pz,
  theta,
  copulaFamilies
){
  
  d <- nrow(mu)
  K <- ncol(mu)
  x <- matrix(nrow=n,ncol=d)
  z <- numeric(K)
  cop <- list()
  intervalTheta <- matrix(nrow=2,ncol=K)
  
  for(z in 1:K){
    if(copulaFamilies[z]=="gaussian"){
      cop[[z]] <- normalCopula(dim=d)
      intervalTheta[,z] <- c(-.99,.99)
    }else if(copulaFamilies[z]=="clayton"){
      cop[[z]] <- claytonCopula(dim=d)
      intervalTheta[,z] <- c(0.001,50)
    }else if(copulaFamilies[z]=="frank"){
      cop[[z]] <- frankCopula(dim=d)
      intervalTheta[,z] <- c(-50,50)
    }else if(copulaFamilies[z]=="indep"){
      cop[[z]] <- indepCopula(dim=d)
      intervalTheta[,z] <- c(0,0)
    }else{
      stop("'copulaFamily' is wrong.")
    }
  }
  
  
  
  
  
  for(i in 1:n){
    z[i] <- sample(1:K,1,prob=pz)
    cop[[z[i]]]@parameters <- theta[z[i]]
    u <-  rCopula(1,cop[[z[i]]])
    for(j in 1:d){
      if(class(Q[[j]])=="function"){
        x[i,j] <- Q[[j]](u[j])*sigma[j,z[i]]+mu[j,z[i]]
      }else{
        x[i,j] <- quantile(Q[[j]], u[j])*sigma[j,z[i]]+mu[j,z[i]]
      }
    }
  }
  
  return( cbind(x,z) )
}

## G: array of size n*d*K representing G_j(y_{ij}-mu_{jz}) for
## j=1,...,d, i=1,...,n and z=1,...,K (same for g);
## y_{ij}=\psi_j^{-1}(x_{ij})
## mu (d*K)-matrix
EMalgo <- function(
  data,
  copulaFamilies,
  nbit, 
  method,
  commonCopula,
  debug = FALSE
){
  
  x <- data
  n <- nrow(x)
  d <- ncol(x)
  K <- length(copulaFamilies)
  km <- kmeans(x,centers=K)
  mu <- t(km$centers)
  mutrack <- array(dim=c(d,K,nbit+1))
  sigma <- matrix(nrow=d, ncol=K)
  sigmatrack <- array(dim=c(d,K,nbit+1))
  pz <- km$size/nrow(x)
  pztrack <- array(dim=c(K,nbit+1))
  theta <- numeric(K) # matrix(nrow=d*(d-1)/2,ncol=K)
  thetatrack <- array(dim=c(K,nbit+1)) # array(dim=c(d*(d-1)/2,K,nbit+1))
  xtilde <- matrix(nrow=n,ncol=d)
  xtildetrack <- array(dim=c(n,d,nbit+1))
  G <- array(dim=c(n,d,K))
  g <- array(dim=c(n,d,K))
  intervalTheta <- matrix(nrow=2,ncol=K)
  cop <- list()
  
  # TODO This should be its own function
  for(z in 1:K){
    if(copulaFamilies[z]=="gaussian"){
      cop[[z]] <- normalCopula(dim=d,dispstr="un")
      intervalTheta[,z] <- c(-.99,.99)
    }else if(copulaFamilies[z]=="clayton"){
      cop[[z]] <- claytonCopula(dim=d)
      intervalTheta[,z] <- c(0.001,50)
    }else if(copulaFamilies[z]=="frank"){
      cop[[z]] <- frankCopula(dim=d)
      intervalTheta[,z] <- c(-50,50)
    }else if(copulaFamilies[z]=="indep"){
      cop[[z]] <- indepCopula(dim=d)
      intervalTheta[,z] <- c(0,1)
    }else if(copulaFamilies[z]=="gumbel"){
      cop[[z]] <- gumbelCopula(dim=d)
      intervalTheta[,z] <- c(1,1000)
    }else if(copulaFamilies[z]=="student"){
      cop[[z]] <- tCopula(dim=d,df=4,df.fixed=TRUE)
      intervalTheta[,z] <- c(-.99,.99)
    }else{
      stop("'copulaFamilies' is wrong.")
    }
  }
  
  if(debug){
    print("finished selecting copula\n Starting with G initialization")
  }
  
  for(j in 1:d){
    for(z in 1:K){
      sigma[j,z] <- sd(x[km$cluster==z,j])
    }
  }
      
  across_dimensions_aux <- function (param){across_dimensions(param,K,n,x,mu,sigma,km)}
  time_init <- system.time(
    result <- mclapply(1:d,across_dimensions_aux, mc.cores = numCores_2)
  )
  for(i in 1:d){
    G[,i,] <- result[[i]][,,1]
    g[,i,] <- result[[i]][,,2]
  }
  
  if(debug){
    print("finished setting G and g, time taken :")
    print(time_init)
  }
  for(z in 1:K){
    print(paste("processing copula: ", z))
    if(copulaFamilies[z]=="indep"){
      cop[[z]]@parameters <- theta[z] <- 0 # theta[,z]
    }else{
      theta[z] <- fitCopula(cop[[z]], pobs(x[km$cluster==z,]),
                            method="itau")@estimate #theta[,z]
      cop[[z]]@parameters <- theta[z] #theta[,z]
    }
  }
  print ("finished setting up copulas")
  objectiveValue <- 0
  mutrack[,,1] <- mu
  sigmatrack[,,1] <- sigma
  thetatrack[,1] <- theta # thetatrack[,,1] <- theta 
  pztrack[,1] <- pz
  
  ## loop
  for(t in 1:nbit){
    
    
    if(debug){
      cat("starting loop ", t,'\n')
    }
    
    ## 0. Calcul de h(z|x^i), x^i multivariate (OK)
    num <- hzx <- matrix(nrow=n, ncol=K)
    denom <- numeric(n)
    for(z in 1:K){
      for(i in 1:n){
        if(sum(G[i,,z]==1)>0 & copulaFamilies[z]=="student"){
          num[i,z] <- 0 ## because analytic limit is zero 
        }else{
          num[i,z] <- abs( dCopula(G[i,,z],copula=cop[[z]]
          )*prod(g[i,,z])*pz[z] ) # hope negative values close to 0
        } # this is to avoid numbers like -0.0567e-267
      }
    }
    if(debug){
      print("Done calculating num")
    }
    for(i in 1:n){
      denom[i] <- sum(num[i,],na.rm=TRUE)
      for(z in 1:K){
        hzx[i,z] <- if(denom[i]==0) 1/K else num[i,z]/denom[i]
      }
    }
    
    if(debug){
      print("finished updating h(z|x_i)")
    }
    
    ## 1. Update pi_z for each z (OK)
    newpz <- numeric(K)
    for(z in 1:K){
      newpz[z] <- mean(hzx[,z],na.rm=TRUE)
    }
    
    if(debug){
      print("finished updating pi_z")
    }
    
    ## 2. Update mu_{jz} for each j=1,...,d, z=1,...,K (OK)
    newmu <- matrix(nrow=d,ncol=K)
    for(z in 1:K){
      for(j in 1:d){
        newmu[j,z] <- sum(x[,j]*hzx[,z])/sum(hzx[,z])
      }
    }
    
    if(debug){
      print("finished updating mu")
    }
    
    ## Update sigma_{j,z}, j=1,...,d, z=1,...,K (Yaroslav's formula)
    newsigma <- matrix(nrow=d,ncol=K)
    for(z in 1:K){
      for(j in 1:d){
        newsigma[j,z] <- sqrt( sum((x[,j]-mu[j,z])^2*hzx[,z])/sum(hzx[,z]) )
      }
    }
    
    if(debug){
      print("finished updating sigma")
    }
    ## Update G, ie create a sample of it
    zsim <- numeric(n)
    for(i in 1:n){
      zsim[i] <- sample(1:K,1,prob=hzx[i,]) 
      for(j in 1:d){
        xtilde[i,j] <- xtildetrack[i,j,t+1] <-
          ( x[i,j]-mu[j,zsim[i]] )/sigma[j,zsim[i]]
      }
    }
    
    if(debug){
      print("finished updating G")
    }
    
    ## Compute the "G" 
    newG1 <- newG2 <- newg1 <- newg2 <- array(dim=c(n,d,K))
    newG <- newg <- array(dim=c(n,d,K))
    time_update_g <- system.time(result <- mclapply(
      1:K,
      parallel_fun_2, 
      mc.cores = numCores_2,
      xtilde = xtilde,
      n = n,
      x = x,
      mu = mu,
      sigma = sigma,
      d = d,
      K
    ))
    for(i in 1:K){
      newG[,,i] <- t(result[[i]][,,1])
      newg[,,i] <- t(result[[i]][,,2])
    }
    
    
    if(debug){
      print("finished updating G 2nd time, time taken")
      print(time_update_g)
    }
    
    ## Update copulas parameters
    newTheta <- numeric(K) #newTheta <- matrix(nrow=d*(d-1)/2,ncol=K) 
    ## If there is a common copula
    if(commonCopula){ 
      optimizeFoo <- function(par){
        sum(temp)
        temp <- numeric(K)
        for(z in 1:K){ # - Inf = 0 = 0
          cop[[z]]@parameters <- par
          temp[z] <- sum(log(dCopula(G[hzx[,z]!=0,,z],copula=cop[[z]]))*
                           hzx[hzx[,z]!=0,z])
        }
        sum(temp)
      }
      
      for(z in 1:K){
        newTheta[z] <- optimize(optimizeFoo,intervalTheta[,z],maximum=TRUE)$maximum
        cop[[z]]@parameters <- theta[z]
      }
    }
    else{
      ## If the copulas are different
      optimizeFoo <- function(par, z){
        cop[[z]]@parameters <- par
        sum(log(dCopula(G[!outliers,,z],copula=cop[[z]]))*
              hzx[!outliers,z])           
      }
      optimFoo <- function(par, z){
        cop[[z]]@parameters <- par
        -sum(log(dCopula(G[!outliers,,z],copula=cop[[z]]))*
               hzx[!outliers,z])           
      }
      for(z in 1:K){
        outliers <- findOutliers(hzx[,z],1e-13) # findOutliers(g[,,z],1e-13)
        # if(length(theta[z])==1){ #theta[,z]
        #   newTheta[z] <- optimize(optimizeFoo,intervalTheta[,z],
        #                            z=z,maximum=TRUE)$maximum
        # }
        # else{
        try(newTheta[z] <- optim(
          theta[z],
          optimFoo,
          z=z,
          method="L-BFGS-B",
          lower = 0
          )$par)
        # }
        cop[[z]]@parameters <- theta[z] # cop[[z]]@parameters <- theta[,z]
      }
    }
    
    if(debug){
      print("finished updating copula values")
    }
    
    ## Compute the objective value
    term1 <- term2 <- term3 <- term4 <- matrix(nrow=n,ncol=K)
    for(z in 1:K){
      for(i in 1:n){
        if(hzx[i,z]==0){## -Inf * 0 = 0
          term1[i,z] <- term2[i,z] <- term3[i,z] <- term4[i,z] <- 0
        }else{
          term1[i,z] <- log(dCopula(G[i,,z],copula=cop[[z]]))*hzx[i,z]
          term2[i,z] <- sum(log(g[i,,z]))*hzx[i,z]
          term3[i,z] <- log(pz[z])*hzx[i,z]
          term4[i,z] <- log(hzx[i,z])*hzx[i,z]
        }
      }
    }        
    
    if(debug){
      print("finished updating objective value")
    }
    
    objectiveValue <- c(objectiveValue,sum(term1+term2+term3)-sum(term4))
    print(objectiveValue[length(objectiveValue)]) # 'objectiveValue' is the
    # log-likelihood of the data. Without the
    # 'sum(log(hzx)*hzx)' term, it is the Q(.|.)
    # function in the standard EM algorithm, ie
    # Q(a|a')=L(a)+H(a|a').
    
    ## Updates the programme variables
    for(z in 1:K){
      cop[[z]]@parameters <- newTheta[z] # cop[[z]]@parameters <- newTheta[,z]
    }
    mu <- mutrack[,,t+1] <- newmu
    sigma <- sigmatrack[,,t+1] <- newsigma
    g <- newg
    G <- newG
    pz <- pztrack[,t+1] <- newpz
    theta <- thetatrack[,t+1] <- newTheta #thetatrack[,,t+1]
  }
  
  return( list(G=G,g=g,theta=thetatrack,mu=mutrack,sigma=sigmatrack,
               pz=pztrack,xtilde=xtildetrack,
               hzx=hzx, copulaFamilies=copulaFamilies,
               Ovalue=objectiveValue[-1])
  )
}

## Laplace distribution
dlaplace <- function(x,b){
  exp(-abs(x)/b)/2/b
}
plaplace <- function(x,b){
  if(x<0){
    exp(x/b)/2
  }else{
    1-exp(-x/b)/2
  }
}
qlaplace <- function(x,b){
  if(x<=.5){
    b*(log(2)+log(x))
  }else{
    -b*(log(2)+log(1-x))
  }
}
rlaplace <- function(n,b){
  sapply(runif(n),qlaplace,b=b)
}








library(copula)
library(quadprog)
library(parallel)

numCores <- 15
numCores_2 <- 1
copulaName <- "frank"
clusterSize <- 3
sampleSize <- 2000
nb_it_em <- 10
method <- "new-stochastic"

names = c(
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv",
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv",
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv"
)

#read the data and combine it. 
data <- NULL
for(name in names){
  genes <- as.matrix(read.csv(name)[,c(1)])
  aux_data <- as.matrix(read.csv(name)[,c(2,3,4,5)])
  if (is.null(data)){
    data <- aux_data
  }else{
    data <- cbind(data,aux_data)
  }
  # break
}

data <- data[sample(1:nrow(data), sampleSize, replace = FALSE),]

result <- EMalgo(
  data,
  copulaFamilies=rep(copulaName,clusterSize),
  nbit=nb_it_em,
  method=method,
  commonCopula=FALSE,
  debug = TRUE
)


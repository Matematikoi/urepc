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
  theta <- matrix(nrow=d*(d-1)/2,ncol=K) # numeric(K)
  thetatrack <- array(dim=c(d*(d-1)/2,K,nbit+1)) # array(dim=c(K,nbit+1))
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
    print("finished setting G and g")
  }
  for(z in 1:K){
    if(copulaFamilies[z]=="indep"){
      cop[[z]]@parameters <- theta[,z] <- 0 #theta[z]
    }else{
      theta[,z] <- fitCopula(cop[[z]], pobs(x[km$cluster==z,]),
                             method="itau")@estimate
      cop[[z]]@parameters <- theta[,z] # theta[z]
    }
  }
  
  objectiveValue <- 0
  mutrack[,,1] <- mu
  sigmatrack[,,1] <- sigma
  thetatrack[,,1] <- theta # thetatrack[,1] <- theta
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
    for(z in 1:K){
      for(j in 1:d){
        if(method=="naive-stochastic"){
          h <- bw.nrd(xtilde[,j])
          for(i in 1:n) newG[i,j,z] <- mean( pnorm((x[i,j]-mu[j,z])/sigma[j,z],
                                                   mean=xtilde[,j],
                                                   sd=h) )
          for(i in 1:n) newg[i,j,z] <- mean( dnorm((x[i,j]-mu[j,z])/sigma[j,z],
                                                   mean=xtilde[,j],
                                                   sd=h) )/sigma[j,z]
        }else if(method=="new-stochastic"){
          h <- bw.nrd(xtilde[,j])
          M <- rbind(rep(1,length(xtilde[,j])), xtilde[,j], xtilde[,j]^2)
          Amat <- cbind(t(M),diag(1,n))
          bvec <- c(1,0,1-h^2,rep(1e-17,n)) # If tolerance is exactly 0, a negative
          # solution can be produced. I guess this is because
          # simulatenous and possibly contradicting constraints
          # can compete and the implementation considers a
          # solution has been achieved when within a certain
          # tolerance, even if the numbers are negative.
          dvec <- rep(0,n)
          Dmat <- diag(2,n)
          
          if(debug){
            cat("starting to solve the matrix K ",z," d ",j, '\n')
          }
          
          kweightshat <- solve.QP(Dmat, dvec, Amat, bvec, meq=3)$solution
          # Dmat <- diag(2,n)
          # Amat <- cbind(t(M),diag(1,n))
          aux_list <- list(
            dvec = dvec, 
            bvec = bvec, 
            meq = 3,
            kweightshat = kweightshat, 
            n = n, 
            M = M
          )
          # saveRDS(aux_list , file = paste(
          #   'opti_data/opt',
          #   number_file_cnt,
          #   '.RDS',
          #   sep = '_'
          # ))
          number_file_cnt <-  number_file_cnt +1
          if(debug){
            cat("finished to solve the matrix K ",z," d ",j,'\n')
          }
          
          for(i in 1:n) newg[i,j,z] <- (1/sigma[j,z])*t(kweightshat)%*%
            dnorm(xtilde[,j], mean=(x[i,j]-mu[j,z])/sigma[j,z], sd=h)
          for(i in 1:n) newG[i,j,z] <- t(kweightshat)%*%
            pnorm((x[i,j]-mu[j,z])/sigma[j,z], mean=xtilde[,j], sd=h)
        }else if(method=="naive-deterministic"){
          h <- bw.nrd(x[,j]) # choix de h non resolu dans ce cas.
          tmp <- matrix(nrow=n, ncol=K) # approche Benaglia et al 2009 ne va pas
          for(i in 1:n){ # non plus car l'estimateur n'est pas une densite
            for(k in 1:K){ # (integrale != 1) et donc on a pas une c.d.f. : pbm
              tmp[i,k] <- sum( dnorm(  (x[,j]-mu[j,k])/sigma[j,k], # pour la 
                                       mean=(x[i,j]-mu[j,z])/sigma[j,z], # copule.
                                       sd=h  )*hzx[,k] )
            }
            newg[i,j,z] <- sum(tmp[i,])/n/sigma[j,z]
          }
          tmp <- matrix(nrow=n, ncol=K)
          for(i in 1:n){
            for(k in 1:K){
              tmp[i,k] <- sum( pnorm( (x[i,j]-mu[j,z])/sigma[j,z],
                                      mean=(x[,j]-mu[j,k])/sigma[j,k],
                                      sd=h  )*hzx[,k] )
            }
            newG[i,j,z] <- sum(tmp[i,])/n
          }
        }else if(method=="new-deterministic"){
          h <- bw.nrd(x[,j])
          print("not yet implemented")
        }else{ # obsolete
          h <- bw.nrd(xtilde[,j])
          for(i in 1:n) newG1[i,j,z] <- mean( pnorm((x[i,j]-mu[j,z])/sigma[j,z],
                                                    mean=xtilde[,j],
                                                    sd=h) )
          for(i in 1:n) newG2[i,j,z] <- mean( pnorm((-x[i,j]+mu[j,z])/sigma[j,z],
                                                    mean=xtilde[,j],
                                                    sd=h) )
          for(i in 1:n) newg1[i,j,z] <- mean( dnorm((x[i,j]-mu[j,z])/sigma[j,z],
                                                    mean=xtilde[,j],
                                                    sd=h) )/sigma[j,z]
          for(i in 1:n) newg2[i,j,z] <- mean( dnorm((-x[i,j]+mu[j,z])/sigma[j,z],
                                                    mean=xtilde[,j],
                                                    sd=h) )/sigma[j,z]
          ## symmetrization
          newg[,j,z] <- (newg1[,j,z]+newg2[,j,z])/2
          newG[,j,z] <- (newG1[,j,z]+1-newG2[,j,z])/2
        }
      }
    }
    
    if(debug){
      print("finished updating G 2nd time")
    }
    
    ## Update copulas parameters
    newTheta <- matrix(nrow=d*(d-1)/2,ncol=K) # newTheta <- numeric(K)
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
        if(length(theta[,z])==1){
          newTheta[,z] <- optimize(optimizeFoo,intervalTheta[,z],
                                   z=z,maximum=TRUE)$maximum
        }
        else{
          if(debug){
            cat ("theta[,z]: ",theta[,z],'\n z: ',z,'\n')
          }
          try(newTheta[,z] <- optim(theta[,z],optimFoo,
                                    z=z,method="BFGS")$par)
          # newTheta[,z] <- tryCatch(
          # {
          #      return (optim(theta[,z],optimFoo,
          #                       z=z,method="BFGS")$par)
          # },
          #     error=function(cond) {
          #     cat(
          #         "theta[,z]: ",
          #         theta[,z],
          #         "\n z : ",
          #         z
          #         )
          #     # Choose a return value in case of error
          #     return(NA)
          # },
          # finally = {
          #     print("finished optimizing copula")
          # }
          # )
        }
        cop[[z]]@parameters <- theta[,z] # cop[[z]]@parameters <- theta[z]
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
      cop[[z]]@parameters <- newTheta[,z] # cop[[z]]@parameters <- newTheta[z]
    }
    mu <- mutrack[,,t+1] <- newmu
    sigma <- sigmatrack[,,t+1] <- newsigma
    g <- newg
    G <- newG
    pz <- pztrack[,t+1] <- newpz
    theta <- thetatrack[,,t+1] <- newTheta
  }
  
  return( list(G=G,g=g,theta=thetatrack,mu=mutrack,sigma=sigmatrack,
               pz=pztrack,xtilde=xtildetrack,
               hzx=hzx, copulaFamilies=copulaFamilies,
               Ovalue=objectiveValue[-1])
  )
}


## Algo for parametric Gaussian model 
EMalgoParamGauss <- function(data, nbit, K=3){
  
  x <- data
  n <- nrow(x)
  d <- ncol(x)
  km <- kmeans(x,centers=K)
  mu <- t(km$centers)
  pz <- km$size/nrow(x)
  covestimateBG <- array(dim=c(d,d,K)) # By Group
  covestimate <- matrix(nrow=d,ncol=d) # mean of the covariance matrices
  sigma <- array(dim=c(d,K)) # standard deviation and NOT variance!
  theta <- numeric(K)
  mutrack <- sigmatrack <- array(dim=c(d,K,(nbit+1)))
  thetatrack <- pztrack <- array(dim=c(K,(nbit+1)))
  
  for(z in 1:K){
    covestimateBG[,,z] <- cov(x[km$cluster==z,])
    sigma[,z] <- sqrt(diag(covestimateBG[,,z]))
    theta[z] <- covestimateBG[1,2,z]/prod(sigma[,z])
  }
  covestimate <- apply(covestimateBG,c(1,2),mean) 
  
  objectiveValue <- 0
  mutrack[,,1] <- mu
  sigmatrack[,,1] <- sigma
  thetatrack[,1] <- theta
  pztrack[,1] <- pz
  
  ## loop
  for(t in 1:nbit){
    
    ## 0. Calcul de h(z|x^i), x^i multivariate (OK)
    num <- hzx <- matrix(nrow=n, ncol=K)
    denom <- numeric(n)
    for(z in 1:K){
      for(i in 1:n){
        num[i,z] <- dmvnorm(x[i,],mu[,z],covestimateBG[,,z])*pz[z] # covestimate??
      }
    }
    for(i in 1:n){
      denom[i] <- sum(num[i,],na.rm=TRUE)
      for(z in 1:K){
        hzx[i,z] <- if(denom[i]==0) 1/K else num[i,z]/denom[i]
      }
    }
    
    ## 1. Update pi_z for each z (OK)
    newpz <- numeric(K)
    for(z in 1:K){
      newpz[z] <- mean(hzx[,z],na.rm=TRUE)
    }
    
    ## 2. Update mu_{jz} for each j=1,...,d, z=1,...,K (OK)
    newmu <- matrix(nrow=d,ncol=K)
    for(z in 1:K){
      for(j in 1:d){
        newmu[j,z] <- sum(x[,j]*hzx[,z])/sum(hzx[,z])
      }
    }
    
    ## 3. Update covariance matrix
    newcovestimateBG <- array(dim=c(d,d,K))
    newcovestimate <- matrix(nrow=d,ncol=d)
    newsigma <- array(dim=c(d,K))
    newtheta <- numeric(K)
    tempmat <- array(dim=c(n,d,d,K))
    for(z in 1:K){
      for(i in 1:n){
        tempmat[i,,,z] <- (x[i,]-newmu[,z])%*%t(x[i,]-newmu[,z])*hzx[i,z]
      }
      for(j1 in 1:d){
        for(j2 in 1:d){
          newcovestimateBG[j1,j2,z] <- sum(tempmat[,j1,j2,z])/sum(hzx[,z])
        }
      }
      newsigma[,z] <- sqrt(diag(newcovestimateBG[,,z]))
      newtheta[z] <- newcovestimateBG[1,2,z]/prod(newsigma[,z])
    }
    for(j1 in 1:d){
      for(j2 in 1:d){
        newcovestimate[j1,j2] <- sum(newcovestimateBG[j1,j2,]*apply(hzx,2,sum))/n
      }
    }
    
    ## Compute the objective value
    term1 <- term2 <- term3 <- term4 <- matrix(nrow=n,ncol=K)
    for(z in 1:K){
      for(i in 1:n){
        term1[i,z] <- log(dmvnorm(x[i,],
                                  mu[,z],covestimateBG[,,z]))*hzx[i,z] # covestimate??
        term3[i,z] <- log(pz[z])*hzx[i,z]
        term4[i,z] <- log(hzx[i,z])*hzx[i,z]
      }
    }
    
    objectiveValue <- c(objectiveValue,sum(term1+term3-term4,na.rm=TRUE))
    print("objective Value:")
    print(objectiveValue[length(objectiveValue)])
    
    ## Updates the programme variables
    mu <- mutrack[,,t+1] <- newmu
    covestimateBG <- newcovestimateBG
    covestimate <- newcovestimate
    sigma <- sigmatrack[,,t+1] <- newsigma
    theta <- thetatrack[,t+1] <- newtheta
    pz <- pztrack[,t+1] <- newpz
  }
  
  return( list(mu=mutrack,covestimate=covestimate,pz=pztrack,
               hzx=hzx,sigma=sigmatrack,theta=thetatrack,
               Ovalue=objectiveValue[-1] ) )
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
numCores_2 <- 4
copulaName <- "gaussian"
clusterSize <- 15
sampleSize <- 1000
nb_it_em <- 2
method <- "new-stochastic"
number_file_cnt <- 0

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
  break
}

data <- data[sample(1:nrow(data), sampleSize, replace = FALSE),]

start_tm <- Sys.time()
result <- EMalgo(
  data,
  copulaFamilies=rep(copulaName,clusterSize),
  nbit=nb_it_em,
  method=method,
  commonCopula=FALSE,
  debug = TRUE
)
## Returns the rows of 'data' whose at least one element is below 'tol'
findOutliers <- function(data,tol){
  where <- (abs(as.matrix(data))<=tol)
  is.outlier <- (apply(where,1,sum)>=1)
  is.outlier
}


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
  time_st <- Sys.time()
  for(j in 1:d){
    sampleOfG <- NA
    for(z in 1:K){
      sigma[j,z] <- sd(x[km$cluster==z,j])
      sampleOfG <- c(sampleOfG, (x[km$cluster==z,j]-mu[j,z])/sigma[j,z])
    }
    sampleOfG <- sampleOfG[-1]
    xtildetrack[,j,1] <- sampleOfG
    for(z in 1:K){
      h <- bw.nrd(sampleOfG)
      M <- rbind(rep(1,length(sampleOfG)), sampleOfG, sampleOfG^2)
      h <- bw.nrd(sampleOfG)
      
      bvec <- c(1,0,1-h^2,rep(0,length(sampleOfG)))
      dvec <- rep(0,length(sampleOfG))
      
      parameters <- list(
        dvec = dvec,
        bvec = bvec, 
        meq = 3, 
        n=n, 
        M=M,
        cores = numCores,
        mean = sampleOfG,
        h = h,
        sigma = sigma,
        mu=mu,
        j = j,
        z = z,
        x = x)
      in_file <- paste(
        "cluster_in_data/parameter_small_",
        j,
        z,
        ".RDS",
        sep = "_"
      )
      out_file <- paste(
        "cluster_out_data/parameter_small_",
        j,
        z,
        ".RDS",
        sep = "_"
      )
      saveRDS(parameters,file = in_file)
      
      
      command <- paste(
        "sbatch -A standby -N 1 -n 1",
        outside_resource,
        "--job-name=opti_key_2 -o cluster_out_data/out_%j_.out run_optimization.sh -i",
        in_file,
        " -o ",
        out_file,
        sep= " "
      )
      
      
      
      # RUN
      system(command,intern = TRUE)
      
      
    }
  }
  while (system("squeue -u glozanop -n opti_key_2 | wc -l",intern = TRUE) != "1"){
    Sys.sleep(5)
    elements_left <- system("squeue -u glozanop -n opti_key_2 | wc -l", intern = TRUE)
    cat(elements_left,",")
  }
  for (j in 1:d){
    for(z in 1:K){
      out_file <- paste(
        "cluster_out_data/parameter_small_",
        j,
        z,
        ".RDS",
        sep = "_"
      )
      out_data <- readRDS(out_file)
      g[,j,z] <- out_data[['g']]
      G[,j,z] <- out_data[['G']]
      
    }
  }
  
  time_end <- Sys.time()
  if(debug){
    print("finished setting G and g, time:")
    print(time_end-time_st)
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
    
    #NOT CANON
    # num[is.na(num)] <- 0
    if (sum(is.na(num))>0){
      num[is.na(num)] <- runif(
        sum(is.na(num)),
        min = max(num[num<Inf & !is.na(num)]) * 0.9,
        max = max(num[num<Inf & !is.na(num)]) * 0.95)
    }
    if(sum((num == 0))>0){
      num[num == 0 ] <- runif(
        sum((num == 0)),
        min = max(1e-65, min(num[num>0])/10),
        max = max(1e-60, min(num[num>0]))
      )
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
    time_st <- Sys.time()
    newG1 <- newG2 <- newg1 <- newg2 <- array(dim=c(n,d,K))
    newG <- newg <- array(dim=c(n,d,K))
    for(z in 1:K){
      for(j in 1:d){
        h <- bw.nrd(xtilde[,j])
        M <- rbind(rep(1,length(xtilde[,j])), xtilde[,j], xtilde[,j]^2)
        bvec <- c(1,0,1-h^2,rep(1e-17,n)) 
        dvec <- rep(0,n)
        
        parameters <- list(
          dvec = dvec,
          bvec = bvec, 
          meq = 3, 
          n=n, 
          M=M,
          cores = numCores,
          mean = xtilde[,j],
          h = h,
          sigma = sigma,
          j = j,
          z = z,
          x = x,
          mu=mu)
        in_file <- paste(
          "cluster_in_data/parameter_small_",
          j,
          z,
          ".RDS",
          sep = "_"
        )
        out_file <- paste(
          "cluster_out_data/parameter_small_",
          j,
          z,
          ".RDS",
          sep = "_"
        )
        saveRDS(parameters,file = in_file)
        
        
        command <- paste(
          "sbatch -A standby -N 1 -n 1",
          outside_resource,
          "--job-name=opti_key_2 -o cluster_out_data/out_%j_.out run_optimization.sh -i",
          in_file,
          " -o ",
          out_file,
          sep= " "
        )
        system(command = command,intern = TRUE)
        
        
      }
    }
    while (system("squeue -u glozanop -n opti_key_2 | wc -l",intern = TRUE) != "1"){
      Sys.sleep(5)
      items_left <- system("squeue -u glozanop -n opti_key_2 | wc -l",intern = TRUE)
      cat (items_left, ",")
    }
    for (j in 1:d){
      for(z in 1:K){
        out_file <- paste(
          "cluster_out_data/parameter_small_",
          j,
          z,
          ".RDS",
          sep = "_"
        )
        out_data <- readRDS(out_file)
        newg[,j,z] <- out_data[['g']]
        newG[,j,z] <- out_data[['G']]
      }
    }
    time_end <- Sys.time()
    
    #make G a little bigger to avoid problems 
    eps_G <- 1e-15
    eps_g <- 1e-15
    newG <- (1-eps_G)*newG + eps_G
    newg <- (1-eps_g)*newg + eps_g
    
    if(debug){
      print("finished updating G 2nd time, time:")
      print(time_end -time_st)
    }
    
    ## Update copulas parameters
    newTheta <- matrix(nrow=d*(d-1)/2,ncol=K) # newTheta <- numeric(K)
    ## If there is a common copula
    time_st <- Sys.time()
    optimizeTheta <- function(par, z){
      cop[[z]]@parameters <- par
      -sum(log(dCopula(G[!outliers,,z],copula=cop[[z]]))*
             hzx[!outliers,z])           
    }
    outliers <- findOutliers(hzx[,z],1e-13) 
    newTheta <- foreach(z=1:K, combine = c)%dopar%{
      optim(theta[,z],optimizeTheta,z=z,method="SANN")$par
    }
    newTheta <- matrix(unlist(newTheta), ncol = K)
    for (z in 1:K){
      cop[[z]]@parameters <- theta[,z]
    }
    time_end <- Sys.time()
    if(debug){
      print("finished updating copula values, time:")
      print(time_end-time_st)
    }
    
    ## Compute the objective value
    term1 <- term2 <- term3 <- term4 <- matrix(nrow=n,ncol=K)
    eps <- 1e-60
    for(z in 1:K){
      for(i in 1:n){
        if(hzx[i,z]==0){## -Inf * 0 = 0
          term1[i,z] <- term2[i,z] <- term3[i,z] <- term4[i,z] <- 0
        }else{
          term1[i,z] <- log(dCopula(G[i,,z],copula=cop[[z]])+eps)*hzx[i,z]
          term2[i,z] <- sum(log(g[i,,z])+ eps)*hzx[i,z]
          term3[i,z] <- log(pz[z] + eps)*hzx[i,z]
          term4[i,z] <- log(hzx[i,z] + eps)*hzx[i,z]
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
    check_point <- list(G=G,g=g,theta=thetatrack,mu=mutrack,sigma=sigmatrack,
         pz=pztrack,xtilde=xtildetrack,
         hzx=hzx, copulaFamilies=copulaFamilies,
         Ovalue=objectiveValue[-1])
    saveRDS(check_point, file ="results_cancer/check_point_2.RDS")
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
library(foreach)
library(doParallel)


registerDoParallel(99)
numCores <- 11
copulaName <- "gaussian"
clusterSize <- 10
sampleSize <- 1000
nb_it_em <- 50
outside_resource <- " --time=00:23:00 --mem-per-cpu=23G"
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

# data <- data[sample(1:nrow(data), sampleSize, replace = FALSE),]

start_tm <- Sys.time()
result <- EMalgo(
  data,
  copulaFamilies=rep(copulaName,clusterSize),
  nbit=nb_it_em,
  method=method,
  commonCopula=FALSE,
  debug = TRUE
)

saveRDS(result, file = "opti_data/result_big_2.RDS")
end_tm <- Sys.time()
print(paste(
  "Taken:",
  end_tm - start_tm,
  "for ",
  nb_it_em,
  " iterations"
))

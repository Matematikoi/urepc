start_time <- Sys.time()
source("./original_code/functions.R")
library(copula)
set.seed(189)
nbcomp <- 3 # number if copulas
mu0 <- matrix(nrow=2,ncol=nbcomp,
              c( -3,  0,
                0,   3,
                3, 0))
sigma0 <- matrix(nrow=2,ncol=nbcomp,
                 c( 2, .7,
                   .7, 1.4,
                   1.4, 2.8))
pz0 <- rep(1/nbcomp,nbcomp)
theta0 <- c(-3.45,3.45,0) 
nbit <- 30
nsample <- 300

data <- simul(
  nsample, 
  Q=list(
    function(x){qnorm(x)},
    function(x){qlaplace(x,b=1/sqrt(2))}
  ),
  mu=mu0,
  sigma=sigma0,
  pz=pz0,
  theta=theta0, # qlaplace(x,b=1/sqrt(2))
  copulaFamilies=rep("frank",nbcomp)
  ) # qnorm(x)

res <- EMalgo(
  data[,1:2], 
  copulaFamilies=rep("frank",nbcomp),
  nbit=nbit, 
  method="naive-stochastic", 
  commonCopula=FALSE
  )

#str(res) 
saveRDS(res, file = 'results/new_result.RDS')

end_time <- Sys.time()
run_time <- end_time - start_time

print(run_time)

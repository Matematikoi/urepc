library(parallel)
library(MASS)
library(foreach)
library(doParallel)

numCores <- detectCores()
registerDoParallel(numCores)  # use multicore, set to the number of our cores

max_i = 4
max_j = 5
foo <- matrix(nrow=max_i,ncol = max_j )

for (i in 1:max_i){
  z <- i+2
  for (j in 1:max_j){
    foo[i,j] <-  i+j+z
  }
}
foo

foreach(i = 1:max_i) %do%
  foreach(j = 1:max_j)%do%
    i+j

foreach(i=1:max_i, .combine='rbind') %dopar% {
  z <- i+2
  foreach(j=1:max_j, .combine = 'c') %dopar% {
    p <- i+j+z
    print(p)
  }
}


library(SPEI)

old.array = array(abs(rnorm(50)), dim=c(72,36,136))

new.array = array(dim=c(72,36,136))
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)
zz <- foreach(i = 1:100, .combine = c) %:% 
  foreach(j = 1:100, .combine = c ) %dopar% {
    new.listoflists <- FUN(old.array[i,j,])
    new.array[i,j,] <- new.listoflists$list
  }
stopCluster(cl)
new.obj <- plyr::ldply(zz, data.frame)





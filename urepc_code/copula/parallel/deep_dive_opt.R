library(quadprog)
library(parallel)
library(ggplot2)
test_fun <- function (name_num){
  data <- readRDS(paste('opti_data/opt',name_num,'.RDS', sep = '_'))
  # data <- readRDS('opti_data/easy.RDS')
  
  # Usual way
  time_st <- Sys.time()
  Dmat <- diag(2,data[['n']])
  Amat <- cbind(t(data[['M']]),diag(1,data[['n']]))
  kweightshat <- solve.QP(
    Dmat,
    data[['dvec']],
    Amat,
    data[['bvec']],
    data[['meq']])
  time_end <- Sys.time()
  print(time_end-time_st)
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
  time_st_2 <- Sys.time()
  kweightshat_2 <- solve.QP.compact(
    Dmat,
    data[['dvec']],
    Amat,
    Aind,
    data[['bvec']],
    data[['meq']])
  time_end_2 <- Sys.time()
  return(list(
    usual_time = time_end-time_st,
    new_time = time_end_2-time_st_2,
    original_sol = kweightshat, 
    new_sol = kweightshat_2))
}

result <- mclapply(0:60, test_fun, mc.cores = 100)

data <- readRDS('opti_data/opt_1_.RDS')



times <- numeric(61*2)
for (i in seq(61)){
  times[i] <- result[[i]]$new_time
  times[i+61] <- result[[i]]$usual_time
}
df <- data.frame(times = times, cat = c(rep('new',61),rep('old',61)))

df

ggplot(df, aes(cat,times,fill = cat))+
  geom_violin(scale = 'area')

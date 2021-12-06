start_time <- Sys.time()
library(copula)
library("aricode")
library(quadprog)
library(ggplot2)
source("./original_code/functions.R")
mx_it <- 500
nb_it_em <- 30
ari_naive <- numeric(mx_it)
ari_new <- numeric(mx_it)
for (i in 1:mx_it){
  sim_data <- read.csv( paste(
    'sim_data/data_',
    i,
    '_.csv',
    sep = ''
  ),row.names = 1)

  res <- EMalgo(
    sim_data[,1:2],
    copulaFamilies=rep("frank",3),
    nbit=nb_it_em,
    method="naive-stochastic",
    commonCopula=FALSE
  )
  
  sim_data[['predicted']] <- as.factor(apply(res$hzx,1,which.max))
  ari_naive[i] <- ARI(sim_data$z,sim_data$predicted)
}

for (i in 1:mx_it){
  sim_data <- read.csv( paste(
    'sim_data/data_',
    i,
    '_.csv',
    sep = ''
  ),row.names = 1)
  
  res <- EMalgo(
    sim_data[,1:2],
    copulaFamilies=rep("frank",3),
    nbit=nb_it_em,
    method="new-stochastic",
    commonCopula=FALSE
  )
  
  sim_data[['predicted']] <- as.factor(apply(res$hzx,1,which.max))
  ari_new[i] <- ARI(sim_data$z,sim_data$predicted)
}

df <- data.frame(
  'ari' = c(ari_naive,ari_new), 
  'method' = c(rep('naive',mx_it),rep('new',mx_it))
  )

ggplot(df,aes(method,ari))+
  geom_violin(scale = 'area', mapping = aes(color = method, fill = method))



end_time <- Sys.time()
run_time <- end_time - start_time

print(run_time)




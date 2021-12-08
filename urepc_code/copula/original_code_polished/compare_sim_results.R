start_time <- Sys.time()
library(copula)
library("aricode")
library(quadprog)
library(ggplot2)
source("./original_code/functions.R")
nb_it_em <- 30
idx <- 1
nsample <- 900

invisible(eval(parse(text=commandArgs(TRUE))))

sim_data <- read.csv( paste(
  'sim_data/_data_idx',
  idx,
  'size',
  nsample,
  '.csv',
  sep = '_'
),row.names = 1)

res_naive <- EMalgo(
  sim_data[,1:2],
  copulaFamilies=rep("frank",3),
  nbit=nb_it_em,
  method="naive-stochastic",
  commonCopula=FALSE
)

sim_data[['predicted_naive']] <- as.factor(apply(res_naive$hzx,1,which.max))
ari_naive <- ARI(sim_data$z,sim_data$predicted)


res_new <- EMalgo(
  sim_data[,1:2],
  copulaFamilies=rep("frank",3),
  nbit=nb_it_em,
  method="new-stochastic",
  commonCopula=FALSE
)

sim_data[['predicted']] <- as.factor(apply(res_new$hzx,1,which.max))
ari_new <- ARI(sim_data$z,sim_data$predicted)


result <- list(
  "ari_new" = ari_new,
  "ari_naive" = ari_naive,
  "ovalue_new" = res_new$Ovalue[30],
  "ovalues_naive" = res_naive$Ovalue[30],
  "sigma_new" = gsub(" ","",toString(res_new$sigma[,,31])),
  "sigma_naive" = gsub(" ","",toString(res_naive$sigma[,,31])),
  "mu_new" = gsub(" ","",toString(res_new$mu[,,31])),
  "mu_naive" = gsub(" ","",toString(res_naive$mu[,,31])),
  "theta_new" = gsub(" ","",toString(res_new$theta[,,31])),
  "theta_naive" = gsub(" ","",toString(res_naive$theta[,,31])),
  "pz_new" = gsub(" ","",toString(res_new$pz[,31])),
  "pz_naive" = gsub(" ","",toString(res_naive$pz[,31]))
)

write.csv(result, file = paste(
  'results_sim_data/_f1_idx',
  idx,
  'size',
  nsample,
  '.csv',
  sep = '_'
))

end_time <- Sys.time()
run_time <- end_time - start_time

print(run_time)




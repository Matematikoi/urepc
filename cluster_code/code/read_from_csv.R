library("mixtools")
library('tidyverse')

names = c(
  "./data/cluster_selection/1-5-CPMcutoff-suffix-1-log-cero-replacement.csv",
  "./data/cluster_selection/1-5-CPMcutoff-suffix-2-log-cero-replacement.csv",
  "./data/cluster_selection/1-5-CPMcutoff-suffix-3-log-cero-replacement.csv"
)

cluster_size = 20
max_iteration = 1000
epsilon = 1e-07
arbmean = TRUE
name_model = 'model_sem_lambda_30_888p_.csv,model_sem_mu_30_888p_.csv,model_sem_sigma_30_888p_.csv'

index = 8

invisible(eval(parse(text=commandArgs(TRUE))))


centers = read.csv(paste('results/sem/', str_split(name_model, ',')[[1]][2], sep = '')
                   ,row.names = 1)
sigma = read.csv(paste('results/sem/', str_split(name_model, ',')[[1]][3], sep = ''),
                 row.names = 1)
lambdas = read.csv(paste('results/sem/', str_split(name_model, ',')[[1]][1], sep = ''),
                   row.names = 1)
size = length(centers)
index = max_iteration


#read the data and combine it. 
data <- NULL
for(name in names){
  aux_data <- as.matrix(read.csv(name)[,c(2,3,4,5)])
  if (is.null(data)){
    data <- aux_data
  }else{
    data <- cbind(data,aux_data)
  }
}

size = length(as.vector(centers$x))

st = Sys.time()
selection <- repnormmixEM(
  t(data),
  mu = as.vector(centers$x),
  sigma = as.vector(sigma$x),
  lambda = as.vector(lambdas$x),
  verb = TRUE,
  epsilon = epsilon,
  maxit = max_iteration,
  arbmean = arbmean,
  
)

write(
  paste(
    index,
    selection$loglik,
    selection$restarts,
    size,
    sep=','),
  file = "results/sem_results_2/loglik_sem_2.csv",
  append = TRUE
)



write.csv(selection$lambda, file = paste(
  'results/sem_results_2/model_sem_2_lambda',
  size,
  index,
  '.csv',
  sep = '_'
))
write.csv(selection$mu, file = paste(
  'results/sem_results_2/model_sem_2_mu',
  size,
  index,
  '.csv',
  sep = '_'
))
write.csv(selection$sigma, file = paste(
  'results/sem_results_2/model_sem_2_sigma',
  size,
  index,
  '.csv',
  sep = '_'
))


ed = Sys.time()
ed- st

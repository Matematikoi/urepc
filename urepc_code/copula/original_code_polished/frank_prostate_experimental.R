start_time <- Sys.time()
library(copula)
library(quadprog)
source("./original_code_polished/functions_experimental_frank.R")

nb_it_em <- 10


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

data <- data[1:10000,]

result <- EMalgo(
  data,
  copulaFamilies=rep("frank",3),
  nbit=nb_it_em,
  method="new-stochastic",
  commonCopula=FALSE,
  debug = TRUE
)


end_time <- Sys.time()
run_time <- end_time - start_time

print(run_time)


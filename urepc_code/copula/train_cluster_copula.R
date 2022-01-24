start_time <- Sys.time()
library(copula)
library(quadprog)

copulaName <- "gaussian"
clusterSize <- 2
sampleSize <- 500
nb_it_em <- 5
method <- "new-stochastic"

invisible(eval(parse(text=commandArgs(TRUE))))

if (copulaName == "frank"){
  source("./original_code_polished/functions_experimental_frank.R")  
}else{
  source("./original_code_polished/functions.R")
}






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

#data <- data[sample(1:nrow(data), sampleSize, replace = FALSE),]

result <- EMalgo(
  data,
  copulaFamilies=rep(copulaName,clusterSize),
  nbit=nb_it_em,
  method=method,
  commonCopula=FALSE,
  debug = TRUE
)

name_file <- paste(
  'results_cancer/model_clusters',
  clusterSize,
  'iterations',
  nb_it_em,
  'copula',
  copulaName,
  '.RDS',
  sep='_'
)

saveRDS(result , file = name_file)

end_time <- Sys.time()
run_time <- end_time - start_time

print(run_time)


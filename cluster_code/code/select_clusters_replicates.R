library("mixtools")

names = c(
  "./data/cluster_selection/1-5-CPMcutoff-suffix-1-log-cero-replacement.csv",
  "./data/cluster_selection/1-5-CPMcutoff-suffix-2-log-cero-replacement.csv",
  "./data/cluster_selection/1-5-CPMcutoff-suffix-3-log-cero-replacement.csv"
)

cluster_size = 10
max_iteration = 500
epsilon = 1e-07
arbmean = TRUE

invisible(eval(parse(text=commandArgs(TRUE))))

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


#data <- data[1:100,]

selection <- repnormmixmodel.sel(
  t(data),
  k = cluster_size,
  verb = TRUE, 
  epsilon = epsilon,
  maxit = max_iteration,
  arbmean = arbmean
  )

write.csv(selection , paste(
  'results/cs_rep/selection_',
  cluster_size,
  '_maxit_',
  max_iteration,
  '.csv',
  sep = ''))

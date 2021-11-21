library("mixtools")

names = c(
  "./data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv",
  "./data/1_5_CPMcutoff_suffix_2_log_cero_replacement.csv",
  "./data/1_5_CPMcutoff_suffix_3_log_cero_replacement.csv"
)

cluster_size = 17
max_iteration = 1000
method <- "npMSL"

invisible(eval(parse(text=commandArgs(TRUE))))

#read the data
data <- NULL

for(name in names){
  print(name)
  aux_data <- as.matrix(read.csv(name)[,c(2,3,4,5)])
  if (is.null(data)){
    data <- aux_data
  }else{
    data <- cbind(data,aux_data)
  }
}


#data <- data[1:100,]

#perform the selection
# it took 1.9 days. 
# with epsilon = 1e-08,maxit = 20000, max_selection = 10
start_time <- Sys.time()
cluster_selection <- npMSL(
  data,
  cluster_size,
  blockid = c(1,2,3,4,1,2,3,4,1,2,3,4),
  samebw = TRUE ,
  verb = TRUE,
  maxiter = max_iteration
)
end_time <- Sys.time()


write.csv(cluster_selection$posteriors, paste(
  "./results/np_posteriors/posteriors",method
  ,"maxiter",max_iteration,
  "cluster-size",cluster_size,".csv", sep="_")
)
saveRDS(cluster_selection,paste(
  "./results/np_models/model",method
  ,"maxiter",max_iteration,
  "cluster-size",cluster_size,".RDS", sep="_")
  )

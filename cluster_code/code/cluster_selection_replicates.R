library("mixtools")

names = c(
  "./data/cluster_selection/1-5-CPMcutoff-suffix-1-log-cero-replacement.csv",
  "./data/cluster_selection/1-5-CPMcutoff-suffix-2-log-cero-replacement.csv",
  "./data/cluster_selection/1-5-CPMcutoff-suffix-3-log-cero-replacement.csv"
)

cluster_size = 5
max_iteration = 10

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


data <- data[1:100,]

#perform the selection
# it took 1.9 days. 
# with epsilon = 1e-08,maxit = 20000, max_selection = 10
start_time <- Sys.time()
cluster_selection <- npEM(
  data,
  cluster_size,
  blockid = c(1,2,3,4,1,2,3,4,1,2,3,4),
  samebw = TRUE ,
  verb = TRUE,
  maxiter = max_iteration
)
end_time <- Sys.time()


write.csv(cluster_selection, paste("./results/cluster_selection/sel_",max_selection,"_iter_",max_iteration,"_",substring(name,26,200), sep=""))

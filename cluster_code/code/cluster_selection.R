library("mixtools")

name = "./urepc/urepc_code/cluster_number_testing/data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv"
max_selection = 5
max_iteration = 100

invisible(eval(parse(text=commandArgs(TRUE))))
name = gsub(":","",name)
print ( max_selection)
print ( max_iteration)
print ( name)
getwd()
#read the data
data <- read.csv(name)[,c(2,3,4,5)]
data <- as.matrix(data)
#perform the selection
# it took 1.9 days. 
# with epsilon = 1e-08,maxit = 20000, max_selection = 10
start_time <- Sys.time()
cluster_selection <- repnormmixmodel.sel( 
  t(data), 
  k = max_selection, 
  verb = TRUE, 
  epsilon = 1e-07,
  maxit = max_iteration
)
end_time <- Sys.time()

print(end_time-start_time)
print(cluster_selection)
write.csv(cluster_selection, paste("./results/cluster_selection/sel_",max_selection,"_iter_",max_iteration,"_",substring(name,26,200), sep=""))

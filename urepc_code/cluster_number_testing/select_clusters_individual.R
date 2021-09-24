library("mixtools")

name = "./urepc/urepc_code/cluster_number_testing/data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv"
max_selection = 10

#read the data
data <- read.csv(name)[,c(2,3,4,5)]
data <- as.matrix(data)
#perform the selection
start_time <- Sys.time()
cluster_selection <- repnormmixmodel.sel( 
  t(data), 
  k = max_selection, 
  verb = TRUE, 
  epsilon = 1e-08,
  maxit = 20000
)
end_time <- Sys.time()

print(end_time-start_time)


saveRDS(cluster_selection,file = "./urepc/urepc_code/cluster_number_testing/results/cluster_sel_LONG_1_5_CPMcutoff_suffix_1_log_cero_replacement_k_10.RDS")
saveRDS(cluster_selection,file = "./results/cluster_sel_LONG_1_5_CPMcutoff_suffix_1_log_cero_replacement_k_10.RDS")
#foo = readRDS("./results/cluster_sel_2_0_CPMcutoff_suffix_1_log_cero_replacement_k_10.RDS")

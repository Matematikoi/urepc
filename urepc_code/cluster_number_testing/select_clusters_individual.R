library("mixtools")

name = "./data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv"
max_selection = 10

#read the data
data <- read.csv(name)[,c(2,3,4,5)]
data <- as.matrix(data)
#perform the selection
cluster_selection <- repnormmixmodel.sel(
  t(data), 
  k = max_selection, 
  verb = TRUE, 
  epsilon = 1e-03
)

saveRDS(cluster_selection,file = "./results/cluster_sel_1_5_CPMcutoff_suffix_1_log_cero_replacement_k_10.RDS")
#foo = readRDS("./results/cluster_sel_2_0_CPMcutoff_suffix_1_log_cero_replacement_k_10.RDS")

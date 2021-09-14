library("mixtools")

cluster_result <- readRDS("./results/1_5_CPMcutoff_suffix_1_log_it_1000_npEM.RDS")

write.csv(cluster_result[["posteriors"]], file = "./results/posteriors_1_5_CPMcutoff_suffix_1_log_it_1000_npEM.csv")

library("mixtools")

data <- read.csv("./data/1_5_CPMcutoff_suffix_1_log.csv", header = TRUE)
#index <- data[,1]
#data <- data.frame()
data <- data.frame(lapply(data[,-1], as.numeric), row.names = data[,1])
#heatmap(as.matrix(data))

mix <- npEM(data, 9 , samebw = FALSE ,verb = TRUE,maxiter = 1000)

saveRDS(mix, file = "./results/1_5_CPMcutoff_suffix_1_log_it_1000_npEM.RDS")

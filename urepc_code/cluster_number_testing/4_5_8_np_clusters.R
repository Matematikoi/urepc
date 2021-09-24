library("mixtools")

data = read.csv(
  "./data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv",
  header = TRUE
  )

data <- data.frame(lapply(data[,-1], as.numeric), row.names = data[,1])

start_time <- Sys.time()
mix <- npEM(data, 5 , samebw = FALSE ,verb = TRUE,maxiter = 1000)
end_time <- Sys.time()

print(end_time-start_time)

saveRDS(mix,
        file = "./results/np_models/npEM_k_5_1_5_CPMcutoff_suffix_1_log_cero_replacement.RDS")

write.csv(
  mix[["posteriors"]],
  "./results/np_posteriors/npEM_k_5_1_5_CPMcutoff_suffix_1_log_cero_replacement_posteriors.csv",
)

library("mixtools")


same_bw = TRUE
max_iter = 10
cluster_number = 15
data_set_name = "data/data.csv"
n_grid = 200
invisible(eval(parse(text=commandArgs(TRUE))))

data = read.csv(data_set_name)

start_time <- Sys.time()
mix_npMSL <- npMSL(
  log(data.matrix(data)+1), 
  cluster_number, 
  samebw = same_bw ,
  verb = FALSE,
  maxiter = max_iter,
  ngrid = n_grid
)
end_time <- Sys.time()

print(end_time-start_time)

parameters = paste(same_bw,max_iter,cluster_number, substring(data_set_name, 6, nchar(data_set_name) - 4), n_grid,sep="_")

print(parameters)

saveRDS(mix_npMSL,
        file = paste("results/sim_model_",parameters,".RDS",sep = ""))

write.csv(
  mix_npMSL[["posteriors"]],
  paste("results/sim_posteriors_",parameters,".csv", sep =""),
)

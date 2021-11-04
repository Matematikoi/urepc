library("mixtools")


same_bw = FALSE
max_iter = 1500
cluster_number = 20
data_set_name = "data/sim_data_2.csv"
n_grid = 200
bwmethod = "S"
invisible(eval(parse(text=commandArgs(TRUE))))

data = read.csv(data_set_name, row.names = 1 )

start_time <- Sys.time()
mix_npMSL <- npMSL(
  log(data.matrix(data)+1), 
  cluster_number, 
  samebw = same_bw ,
  blockid = c("a","a","b","b"),
  verb = TRUE,
  maxiter = max_iter,
  ngrid = n_grid,
  bwmethod = bwmethod
)
end_time <- Sys.time()

print(end_time-start_time)

parameters = paste(
  same_bw,
  max_iter,
  cluster_number, 
  bwmethod,
  n_grid,
  sep="_")

print(parameters)

saveRDS(mix_npMSL,
        file = paste("results/np_replicates/model_npMSL_",parameters,".RDS",sep = ""))

write.csv(
  mix_npMSL[["posteriors"]],
  paste("results/np_replicates/sim_posteriors_npMSL_",parameters,".csv", sep =""),
)


library("mixtools")


same_bw = TRUE
max_iter = 1000
cluster_number = 15
data_set_name = "./data/data.csv"
n_grid = 200
invisible(eval(parse(text=commandArgs(TRUE))))

data = read.csv(data_set_name)

start_time <- Sys.time()
mix_npMSL <- npMSL(
  log(data.matrix(data)+1), 
  cluster_number, 
  samebw = same_bw ,
  verb = TRUE,
  maxiter = max_iter,
  ngrid = n_grid
)
end_time <- Sys.time()

print(end_time-start_time)

saveRDS(mix_npMSL,
        file = "results/model.RDS")

write.csv(
  mix_npMSL[["posteriors"]],
  "results/posteriors.csv",
)

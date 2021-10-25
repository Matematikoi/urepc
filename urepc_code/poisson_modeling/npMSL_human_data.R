library("mixtools")

data = read.csv(
  "./human_sim_data/data_cero_replace.csv"
)

start_time <- Sys.time()
mix_npMSL <- npMSL(
  log(data.matrix(data)+1), 
  15, 
  samebw = TRUE ,
  verb = TRUE,
  maxiter = 10
  )
end_time <- Sys.time()

print(end_time-start_time)
 
# saveRDS(mix_npMSL,
#         file = "human_sim_data/npMSL/model_2.RDS")
# 
# write.csv(
#   mix_npMSL[["posteriors"]],
#   "human_sim_data/npMSL/posteriors_2.csv",
# )
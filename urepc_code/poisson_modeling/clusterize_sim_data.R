library(HTSCluster)
library(HTSFilter)
library(Biobase)
library("mixtools")


y_sim <- read.csv("./data/simulated_data_points.csv", row.names = 1)
conds_sim <- c("sim1","sim1","sim2","sim2")
y.filter_sim <- HTSFilter(y_sim, conds_sim, norm="TMM")

table(y.filter_sim$on) ## 11751 off, 3250 on

dat.select_sim <- y.filter_sim$filteredData
write.csv(dat.select_sim, "./results/sim_data_filtered.csv")

set.seed(12345)

#using norm instead of lib.type
start_time <- Sys.time()
PMM <- PoisMixClusWrapper(y=dat.select, gmin=1, gmax=35, conds=conds, split.init=TRUE, norm="TMM" )
end_time <- Sys.time()

end_time - start_time # 14 min

mod.BIC <- PMM$BIC.results
mod.ICL <- PMM$ICL.results
mod.Djump <- PMM$Djump.results
mod.DDSE <- PMM$DDSE.results

summary(PMM) #selected number of clusters

start_time <- Sys.time()
PMM_9 <- PoisMixClus(y=dat.select_sim, g = 9 , conds=conds_sim, norm="TMM" )
end_time <- Sys.time()

write.csv(PMM_9$labels,file = "./results/sim_data_poisson_cluster_labels.csv")
write.csv(PMM_9$probaPost,file = "./results/sim_data_poisson_cluster_posteriors.csv")

#npMSL
start_time <- Sys.time()
mix_npMSL_sim <- npMSL(dat.select_sim, 9 , samebw = TRUE ,verb = TRUE,maxiter = 1000)
end_time <- Sys.time()

end_time-start_time

write.csv(mix_npMSL_sim$posteriors, file = "./results/sim_data_npMSL_cluster_posteriors.csv")

#npMSL with log
start_time <- Sys.time()
mix_npMSL_sim <- npMSL(log(dat.select_sim), 9 , samebw = TRUE ,verb = TRUE,maxiter = 1000)
end_time <- Sys.time()

end_time-start_time

write.csv(mix_npMSL_sim$posteriors, file = "./results/sim_data_npMSL_log_cluster_posteriors.csv")




#npEM
start_time <- Sys.time()
mix_npEM_sim <- npEM(dat.select_sim, 9 , samebw = TRUE ,verb = TRUE,maxiter = 1000)
end_time <- Sys.time()

end_time-start_time

write.csv(mix_npEM_sim$posteriors, file = "./results/sim_data_npEM_cluster_posteriors.csv")

#npEM with log
start_time <- Sys.time()
mix_npEM_sim <- npEM(log(dat.select_sim), 9 , samebw = TRUE ,verb = TRUE,maxiter = 1000)
end_time <- Sys.time()

end_time-start_time

write.csv(mix_npEM_sim$posteriors, file = "./results/sim_data_npEM_log_cluster_posteriors.csv")



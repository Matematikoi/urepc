library(HTSCluster)
library(HTSFilter)
library(Biobase)


data(sultan)
conds <- as.vector(phenoData(sultan)$cell.line)
y <- exprs(sultan)
y.filter <- HTSFilter(y, conds, norm="TMM")
data_filtered <- y.filter$filteredData

first_poisson_15 <- PoisMixClus(data.matrix(data_filtered), 15, norm = "TMM",   conds=conds)

# set.seed(12345)
# #run the clustering
# start_time <- Sys.time()
# PMM <- PoisMixClusWrapper(
#   y= data.matrix(data_filtered) ,
#   gmin=1,
#   gmax=35,
#   conds=conds,
#   split.init=TRUE,
#   norm="TMM"
# )
# 
# end_time <- Sys.time()
# 
# end_time - start_time
# 
# saveRDS(PMM, file = "./human_sim_data/poisson_mixture_model.RDS")
# write.csv(data_filtered, file = "./human_sim_data/data_human.csv")
# 
# data_points <- data_filtered
# normalization <- PMM$Djump.results$norm
# lambda <- PMM$Djump.results$lambda
# labels <- PMM$Djump.results$labels
# pi <- PMM$Djump.results$pi


data_points <- data_filtered
normalization <- first_poisson_15$norm
lambda <- first_poisson_15$lambda
labels <- first_poisson_15$labels
pi <- first_poisson_15$pi



w <- rowSums(data.matrix(data_points))
s <- normalization
labels <- as.numeric(unlist(labels))

cluster_size <- length(pi)

lambda_per_cluster <- list()
for (k in seq(cluster_size)){
  lambda_per_cluster[[k]] <- outer(as.numeric(w), as.vector(unlist(lambda[,k]*normalization)), '*')
}

sim_data <- matrix(, nrow = length(w), ncol = length(normalization))
sim_lambdas <- matrix(, nrow = length(w), ncol = length(normalization))
for (row in seq(length(w))){
  sim_data[row,] <- rpois(length(normalization),lambda_per_cluster[[labels[row]]][row,])
  sim_lambdas[row,] <- lambda_per_cluster[[labels[row]]][row,]
}
# 
# write.csv(sim_data, file = "./human_sim_data/sim_data_1.csv")
# write.csv(labels, file = "./human_sim_data/sim_data_1_labels.csv")


start_time <- Sys.time()
mix_pois_sim <- PoisMixClus(data.matrix(sim_data), 15, norm = "TMM",   conds=c("a","b","a","b"))
end_time <- Sys.time()

end_time - start_time

# write.csv(mix_pois_sim$labels, file = "./human_sim_data/sim_data_poisson_clustering_14.csv")

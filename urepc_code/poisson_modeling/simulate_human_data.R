library(HTSCluster)
library(HTSFilter)
library(Biobase)


data(sultan)
conds <- as.vector(phenoData(sultan)$cell.line)
y <- exprs(sultan)
y.filter <- HTSFilter(y, conds, norm="TMM")
data_filtered <- y.filter$filteredData

first_poisson_15 <- PoisMixClus(data.matrix(data_filtered), 15, norm = "TMM",   conds=conds)

data_points <- data_filtered
normalization <- first_poisson_15$norm
lambda <- first_poisson_15$lambda
labels <- first_poisson_15$labels
pi <- first_poisson_15$pi



w <- rowSums(data.matrix(data_points))
labels <- as.numeric(unlist(labels))

cluster_size <- length(pi)


lambda_per_cluster <- list()
for (k in seq(cluster_size)){
  lambda_aux <- c(
    lambda[,k][['HEK293T']],
    lambda[,k][['HEK293T']],
    lambda[,k][['Ramos B cell']],
    lambda[,k][['Ramos B cell']]
  )
  lambda_per_cluster[[k]] <- outer(as.numeric(w), as.vector(unlist(lambda_aux*normalization)), '*')
}

sim_data <- matrix(, nrow = length(w), ncol = length(normalization))
for (row in seq(length(w))){
  sim_data[row,] <- rpois(length(normalization),lambda_per_cluster[[labels[row]]][row,])
}


sim_conds <- c("a","a","b","b")
#sim_data_filtered <- HTSFilter(sim_data, conds, norm="TMM")
write.csv(sim_data, file = "./human_sim_data/sim_data_2.csv")
write.csv(labels, file = "./human_sim_data/sim_data_2_labels.csv")


start_time <- Sys.time()
mix_pois_sim <- PoisMixClus(data.matrix(sim_data), 15, norm = "TMM",   conds=c("a","a","b","b"))
end_time <- Sys.time()

end_time - start_time

write.csv(mix_pois_sim$labels, file = "./human_sim_data/sim_data_2_poisson_clustering_14.csv")


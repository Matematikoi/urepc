library(HTSCluster)
library(HTSFilter)
library(Biobase)
library(dplyr)


# read the data and filter
fly_data_original  <- read.csv('./data/fly_data_embryos.csv', row.names = 1)
fly_data_sim <-  read.csv('./sim_databases/data_no_zeros.csv')
conditions <- colnames(fly_data_original)


start_time <- Sys.time()
mix_pois_sim <- PoisMixClus(data.matrix(fly_data_sim), 29, norm = "TMM",   conds=conditions)
end_time <- Sys.time()

end_time - start_time

write.csv(mix_pois_sim$labels, file = "./results/sim_data_poisson_clustering_29.csv")

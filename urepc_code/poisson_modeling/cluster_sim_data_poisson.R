library(HTSCluster)
library(HTSFilter)
library(Biobase)
library(dplyr)


# read the data and filter
fly_data_original  <- read.csv('./data/fly_data_embryos.csv', row.names = 1)
fly_data_sim <-  read.csv('./results/sim_data_1.csv', row.names = 1)
conditions <- colnames(fly_data_original)

fly_data_filter <- fly_data_sim%>%
  filter(0 < rowSums(.))

start_time <- Sys.time()
PoisMixClus(data.matrix(fly_data_filter), 29, norm = "TMM",   conds=conditions)
end_time <- Sys.time()

end_time - start_time

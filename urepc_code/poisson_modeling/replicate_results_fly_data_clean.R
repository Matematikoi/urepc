#import libraries
library(HTSCluster)
library(HTSFilter)
library(Biobase)
library(dplyr)


# read the data and filter
fly_data <- read.csv('./data/fly_data_embryos.csv', row.names = 1)
conditions <- colnames(fly_data)
# Can not be done since it needs replicates
#fly_data_filter <- HTSFilter(fly_data, conditions, norm="TMM")
fly_data_filter <- fly_data %>%
  filter(0 < rowSums(.))


# run the clustering
start_time <- Sys.time()
PMM <- PoisMixClusWrapper(
  y= data.matrix(fly_data_filter) , 
  gmin=1,
  gmax=60, 
  conds=conditions, 
  split.init=TRUE, 
  norm="TMM" 
)

end_time <- Sys.time()

end_time - start_time


summary(PMM)

norm_PMM <- PMM$DDSE.results$norm
lambdas <- PMM$DDSE.results$lambda
labels <- PMM$DDSE.results$labels
pis <- PMM$DDSE.results$pi
posteriors <- PMM$DDSE.results$probaPost

write.csv(norm_PMM, file = "./results/fly_data_norm.csv")
write.csv(fly_data_filter, file = "./results/fly_data_points.csv")
write.csv(lambdas, file = "./results/fly_data_lambda.csv")
write.csv(labels, file = "./results/fly_data_labels.csv")
write.csv(pis, file = "./results/fly_data_pi.csv")
write.csv(posteriors, file = "./results/fly_data_posteriors.csv" )
saveRDS(PMM, file = "./results/poisson_mixture_model.RDS")

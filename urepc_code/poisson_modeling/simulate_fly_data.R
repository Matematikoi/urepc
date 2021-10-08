library(dplyr)


data_points <- read.csv("./results/fly_data_points.csv", row.names = 1)
normalization <- read.csv("./results/fly_data_norm.csv", row.names = 1)
lambda <- read.csv("./results/fly_data_lambda.csv", row.names = 1)
labels <- read.csv("./results/fly_data_labels.csv", row.names = 1)[[1]]
pi <- read.csv("./results/fly_data_pi.csv", row.names = 1)

  
  
w <- rowSums(data.matrix(data_points))
s <- normalization
labels <- as.numeric(unlist(labels))

cluster_size <- length(pi[[1]])

lambda_per_cluster <- list()
for (k in seq(cluster_size)){
  lambda_per_cluster[[k]] <- outer(as.numeric(w), as.vector(unlist(lambda[,k]*normalization)), '*')
}

sim_data <- matrix(, nrow = length(w), ncol = length(normalization[[1]]))
for (row in seq(length(w))){
  sim_data[row,] <- rpois(length(normalization[[1]]),lambda_per_cluster[[labels[row]]][row,])
}

write.csv(sim_data, file = "./results/sim_data_1.csv")




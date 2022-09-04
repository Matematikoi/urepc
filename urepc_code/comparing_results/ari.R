library(aricode)
library(ggplot2)
getARI <- function(labels1, labels2){
  joinLabels <- merge(labels1 ,  labels2, by = "row.names", all = FALSE)
  ARI(joinLabels[,2],joinLabels[,3])
}

names <- c(
  'data/arcSinTransform_cluster_25.csv',
  'data/npEM_17_cluster_inputing.csv',
  'data/npMSL_17_cluster_inputing.csv',
  'data/poisson_17_cluster_old.csv',
  'data/2-cpm-npMSL-15-cluster.csv'
)

results <- list()

for (name in names){
  newName <- substr(name, 6,nchar(name)-4);
  results[[newName]] <- read.csv(name, row.names = 1, col.names = c('index','label'))
}

ariComparisons <- matrix( nrow = length(names), ncol = length(names), dimnames = list(names(results),names(results)))
for (name1 in names(results)){
  for (name2 in names(results)){
    ariComparisons[name1,name2] <- getARI(results[[name1]], results[[name2]])
  }
}

write.csv(ariComparisons, file = 'results/ari_comparisons.csv')

for (name in names(results)){
  data <- results[[name]]
  tempPlot <- ggplot(data, aes(x = reorder(label, label, function(x)-length(x))))+
    geom_bar()+
    labs(title = paste('histogram of cluster size in ', name), x='cluster number')
  ggsave(tempPlot, file = paste('results/histogram',name,'.png'),width = 14,height = 10, units = 'cm')
}

posteriorNames <- c(
  'data/2-cpm-npMSL-15-posterior.csv',
  'data/arcSinTransform_cluster_posterior_25_.csv',
  'data/npMSL_posteriors_17_inputting.csv'
)

for (filename in posteriorNames){
  data <- read.csv(filename, row.names = 1)
  dev.copy(png, paste('results/max_probability', substr(filename, 6,nchar(filename)-4), '.png'))
  hist(apply(data, 1, max), breaks = 'FD', main = paste('Max probability in posteriors for ', substr(filename, 6,nchar(filename)-4)))
  dev.off()
}

  
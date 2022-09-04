library(aricode)
library(ggplot2)
getARI <- function(labels1, labels2){
  joinLabels <- merge(labels1 ,  labels2, by = "row.names", all = FALSE)
  ARI(joinLabels[,2],joinLabels[,3])
}

names <- c(
  'results/clusters_1_5_npMSL_it_10000_k_17_original-k_17_.csv',
  'results_poisson/poisson_cluster_19_label.csv',
  'results_wgcna/arcSinTransform_cluster_label_19_.csv'
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

write.csv(ariComparisons, file = 'ari/ari_comparisons.csv')

for (name in names(results)){
  data <- results[[name]]
  tempPlot <- ggplot(data, aes(x = reorder(label, label, function(x)-length(x))))+
    geom_bar()+
    labs(title = paste('histogram of cluster size in ', name), x='cluster number')
  ggsave(tempPlot, file = paste('ari/histogram',substr(name,15,100),'.png'),width = 14,height = 10, units = 'cm')
}

posteriorNames <- c(
  'results/posteriors_1_5_npMSL_it_10000_k_17_original-k_17_.csv',
  'results_poisson/poisson_cluster_19_posterior.csv',
  'results_wgcna/arcSinTransform_cluster_posterior_19_.csv'
)

for (filename in posteriorNames){
  data <- read.csv(filename, row.names = 1)
  dev.copy(png, paste('ari/max_probability', substr(filename, 17,nchar(filename)-4), '.png', sep =''))
  hist(apply(data, 1, max), breaks = 'Sturges', main = paste('Max probability in posteriors for ', substr(filename, 15,nchar(filename)-4)))
  dev.off()
}


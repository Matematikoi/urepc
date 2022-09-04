# Load Libraries and necesary files

load(file = 'installed_packages.RData')
library(WGCNA)
options(stringsAsFactors = FALSE);
enableWGCNAThreads(95)


# READ THE DATA
names <- c(
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv",
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv",
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv"
)

data <- NULL
for(name in names){
  genes <- as.matrix(read.csv(name)[,c(1)])
  aux_data <- as.matrix(read.csv(name)[,c(2,3,4,5)])
  if (is.null(data)){
    data <- aux_data
  }else{
    data <- cbind(data,aux_data)
  }
  break
}
datExpr <- t(data)


#=====================================================================================
#
#  Code chunk 2
#
#=====================================================================================

hardThreshold <- pickHardThreshold(datExpr)

correlations <- cor(datExpr, method = "pearson")

hardThreshold_2 <- pickHardThreshold.fromSimilarity(correlations)

# Process the net
network <- signumAdjacencyFunction(correlations, 0.30)
stats::hclust(network, method = "complete")

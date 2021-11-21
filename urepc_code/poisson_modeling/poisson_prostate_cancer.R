library(HTSCluster)
library(HTSFilter)
library(Biobase)


names = c(
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_1.csv",
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_2.csv",
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_3.csv"
)

#read the data and combine it. 
data <- NULL
for(name in names){
  aux_data <- as.matrix(read.csv(name)[,c(2,3,4,5)])
  if (is.null(data)){
    data <- aux_data
  }else{
    data <- cbind(data,aux_data)
  }
}

conds <- c(
  "C42",
  "C42B",
  "LNCAP",
  "MR49F",
  "C42",
  "C42B",
  "LNCAP",
  "MR49F",
  "C42",
  "C42B",
  "LNCAP",
  "MR49F"
)
data <- floor(data)
data_filter <- HTSFilter(data, conds, norm="TMM")

poisson_prostate <- PoisMixClus(data.matrix(data_filter$filteredData), 17, norm = "TMM",   conds=conds)


write.csv(poisson_prostate$labels, "results/poisson_prostate_17.csv")
write.csv(data_filter$filteredData, "results/filtered_data_17.csv")

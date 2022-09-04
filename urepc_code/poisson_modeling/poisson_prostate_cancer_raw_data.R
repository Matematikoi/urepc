library(coseq)
set.seed(8888)

name <- "../cluster_number_testing/data/1_5_CPMcutoff_suffix_1.csv"
nameRawFile <- "data/liu_raw.csv"

#read the data and combine it. 
selectedGenes <- read.csv(name)[,c(1)]
rawData <- read.csv(nameRawFile)
rawData <- rawData[rawData$gene %in% selectedGenes, ]
data <- rawData[,seq(2,13)]
max_index <- 40
#Process the data
rownames(data) <- selectedGenes
conds <- c(
  "C42",
  "C42",
  "C42",
  "C42B",
  "C42B",
  "C42B",
  "LNCAP",
  "LNCAP",
  "LNCAP",
  "MR49F",
  "MR49F",
  "MR49F"
)

time_start <- Sys.time();
# gaussian mixture with arcsin
runArcSin <- coseq(data, K = 2:max_index, model = 'Normal', transformation = "arcsin");

# gaussian mixture with logit
runlogit <- coseq(data, K = 2:max_index, model = 'Normal', transformation = "logit");

# runPoisson <- coseq(rawData[,seq(2,13)], conds = conds, K = 2:10, model = 'Poisson')
# data_filter <- HTSFilter(rawData[,seq(2,13)], conds, norm="TMM")

# pmm <- PoisMixClusWrapper(y=data_filter$filteredData, gmin=1, gmax = 20, conds=conds, split.init=TRUE)
time_end <- Sys.time();

for (index in 2:max_index){
  new_index = paste("K=",index, sep = "")
  write.csv(
    runArcSin@allResults[[new_index]], 
    file = paste("results_wgcna/arcSinTransform_cluster_posterior",index,".csv", sep = "_"),
    row.names = selectedGenes
  )
  write.csv(
    runlogit@allResults[[new_index]], 
    file = paste("results_wgcna/logitTransform_cluster_posterior",index,".csv", sep = "_"),
    row.names = selectedGenes
  )
  
}

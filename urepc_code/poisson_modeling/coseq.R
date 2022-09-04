library(coseq)
set.seed(8888)

name <- "../cluster_number_testing/data/1_5_CPMcutoff_suffix_1.csv"
nameRawFile <- "data/liu_raw.csv"

#read the data and combine it. 
selectedGenes <- read.csv(name)[,c(1)]
rawData <- read.csv(nameRawFile)
rawData <- rawData[rawData$gene %in% selectedGenes, ]
data <- rawData[,seq(2,13)]

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

time_start <- Sys.time()
# runPoisson <- coseq(data, conds = conds, K = 2:40, model = 'Poisson', transformation = 'arcSin');

runArcSin <- coseq(data, K = 2:40, model = 'Normal', transformation = "arcsin");
time_end <- Sys.time()
# 
# barplot(runPoisson@metadata$ICL[1:19])
# plot(runPoisson)
# write.csv(apply(runPoisson@allResults$`K=20`,1,which.max), file = "wgcna_20.csv")

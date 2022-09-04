library("mixtools")
library("dplyr")

cpm <- '1_5'
npAlgorithm <- "npMSL"
totalIterations <- 1000 #500
fileName <- paste('./data/', cpm,'_CPMcutoff_suffix_1_log.csv', sep ="")
clusterSize <- 17; #change to 17
maxIterationSmallEM <- 5
amountOfSmallEM <- 15 #change to 10 or something like that
conds <- c(1,2,3,4)

set.seed(240)


invisible(eval(parse(text=commandArgs(TRUE))))

name = "data/prad_Liu_FPKM_1.5CPMcutoff.csv"

#read the data and combine it. 
data <- log(read.csv(name, row.names = 1, header= TRUE) +1) [,c(3,6,9,12)]

# data <- data[sample(nrow(data), size = 1000),]




getMidPoints <- function (cpm){
  return (readRDS("results/npCluster_smallEM_repnormmix_2_1_5_.RDS"))
}

start_time <- Sys.time()
nonDuplicatedCenters <- getMidPoints(cpm)
if (npAlgorithm == "npMSL"){
  npClustering <- npMSL(
    data,
    17,
    samebw = FALSE ,
    verb = TRUE,
    maxiter = totalIterations,
    blockid= conds
  )
} else {
  npClustering <- npEM(
    data,
    nonDuplicatedCenters, 
    samebw = FALSE ,
    verb = TRUE,
    maxiter = totalIterations
  )
}
end_time <- Sys.time()

end_time - start_time

posteriors <- npClustering$posteriors
cluster <- apply(posteriors, 1 , which.max)
name <- paste(
  cpm,
  npAlgorithm,
  "it",
  totalIterations,
  "k",
  length(unique(cluster)),
  "original-k",
  clusterSize,
  ".csv",
  sep = "_"
)
# write.csv(cluster, file = paste("results/clusters",name,sep ="_"), row.names = row.names(data))
# write.csv(posteriors, file = paste("results/posteriors",name,sep ="_"), row.names = row.names(data))

# saveRDS(npClustering, file = 'results/npClusterMSL_2cpm_.RDS')



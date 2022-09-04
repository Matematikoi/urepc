library("mixtools")
library("dplyr")
library("rlist")
library("parallel")
library("ids")

cpm <- '1_5'
npAlgorithm <- "repnormmix"
totalIterations <- 16 #500
clusterSize <- 17; #change to 17
maxIterationSmallEM <- 5
amountOfSmallEM <- 1000  #change to 10 or something like that
numberOfCores <- 99

set.seed(240)


invisible(eval(parse(text=commandArgs(TRUE))))

name = "data/prad_Liu_FPKM_1.5CPMcutoff.csv"

#read the data and combine it. 
data <- log(read.csv(name, row.names = 1, header= TRUE) +1) [,c(2,5,8,11)]
# small dataset

# data <- as.matrix(read.csv(fileName, row.names = 1));

# data <- data[sample(nrow(data), size = 1000),]



smallEM <- function (data, clusterSize, maxIterationSmallEM){
  result = tryCatch (
    repnormmixEM(
      t(data),
      k = clusterSize,
      verb = TRUE,
      maxit = maxIterationSmallEM,
    ),
    warning = function (w) list("loglik" = -Inf ),
    error = function (e) list("loglik" = -Inf )
  )
  return (result);
}

initializeWithSmallEM <- function (data,clusterSize,maxIterationSmallEM, numberOfCores){
  bestClustering <- NULL
  smallEMParallel <- function (clustersize) smallEM(data= data, clusterSize= clustersize, maxIterationSmallEM = maxIterationSmallEM)
  clusterings <- mclapply(integer(numberOfCores) +clusterSize, smallEMParallel, mc.cores = numberOfCores)
  # clusterings <- lapply(integer(numberOfCores) +clusterSize, smallEMParallel)
  
  return (clusterings[[which.max(list.map(clusterings, loglik))]]);
}

getMidPoints <- function (data, post, clusterSize){
  midPoints  <- matrix(ncol = ncol(data))
  for (i in seq(clusterSize)){
    acu <- integer(ncol(data))
    cnt <- 0 
    
    
    for (j in seq(nrow(data))){
      if(which.max(post[j,]) == i ){
        acu <- acu + data[j,]
        cnt <- cnt + 1
      }
    }
    if (cnt > 0 ){
      new_point <- acu/cnt
    }else{
      new_point <- data[which.max(post[,i]),] 
    }
    midPoints <- rbind(midPoints, new_point)
    
  }
  return (midPoints[-1,])
}

start_time <- Sys.time()
startingClustering <- initializeWithSmallEM (data,clusterSize,maxIterationSmallEM, numberOfCores)
midPoints <- getMidPoints(t(startingClustering$x),startingClustering$posterior,clusterSize)
nonDuplicatedCenters <- midPoints[!duplicated(midPoints),]
end_time <- Sys.time()

print(end_time - start_time)


saveRDS(nonDuplicatedCenters, file = paste('results/npCluster_smallEM_repnormmix_2',cpm,'.RDS', sep = '_'))



library("mixtools")
library("dplyr")
library("rlist")
library("parallel")
library("ids")

cpm <- '1_5'
npAlgorithm <- "npMSL"
totalIterations <- 16 #500
clusterSize <- 17; #change to 17
maxIterationSmallEM <- 3
amountOfSmallEM <- 5 #change to 10 or something like that
numberOfCores <- 5

set.seed(240)


invisible(eval(parse(text=commandArgs(TRUE))))

names = c(
  paste("./data/",cpm,"_CPMcutoff_suffix_1.csv", sep = ""),
  paste("./data/",cpm,"_CPMcutoff_suffix_2.csv", sep = ""),
  paste("./data/",cpm,"_CPMcutoff_suffix_3.csv", sep = "")
)

#read the data and combine it. 
data <- NULL
for(name in names){
  genes <- as.matrix(read.csv(name)[,c(1)])
  aux_data <- as.matrix(read.csv(name)[,c(2,3,4,5)])
  if (is.null(data)){
    data <- aux_data
  }else{
    data <- cbind(data,aux_data)
  }
}
# small dataset

# data <- as.matrix(read.csv(fileName, row.names = 1));

data <- data[sample(nrow(data), size = 1000),]



smallEM <- function (data, clusterSize, maxIterationSmallEM){
  result = tryCatch (
    if (npAlgorithm == "npMSL"){
      npClustering <- npMSL(
        data,
        clusterSize,
        samebw = FALSE ,
        verb = TRUE,
        maxiter = maxIterationSmallEM,
        blockid= c(1,2,3,4,1,2,3,4,1,2,3,4)
      )
    } else {
      npClustering <- npEM(
        data,
        clusterSize, 
        samebw = FALSE ,
        verb = TRUE,
        maxiter = maxIterationSmallEM,
        blockid= c(1,2,3,4,1,2,3,4,1,2,3,4)
      )
    },
    warning = function (w) list("loglik" =  integer(maxIterationSmallEM)-Inf ),
    error = function (e) list("loglik" = integer(maxIterationSmallEM) -Inf )
  )
  return (result);
}

initializeWithSmallEM <- function (data,clusterSize,maxIterationSmallEM, numberOfCores){
  bestClustering <- NULL
  smallEMParallel <- function (clustersize) smallEM(data= data, clusterSize= clustersize, maxIterationSmallEM = maxIterationSmallEM)
  # clusterings <- mclapply(integer(numberOfCores) +clusterSize, smallEMParallel, mc.cores = numberOfCores)
  clusterings <- lapply(integer(numberOfCores) +clusterSize, smallEMParallel)
  
  return (clusterings[[which.max(list.map(clusterings, loglik[maxIterationSmallEM]))]]);
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
midPoints <- getMidPoints(startingClustering$data,startingClustering$posteriors,clusterSize)
nonDuplicatedCenters <- midPoints[!duplicated(midPoints),]
if (npAlgorithm == "npMSL"){
  npClustering <- npMSL(
    data,
    nonDuplicatedCenters, 
    samebw = FALSE ,
    verb = TRUE,
    maxiter = totalIterations,
    blockid= c(1,2,3,4,1,2,3,4,1,2,3,4)
  )
} else {
  npClustering <- npEM(
    data,
    nonDuplicatedCenters, 
    samebw = FALSE ,
    verb = TRUE,
    maxiter = totalIterations,
    blockid= c(1,2,3,4,1,2,3,4,1,2,3,4)
  )
}
end_time <- Sys.time()

print(end_time - start_time)







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
  # break
}
set.seed(10);
# data <- data[sample(nrow(data),size=1000, replace=TRUE),]
datExpr <- t(data)


# Process the net


net <- blockwiseModules(datExpr, 
                        power = 5,
                        TOMType = "unsigned", 
                        minModuleSize = 15,
                        reassignThreshold = 0, 
                        mergeCutHeight = 0.1,
                        numericLabels = TRUE, 
                        pamRespectsDendro = FALSE,
                        saveTOMs = TRUE,
                        saveTOMFileBase = "ProstateCancerTOM", 
                        verbose = 3)

moduleLabels = net$colors
moduleColors = labels2colors(net$colors)
MEs = net$MEs;
geneTree = net$dendrograms[[1]];
max(moduleLabels)
# TOMplot



# Calculate topological overlap anew: this could be done more efficiently by saving the TOM
# calculated during module detection, but let us do it again here.
dissTOM = 1-TOMsimilarityFromExpr(datExpr, power = 5);

cmd1=cmdscale(as.dist(dissTOM),2)
sizeGrWindow(7, 6)
par(mfrow=c(1,1))
plot(cmd1, col=as.character(moduleColors),  main="MDS plot",
     xlab="Scaling Dimension 1", ylab="Scaling Dimension 2")
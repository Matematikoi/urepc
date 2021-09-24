# Replicate results from Co-expression analysis of RNA-seq data with 
# the HTSClusterpackage

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("edgeR")
BiocManager::install("HTSFilter")
BiocManager::install("HTSCluster")
BiocManager::install("Biobase", force = TRUE)


library(HTSCluster)
library(HTSFilter)
library(Biobase)
data(sultan)
conds <- as.vector(phenoData(sultan)$cell.line)
y <- exprs(sultan)


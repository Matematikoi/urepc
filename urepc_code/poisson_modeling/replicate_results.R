# Replicate results from Co-expression analysis of RNA-seq data with 
# the HTSClusterpackage

library(HTSCluster)
library(HTSFilter)
library(Biobase)
data(sultan)
conds <- as.vector(phenoData(sultan)$cell.line)
y <- exprs(sultan)

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

# This data is from human embryonic kidney (HEK293T)

data(sultan)
conds <- as.vector(phenoData(sultan)$cell.line)
y <- exprs(sultan)
# PreProcessing. Selects a treshold and eliminates the rest

y.filter <- HTSFilter(y, conds, norm="TMM")

table(y.filter$on) ## 4054 off, 4956 on
dat.select <- y.filter$filteredData

set.seed(12345)

#using norm instead of lib.type
start_time <- Sys.time()
PMM <- PoisMixClusWrapper(y=dat.select, gmin=1, gmax=35, conds=conds, split.init=TRUE, norm="TMM" )
end_time <- Sys.time()

end_time - start_time # 16 min

mod.BIC <- PMM$BIC.results
mod.ICL <- PMM$ICL.results
mod.Djump <- PMM$Djump.results
mod.DDSE <- PMM$DDSE.results

summary(PMM) #selected number of clusters

DDSE <- PMM$capushe@DDSE # DDSE results
#plot(DDSE, newwindow=F, ask=F) # DDSE diagnostic plots
DDSE@model # Model selected by DDSE

#plot(DDSE, newwindow=F, ask=F)
plot(PMM, graphs=c("ICL","BIC","capushe"), data = dat.select)

# Visualizing results

mod <- PMM$DDSE.results
summary(mod)
plot(mod, graphs=c("map","may.bycluster","lambda"), data = dat.select)

#histograms by cluster and variable  (density)
plot(mod, graphs=c("weighted.histograms"), data = dat.select)


#plot(mod, graphs="map.bycluster")

# Labels and results
labels <- mod$labels
probaPost <- mod$probaPost


plot(mod, graphs="lambda")

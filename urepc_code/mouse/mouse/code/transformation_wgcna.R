library(coseq)
# library(HTSFilter)

# set.seed(8888)

name = "data/1_5_cpm_mouse.csv"

#read the data and combine it. 
data <- read.csv(name, row.names = 1, header= TRUE)

max_index = 25
conds <- c(
  "kidney",
  "kidney",
  "liver",
  "liver",
  "lung",
  "lung",
  "short_interstine",
  "short_interstine"
)
data_filter <- data # HTSFilter(data, conds, norm="TMM")

time_start <- Sys.time();
# gaussian mixture with arcsin
runArcSin <- coseq(data_filter, K = 2:max_index, model = 'Normal', transformation = "logit",GaussianModel = "Gaussian_pk_Lk_Bk" , normFactors="none");
# runArcSin <- coseq(data_filter, K = 2:max_index, model = 'Normal', transformation = "logit" , normFactors="none");

# gaussian mixture with logit
# runlogit <- coseq(data, K = 2:max_index, model = 'Normal', transformation = "logit");

# runPoisson <- coseq(rawData[,seq(2,13)], conds = conds, K = 2:10, model = 'Poisson')
# data_filter <- HTSFilter(rawData[,seq(2,13)], conds, norm="TMM")

# pmm <- PoisMixClusWrapper(y=data_filter$filteredData, gmin=1, gmax = 20, conds=conds, split.init=TRUE)
time_end <- Sys.time();
# #
for (index in 2:max_index){
  new_index = paste("K=",index, sep = "")
  write.csv(
    runArcSin@allResults[[new_index]],
    file = paste("results_wgcna/logitTransform_cluster_posterior",index,".csv", sep = "_"),
    row.names = row.names(data_filter)
  )
  write.csv(
    apply(runArcSin@allResults[[new_index]], 1 , which.max),
    file = paste("results_wgcna/logitTransform_cluster_label",index,".csv", sep = "_"),
    row.names = row.names(data_filter)
  )

}
saveRDS(runArcSin, file = "results_wgcna/data_clustered_logistic.RDS")




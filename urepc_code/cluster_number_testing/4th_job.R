library(HTSCluster)
library(HTSFilter)
library(Biobase)
library(dplyr)

data1 = read.csv(
  "./data/1_5_CPMcutoff_suffix_1.csv",
  header = TRUE
)
data2 = read.csv(
  "./data/1_5_CPMcutoff_suffix_2.csv",
  header = TRUE
)

data1 <- data.frame(lapply(data1[,-1], as.numeric), row.names = data1[,1])
data2 <- data.frame(lapply(data2[,-1], as.numeric), row.names = data2[,1])


data_total <- bind_cols(data1,data2)
conds <- c("c42","c42b","lncap","mr49f","c42","c42b","lncap","mr49f")

y.filter <- HTSFilter(round(data_total), conds, norm="TMM")


start_time <- Sys.time()
PMM_9 <- PoisMixClus(y=y.filter$filteredData, g = 8 , conds=conds, norm="TMM" )
end_time <- Sys.time()

end_time - start_time


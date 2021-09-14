library("tidyverse")
library("mixtools")

data <- read.csv("./data/prad_Liu_FPKM_2CPMcutoff.csv")
#data <- data[1:100,]

log_scaled <- function(x) log(x+1)
data_log <- apply(data[2:13],2,log_scaled)

mix <- npEM(data_log, 3 , samebw = TRUE ,verb = TRUE)

saveRDS(mix, file = "npEM_mixture_3_comp.RDS")

library("tidyverse")
library("mixtools")

data <- read.csv("./data/prad_Liu_FPKM_2CPMcutoff.csv")
log_scaled <- function(x) log(x+1)
data_log <- apply(data[2:13],2,log_scaled)

mix <- npEM(data_log, 2 , samebw = TRUE ,verb = TRUE)

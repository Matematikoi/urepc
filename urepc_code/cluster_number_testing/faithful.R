library("mixtools")
library("ggplot2")


cluster_selection = repnormmixmodel.sel(t(faithful),6, epsilon = 1e-2)
cluster_selection = 
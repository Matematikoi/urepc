library("mixtools")
library("dplyr")

names = c(
  "./data/1_5_CPMcutoff_suffix_1_log_cero_replacement.csv",
  "./data/1_5_CPMcutoff_suffix_2_log_cero_replacement.csv",
  "./data/1_5_CPMcutoff_suffix_3_log_cero_replacement.csv"
)

cluster_size <- 20
max_iteration <- 10
cut_size <- 500
method <- "npEM"

invisible(eval(parse(text=commandArgs(TRUE))))

#read the data
data <- NULL

for(name in names){
  print(name)
  aux_data <- as.matrix(read.csv(name)[,c(2,3,4,5)])
  if (is.null(data)){
    data <- aux_data
  }else{
    data <- cbind(data,aux_data)
  }
}


# data <- data[sample(nrow(data), cut_size), ]

start_time <- Sys.time()
clusters <- repnormmixEM(
  t(data),
  k = cluster_size,
  verb = TRUE,
  maxit = max_iteration 
)
end_time <- Sys.time()

end_time - start_time



post <- data.frame(clusters$posterior)
mid_points  <- c()
for (i in seq(cluster_size)){
  acu <- integer(dim(data)[2])
  cnt <- 0 
  for (j in seq(cut_size)){
    if(which.max(post[j,]) == i ){
      acu <- acu + data[j,]
      cnt <- cnt + 1
    }
  }
  if (cnt > 0 ){
    new_point <- c(mid_points,acu/cnt)
  }else{
    new_point <- data[which.max(post[,i]),] 
  }
  mid_points <- c(mid_points, new_point)
}




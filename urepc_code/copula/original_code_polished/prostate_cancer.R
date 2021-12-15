
names = c(
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_1.csv",
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_2.csv",
  "../cluster_number_testing/data/1_5_CPMcutoff_suffix_3.csv"
)

#read the data and combine it. 
data <- NULL
for(name in names){
  genes <- as.matrix(read.csv(name)[,c(1)])
  aux_data <- as.matrix(read.csv(name)[,c(2,3,4,5)])
  if (is.null(data)){
    data <- aux_data
  }else{
    data <- cbind(data,aux_data)
  }
}
 data <- data[1:100,]
 
 result
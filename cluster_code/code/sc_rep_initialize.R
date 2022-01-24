start_time = Sys.time()
library("mixtools")

names = c(
  "./data/cluster_selection/1-5-CPMcutoff-suffix-1-log-cero-replacement.csv",
  "./data/cluster_selection/1-5-CPMcutoff-suffix-2-log-cero-replacement.csv",
  "./data/cluster_selection/1-5-CPMcutoff-suffix-3-log-cero-replacement.csv"
)

cluster_size = 20
max_iteration = 5
epsilon = 1e-07
arbmean = TRUE
centers = '0.3427277,0.7526406,1.7002166,1.2108917,2.4570630,1.8975534,4.5094471,1.9887118,2.4530551,2.6384025,3.7164326,4.1491691,1.9290633,2.4280193,3.5494549,2.1343914,1.7738513,6.0932173,4.9282233,5.1722011'
sigma = '0.3917200,0.7352922,1.1274329,1.0090379,1.2414736,2.3177128,1.4631707,1.3140649,0.9601243,1.3159100,1.4675170,2.6136707,2.3331889,2.4445397,1.3725369,2.3724144,1.3396543,1.5366949,1.0828314,1.4435838'

index = 69

invisible(eval(parse(text=commandArgs(TRUE))))

sigma = as.numeric(unlist(strsplit(sigma,split=',')))
centers = as.numeric(unlist(strsplit(centers,split=',')))

#read the data and combine it. 
data <- NULL
for(name in names){
  aux_data <- as.matrix(read.csv(name)[,c(2,3,4,5)])
  if (is.null(data)){
    data <- aux_data
  }else{
    data <- cbind(data,aux_data)
  }
}

size = length(centers)

selection <- repnormmixEM(
  t(data),
  mu = centers,
  sigma = sigma,
  verb = TRUE,
  epsilon = epsilon,
  maxit = max_iteration,
  arbmean = arbmean,

)


write(
  paste(
    index,
    selection$loglik,
    selection$restarts,
    size,
    sep=','),
  file = "results/sem/loglik_sem.csv",
  append = TRUE
)



write.csv(selection$lambda, file = paste(
  'results/sem/model_sem_lambda',
  size,
  index,
  '.csv',
  sep = '_'
))
write.csv(selection$mu, file = paste(
  'results/sem/model_sem_mu',
  size,
  index,
  '.csv',
  sep = '_'
))
write.csv(selection$sigma, file = paste(
  'results/sem/model_sem_sigma',
  size,
  index,
  '.csv',
  sep = '_'
))


end_time = Sys.time()
end_time-start_time

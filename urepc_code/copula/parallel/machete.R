saveRDS(x, file = 'data_for_tests/x.RDS')
saveRDS(mu, file = 'data_for_tests/mu.RDS')
saveRDS(sigma, file = 'data_for_tests/sigma.RDS')
saveRDS(d, file = 'data_for_tests/d.RDS')
saveRDS(K, file = 'data_for_tests/K.RDS')
saveRDS(n, file = 'data_for_tests/n.RDS')
saveRDS(km, file = 'data_for_tests/km.RDS')



with 5000 n and 4 dimensions and 15 clusters
[1] "parallel time, super parallel"
user   system  elapsed 
1094.487   37.515   91.048 
user   system  elapsed 
1468.029   43.428   44.589 

estimated for serial time <-  3000 seconds 


> View(G[,1,])
> View(t(result[[1]][,,1]))

full
user    system   elapsed 
23832.325   264.973  1729.975 
user    system   elapsed 
41007.310   333.849  1163.685 
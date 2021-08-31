library("mixtools")

# Seeing the data 
ggplot(data = faithful, mapping = aes(x=waiting))+
  geom_bar()

#using the normalmixEM to identify the mixture
wait1 <- normalmixEM(
  x = faithful[,"waiting"], 
  lambda = .5, 
  mu = c(55, 80), 
  sigma = 5
  )

plot(wait1, density=TRUE, cex.axis=1.4, cex.lab=1.4, cex.main=1.8, + 
       main2="Time between Old Faithful eruptions", xlab2="Minutes")

index = seq(1,lengths(wait1["all.loglik"]))
df = data.frame(wait1["all.loglik"],index)
ggplot( data = df, mapping = aes(y=all.loglik, x = index))+
  geom_line()


#plotting the normal curves. 



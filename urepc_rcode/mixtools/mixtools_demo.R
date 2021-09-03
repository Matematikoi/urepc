library("mixtools")

# Seeing the data 
ggplot(data = faithful, mapping = aes(x=waiting))+
  geom_bar()

#using the normalmixEM to identify the mixture
wait1 <- normalmixEM(
  x = faithful[,"waiting"], 
  )


index = seq(1,lengths(wait1["all.loglik"]))
df = data.frame(wait1["all.loglik"],index)
ggplot( data = df, mapping = aes(y=all.loglik, x = index))+
  geom_line()


#plotting the normal distributions 

ggplot(data = faithful, mapping = aes(x=waiting))+
  geom_histogram(aes(y =..density..),binwidth = 1)+
  geom_function(fun = function(x) wait1[["lambda"]][1]*dnorm(
                  x,mean = wait1[["mu"]][1], sd = wait1[["sigma"]][1]),
                color = "red",
                size = 3)+
  geom_function(fun = function(x) wait1[["lambda"]][2]*dnorm(
                  x,mean = wait1[["mu"]][2], sd = wait1[["sigma"]][2]),
                color = "green",
                size = 3)

#getting a summary
summary(wait1)



















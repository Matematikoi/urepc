original <- readRDS('results/original_result.RDS')
new <- readRDS('results/new_result.RDS')

all.equal(original,new)

data2 <-data.frame(data) 
data2$z <- as.factor(data2$z)
ggplot(data2, mapping = aes(x = V1, color = z) )+
  geom_density()

ggplot(data2, mapping = aes(x = V2, color = z) )+
  geom_density()

library("ggExtra")
library("ggplot2")
library("aricode")

original <- readRDS('results/original_result.RDS')
new <- readRDS('results/new_result.RDS')

all.equal(original,new) # should be True

data2 <-data.frame(data) 
data2$z <- as.factor(data2$z)

p <- ggplot(data2, aes(x = V1,y = V2, color = z)) + geom_point()
ggMarginal(p, groupColour = TRUE, groupFill = TRUE)

data2[['predicted_label']] <- as.factor(apply(new$hzx,1,which.max))
data2[['predicted_label_ori']] <- as.factor(apply(original$hzx,1,which.max))
p <- ggplot(data2, aes(x = V1,y = V2, color = predicted_label)) + geom_point()
ggMarginal(p, groupColour = TRUE, groupFill = TRUE)


ARI(data2$z,data2$predicted_label)
ARI(data2$z,data2$predicted_label_ori)

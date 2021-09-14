library("tidyverse")
library("mixtools")


mixture_npEM = readRDS("npEM_mixture.RDS")

df = data.frame(
  prob1 = mixture_npEM[["posteriors"]][,1],
  prob2 = mixture_npEM[["posteriors"]][,2]
)

df = df %>%
  mutate(
    label = (prob1 > prob2)+0
  )

write.csv(x=df, file="posteriors_and_label.csv")

ggplot(data = df, mapping = aes(label))+
  geom_histogram()






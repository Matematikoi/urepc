library("tidyverse")

result_data <-  as_tibble(read.csv("./results/selection_results_15.csv"))%>%
  select(-X)

aux_data <- result_data %>%
  select(starts_with("X"))%>%
  t()%>%
  as.matrix()

aux_data <- aux_data*-1
colnames(aux_data, do.NULL = FALSE)
colnames(aux_data) <- t(result_data["selection"])
aux_data <- data.frame(aux_data)
aux_data$indexes <- as.character(seq(15))
aux_data$num_indexes <- seq(15)

aux_data <- aux_data%>%
  filter(num_indexes > 5)


ggplot(aux_data, aes(x = reorder(indexes, -ICL), y = ICL, fill = indexes)) + 
  geom_bar(stat = "identity")+
  coord_cartesian(ylim = c(min(aux_data$ICL),max(aux_data$ICL)))+
  ggtitle("ICL test across different size of cluster")+
  xlab("cluster size")

ggplot(aux_data, aes(x = reorder(indexes, -BIC), y = BIC, fill = indexes)) + 
  geom_bar(stat = "identity")+
  coord_cartesian(ylim = c(min(aux_data$BIC),max(aux_data$BIC)))+
  ggtitle("BIC test across different size of cluster")+
  xlab("cluster size")

ggplot(aux_data, aes(x = reorder(indexes, -CAIC), y = CAIC, fill = indexes)) + 
  geom_bar(stat = "identity")+
  coord_cartesian(ylim = c(min(aux_data$CAIC),max(aux_data$CAIC)))+
  ggtitle("CAIC test across different size of cluster")+
  xlab("cluster size")

ggplot(aux_data, aes(x = reorder(indexes, -ICL), y = ICL, fill = indexes)) + 
  geom_bar(stat = "identity")+
  coord_cartesian(ylim = c(min(aux_data$ICL),max(aux_data$ICL)))+
  ggtitle("ICL test across different size of cluster")+
  xlab("cluster size")



  


#Esto es para discretos!!!
#selection_mult <-  multmixmodel.sel(data, comps = c(1,2), epsilon = 1e-03)
#foo <- repnormmixmodel.sel(t(data), k = 5, verb = TRUE, epsilon = 1e-01)
#saveRDS(foo, file = "./results/repnormmixmodel_sel_log_k_2.RDS")

library("mixtools")



 

names = as.matrix(read.table("./data/new_datasets_names.txt", sep=",", header=FALSE))
max_selection = 15

errors = c()
result_df <- data.frame()
for (name in names ){
  tryCatch(
    expr = {
      #read the data
      data <- read.csv(name)[,c(2,3,4,5)]
      data <- as.matrix(data)
      #perform the selection
      cluster_selection <- repnormmixmodel.sel(
        t(data), 
        k = max_selection, 
        verb = TRUE, 
        epsilon = 1e-03
      )
      
      # Prepare data
      df <- data.frame(cluster_selection)
      #add the selection method to the columns
      df <- cbind(selection = rownames(df), df)
      # add log convertion information
      is_log <- grepl( "log", name, fixed = TRUE)
      df <- cbind(log = rep(is_log,dim(df)[1]), df)
      #add origin and suborigin information
      origin <- substring(name,8,10)
      sub_origin <- substring(name,29,29)
      df <- cbind(origin = rep(origin,dim(df)[1]), df)
      df <- cbind(sub_origin = rep(sub_origin,dim(df)[1]), df)
      #add name
      df <- cbind(file = rep(name,dim(df)[1]), df)
      #Add K 
      df <- cbind(max_clusters = rep(max_selection,dim(df)[1]), df)
      rownames(df) <- 1:nrow(df)
      #adding temporary dataframe to end dataframe
      result_df <- rbind(result_df,df)
    },
    error = function(e){
      message('Caught an error!')
      message(name)
    }
  )
}

write.csv(result_df , file = "./results/selection_results_13.csv")
saveRDS(errors, file = "./results/errors_in_selection_13.RDS")

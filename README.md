# UREP-C Undergraduate Research at Purdue - Colombia
Project at Purdue university

This project is about clustering data related to prostate cancer data. It uses the R package mixtools
There are two main folders `cluster_code` and `urepc_code`. The `cluster_code` refers to code that was used tu run in cluster computing. The `urepc_code` was the general code that was used throught the research. 

# Cluster Code
It has the `code` folder where the main scripts are saved. In here the files that end with `.sub` are files that serve mainly a logistic process in the cluster, i.e. configuration files to run in cluster computing. Some important files are `npEM.R` and `npMSL.R` that mainly cluster using the `blockid` parameter. They are controlled by different `.sub` subroutines. 

The `data` folder contain files with data needed to run the different algorithms. 

The `npMSLvsPoisson` folder contains files that were used to compare our methods to the ones Dr Rau proposed. 

Finally the `results/cluster_selection` contains some of the results of the algorithms runs. 

# Urepc Code

The `mixtools` folder is only a folder to keep some experiment scripts from the mixtools package. Similarly `r4ds` is used to store some R scripts for learning purposes. 

The `first_analysis` folder keeps the very first code used to understand the data. Nothing too relevant. 

The `cluster_number_testing` is the first important folder since it keeps the data used to select the amount of clusters that we would use in clustering. Some important files are 

1. `k_means_SEM.ipynb` it runs k means multiple times on the data and prepares the data to be used in a small EM approach. 
1. `small_EM.R` runs the data from the k means using the EM approach. 

Some of this code was later used in combination with the cluster code to create the results shown. 


The `copula` folder holds important files in the `original_code_polished` folder. Mainly how to use `frank` and `gaussian` copula with our data. Then it also adds the parallelism in the `parallel` folder where `gaussian_parallel_optimized.R` would be how you paralellize using one machine with multiple cores. However there is also `cluster_code/gaussian.R` where there is the code to paralellize the copula method using multiple computers and the `cluster_code/solve_qp_compact.R` file in multiple computers. 

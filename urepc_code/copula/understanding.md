There are two main scripts

Test.R
=====




Functions.R
=============
It has 5 important functions.

findOutliers()
-------------
### Description
Short function.
Finds outliers according to a tolerance level. 
### Parameters
Receives `data`, `tol`

1. `data` is a matrix like 
1. `tol` is a tolerance for the outliers. 

###  Returns 
A binary matrix giving whether or not an item in the matrix is an outlier

### Doubts:
What is sum? 

simul()
------------
### Description
Dimension `d` is stablished as the amount of rows in `mu`.

Amount of clusters `K` is the amount of columns in `mu`

Creates a simulated dataset of copula mixture models. 




### Parameters
1. `n` is the amount of samples.
1. `Q` has two options.
    1. A list of quantile functions $G^{-1}$.
    1. A list of quantile vectors $\tilde x_i$ of dimension $n \times 1$. In the list $Q = (\tilde x_1,...,\tilde x_n)$ where the $\tilde x_j$ are a sample of $G_j$.
1. `mu` means matrix $d \times K$ where $d$ is the dimension and $K$ the amount of clusters.
1. `sigma` standard deviations matrix $d \times K$ where $d$ is the dimension and $K$ the amount of clusters.
1. `pz0` a $K$ dimensional vector that represents the mixing proportions. 
1. `theta0` a $K$ dimensional vector that represents the parameter in each of the $K$ copulas. 
1. `copulaFamilies` a $K$ list of strings describing the type of copula family for each copula. Possible options are :

    1.`'gaussian'`
    1.`'clayton'`
    1.`'frank'`
    1.`'indep'`

### Returns
A $(d+1)\times n$ matrix of generated data. The first $d$ columns correspond to the data points generated and the last one corresponds to the cluster.
### Doubts
1. What is Q? 


EMalgo()
----------

### Description
Runs EM-like algorithm
Let $n$ be the sample size of the data and $d$ be the dimension of the data. 


### Parameters
1. `data` is a 2d matrix of size $n \times d$ containing the data that you want to cluster. 
1. `method` is a string that determines the method you want to use for the clustering. Valid strings are :
    1. `"naive-deterministic"`
    1. `"naive-stochastic"`
    1. `"new-deterministic"`
    1. `"new-stochastic"`
1. `nbit` is an integer that sets the number of iterations. Every iteration will be saved. 
1. `copulaFamilies` a $K$ list of strings describing the type of copula family. The function determines the number of clusters using the size of this vector. For each copula. Possible options are :
    1.`'gaussian'`
    1.`'clayton'`
    1.`'frank'`
    1.`'indep'`
### Returns
A 10 size list containing:
1. `G` is a $n\times K \times d$ array. Where $n$ is the number of samples in the data, $K$ is the number of clusters and $d$ is the dimension of the data. IT refers to the cdf of the generator. 
1. `g`  is a $n\times K \times d$ array. Where $n$ is the number of samples in the data, $K$ is the number of clusters and $d$ is the dimension of the data. It refers to the generators. 
1. `theta` is a $1\times K \times m$ shape array where $K$ is the amount of clusters and $m$ is the number of iterations the algorithm runned. It represents the copula paramenter for each cluster, and each iteration.
1. `mu` is a $d\times K\times m$ array where $d$ is the dimension of the data, $K$ the cluster size selected, and $m$ the number of iterations. It represents the mean of each cluster, dimension, and iteration.
1. `sigma` is a $d\times K\times m$ array where $d$ is the dimension of the data, $K$ the cluster size selected, and $m$ the number of iterations. It represents the standard deviation of each cluster, dimention, and iteration.
1. `pz` is a $K\times m $ array where $K$ is the number of clusters and $m$ is number of iterations. It represents the mixing proportions at every iteration. 
1. `xtilde` is a $n \times d \times m$ array. Where n is the number of samples in the data, $d$ is the number of dimension in the data, and $m$ is the number of iterations.They are a list of quantile vectors.  
1. `hzx` is a $n \times K$ matrix where $n$ is the number of samples in the data and $K$ is the number of clusters. It represents the posterior probabilities at the last iterations.
1. `copulaFamilies` a $K$ size string vector representing the copula in each cluster. 
1. `Ovalue` is a $m$ size vector where $m$ is the number of iterations. It represents the log-likelihood of the data without the `sum(log(hzx)*hzx)` term for each iteration. 
It is the `Q(.|.)` function in the standard EM algorithm, i.e. `Q(a|a')=L(a)+H(a|a')`.
### Doubts & Comments
1. Why return every iteration? maybe later change this so that it doesn't consume as much memory and is easier to manage.
1. Probably should return the labels. 

EMalgoParamGauss()
------------------

### Description

### Parameters

### Returns

### Doubts
1. Double check the dimension of `xtilde`

dlaplace(), plaplace(), qlaplace(), rlaplace()
--------------------

### Description

### Parameters

### Returns

### Doubts

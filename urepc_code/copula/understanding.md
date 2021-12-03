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
    1. A list of quantile vectors $\tilde q_i$ of dimension $n \times 1$. In the list $Q = (\tilde q_1,...,\tilde q_n)$ where the $\tilde q_j$ are a sample of $G_j$.
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

### Parameters

### Returns

### Doubts


EMalgoParamGauss()
------------------

### Description

### Parameters

### Returns

### Doubts


dlaplace(), plaplace(), qlaplace(), rlaplace()
--------------------

### Description

### Parameters

### Returns

### Doubts

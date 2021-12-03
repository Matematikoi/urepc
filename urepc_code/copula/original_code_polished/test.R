## MIT License

## Copyright (c) 2021 Gildas Mazo

## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:

## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.

## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

source("functions.R")
library(copula)
nbcomp <- 3
mu0 <- matrix(nrow=2,ncol=nbcomp,
              c( -3,  0,
                0,   3,
                3, 0))
sigma0 <- matrix(nrow=2,ncol=nbcomp,
                 c( 2, .7,
                   .7, 1.4,
                   1.4, 2.8))
pz0 <- rep(1/nbcomp,nbcomp)
theta0 <- c(-3.45,3.45,0) 
nbit <- 30
nsample <- 300

data <- simul(nsample, Q=list(
                                function(x){qnorm(x)},
                                function(x){qlaplace(x,b=1/sqrt(2))}),
                       mu=mu0,
                       sigma=sigma0,
                       pz=pz0,
                       theta=theta0, # qlaplace(x,b=1/sqrt(2))
                       copulaFamilies=rep("frank",nbcomp)
                       ) # qnorm(x)

res <- EMalgo(data[,1:2], copulaFamilies=rep("frank",nbcomp), 
              nbit=nbit, method="naive-stochastic", commonCopula=FALSE)

str(res) 


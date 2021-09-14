library("mixtools")
library("tidyverse")
# No integrated function for extrapolating.

faithful_vector <- matrix(unlist(faithful, use.names=FALSE),ncol = 2)

centers <- matrix(c(4,80,1,50), 2, 2, byrow = TRUE)

clusters_non_parametric <- npEM(
  faithful_vector,  
  mu0 = 2, 
  samebw = FALSE,
  eps = 1e-8,
  maxiter = 5000)

#plot(clusters_non_parametric,whichplots = 2)



npEMout <- clusters_non_parametric
component=2
block=1
coords <- npEMout$blockid == block
bs <- sum(coords) # block size
xx <- as.vector(npEMout$data[,coords]) # flatten data 
wts <- rep(npEMout$post[,component],bs) # duplicate weights
if (is.matrix(npEMout$bandwidth)){
    bw <- npEMout$bandwidth[block,component]
  }else bw <- npEMout$bandwidth

# second component
block2=2
coords2 <- npEMout$blockid == block2
bs2 <- sum(coords2) # block size
xx2 <- as.vector(npEMout$data[,coords2]) # flatten data 
wts2 <- rep(npEMout$post[,component],bs2) # duplicate weights
if (is.matrix(npEMout$bandwidth)){
  bw2 <- npEMout$bandwidth[block,component]
}else bw2 <- npEMout$bandwidth

wkde(xx,c(1.8,54.0),wts,bw)
wkde(xx2,c(1.8,54.0),wts2,bw2)

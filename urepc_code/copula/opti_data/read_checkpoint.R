library ("copula")
foo <- readRDS('results_cancer/check_point.RDS')

K <- 10
n <- 16247
term1 <- term2 <- term3 <- term4 <- matrix(nrow=n,ncol=K)
it <- 1
eps <- 0

hzx <- foo$hzx
# hzx <- (1-eps)*hzx+eps
G <- foo$G
g <- foo$g
pz <- foo$pz[,it]
cop <- list()
for (z in 1:K){
  cop[[z]] <- normalCopula(dim=12,dispstr="un")
  cop[[z]]@parameters <- foo$theta[,z,it]
}

for(z in 1:K){
  print(z)
  for(i in 1:n){
    if(hzx[i,z]==0){## -Inf * 0 = 0
      term1[i,z] <- term2[i,z] <- term3[i,z] <- term4[i,z] <- 0
    }else{
      term1[i,z] <- log(dCopula(G[i,,z],copula=cop[[z]]))*hzx[i,z]
      term2[i,z] <- sum(log(g[i,,z]))*hzx[i,z]
      term3[i,z] <- log(pz[z])*hzx[i,z]
      term4[i,z] <- log(hzx[i,z])*hzx[i,z]
    }
  }
}

print(sum(term1+term2+term3)-sum(term4))

#485714.4

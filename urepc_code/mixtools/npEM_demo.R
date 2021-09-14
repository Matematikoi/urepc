library("mixtools")

###### Section 4:  Nonparametric and Semiparametric Methods
attach(faithful)
wait1 <- normalmixEM(waiting, lambda = c(0.5,0.5), mu = c(55, 80), sigma = c(5,5))


plot(wait1, which = 2, cex.axis = 1.4, cex.lab = 1.4, cex.main = 1.8,
     main2 = "Time between Old Faithful eruptions", xlab2 = "Minutes")

#Using semiparametric EM-like algorithm. 
wait2 <- spEMsymloc(waiting, mu0 = c(55, 80))
plot(wait2, lty = 2, newplot = FALSE, addlegend = FALSE)

bw.nrd0(waiting)

wait2a <- spEMsymloc(waiting, mu0 = c(55, 80), bw = 1)
wait2b <- spEMsymloc(waiting, mu0 = c(55, 80), bw = 2)
plot(wait2a, lty = 1, addlegend = FALSE, cex.axis = 1.4,
     cex.lab = 1.4, cex.main = 1.8, xlab = "Minutes",
     title = "Time between Old Faithful eruptions")
plot(wait2b, lty = 2, newplot = FALSE, addlegend = FALSE)

# S are the repetitions, n the datapoints, m mixtures, r the coordinates.
m = 2; r = 3; n = 300; S = 100
lambda <- c(0.4, 0.6)
mu <- matrix(c(0, 0, 0, 3, 4, 5), m, r, byrow = TRUE)
sigma <- matrix(rep(1, 6), m, r, byrow = TRUE)

centers <- matrix(c(0, 0, 0, 4, 4, 4), 2, 3, byrow = TRUE)
ISE <- matrix(0, m, r, dimnames = list(Components = 1:m, Blocks = 1:r))
nblabsw <- 0
# Run the montecarlo experiment
for (mc in 1:S) {
  x <- rmvnormmix(n, lambda, mu, sigma)
  a <- npEM(x, centers, verb = FALSE, samebw = FALSE)
  if (a$lambda[1] > a$lambda[2]) nblabsw <- nblabsw + 1
  for (j in 1:m) {
    for (k in 1:r) {
      ISE[j, k] <- ISE[j, k] + ise.npEM(a, j, k, dnorm,
                                        lower = mu[j, k] - 5, upper = mu[j, k] + 5, plots = FALSE,
                                        mean = mu[j, k], sd = sigma[j, k])$value
    }
  }
}
MISE <- ISE/S
print(sqMISE <- sqrt(MISE))

summary(a)

plot(a)




m <- 2; r <- 5
lambda <- c(0.4, 0.6)
df <- c(2, 10); ncp <- c(0, 8)
sh1 <- c(1, 1) ; sh2 <- c(1, 5)

n <- 300; z <- sample(m, n, rep = TRUE, prob = lambda)
r1 <- 3; z2 <- rep(z, r1)
x1 <- matrix(rt(n * r1, df[z2], ncp[z2]), n, r1)
r2 <- 2; z2 <- rep(z, r2)
x2 <- matrix(rbeta(n * r2, sh1[z2], sh2[z2]), n, r2)
x <- cbind(x1, x2)

id <- c(rep(1, r1), rep(2, r2))
centers <- matrix(c(0, 0, 0, 1/2, 1/2, 4, 4, 4, 1/2, 1/2), m, r, 
                  byrow = TRUE)
b <- npEM(x, centers, id, eps = 1e-8, verb = FALSE, samebw = FALSE)

plot(b, breaks = 15)

par(mfrow = c(2, 2))
for (j in 1:2){
  ise.npEM(b, j, 1, truepdf = dt, lower = ncp[j] - 10,
           upper = ncp[j] + 10, df = df[j], ncp = ncp[j])
  ise.npEM(b, j, 2, truepdf = dbeta, lower = -0.5,
           upper = 1.5,  shape1 = sh1[j], shape2 = sh2[j])
}


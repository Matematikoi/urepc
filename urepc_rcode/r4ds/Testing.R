# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution

x <- seq(-3, 6, length = 100);
hx1 <- dnorm(x, mean=0, sd=1);
hx2 <- dnorm(x, mean=3,sd=1);
hxComb <- 0.75*hx1 + 0.25*hx2;
plot(x,hxComb, type="l", col = "green");


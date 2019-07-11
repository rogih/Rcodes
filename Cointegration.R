# Multivariate normal
install.packages("mvtnorm")
library("mvtnorm")

# Number of time series
nvar <- 2
# Mean vetor
mu.vec <- rep(0, nvar)

# Covariance vector
cov.mat <- c(1, 0, 0, 1)
cov.mat <- matrix(cov.mat, nvar, nvar, byrow = TRUE)

# Cointegrated vector
e <- rmvnorm(250, mu.vec, cov.mat)
u.ar1 <- arima.sim(model = list(ar = 0.75), 250, innov = e[, 1])
y2 <- cumsum(e[, 2])
y1 <- y2 + u.ar1


# Plotting
par(mfrow = c(2, 1))
ts.plot(y1, y2, lty = c(1, 3), main = "Simulated bivariate cointegrated system", sub = "1 cointegrating vector, 1 common trend")
legend("bottomright", legend = c("y1", "y2"), lty = c(1, 3))
ts.plot(u.ar1, main = "Cointegrating residual")

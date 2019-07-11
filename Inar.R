# How to simulate an INAR(1) process
simuInar <- function(n, alpha, lambda) {
  y1 <- lambda / (1 - alpha)
  y <- matrix(NA, 1, n)
  y[1] <- round(y1, 0)
  for (i in 2:n) {
    x <- rbinom(1, y[i - 1], alpha)
    erro <- rpois(1, lambda)
    y[i] <- x + erro
  }
  return(y)
}

teste <- simuInar(200, 0.35, 20)
plot(as.ts(teste[1, ]), ylab = "Value", xlab = "Time")
acf(as.ts(teste[1, ]), main = "Autocorrelation INAR(1)")

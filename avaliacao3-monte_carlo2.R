# Função para implementar o LCG
lcg <- function(m, a, c, X0, N) {
  X <- numeric(N)
  X[1] <- X0
  for (i in 2:N) {
    X[i] <- (a * X[i - 1] + c) %% m
  }
  u <- X / m
  return(u)
}

# Função para a Inverse Transform Sampling
inverse_transform_sampling <- function(u, dist = "uniform", params = list(min = 0, max = 1)) {
  if (dist == "uniform") {
    return (params$min + (params$max - params$min) * u)
  } else if (dist == "normal") {
    return (qnorm(u, mean = params$mean, sd = params$sd))
  } else {
    stop("Distribuição não suportada")
  }
}

# Função para o Método de Monte Carlo com Critério de Parada por Convergência e Plotagem
monte_carlo_plot <- function(N, f, tol = 1e-6, max_iter = 10000, plot_iters = c(100, 500, 1000, 5000, 10000)) {
  estimates <- numeric(max_iter)
  par(mfrow = c(3, 2))
  
  for (i in 1:max_iter) {
    x <- runif(N)
    estimates[i] <- mean(f(x))
    
    if (i %in% plot_iters) {
      hist(f(x), main = paste("Iteração:", i), xlab = "Y", col = "blue", freq = FALSE)
      lines(density(f(x)), col = "darkblue", lwd = 2)
    }
    
    if (i > 1 && abs(estimates[i] - estimates[i-1]) < tol) {
      break
    }
  }
  return (estimates[1:i])
}

# Função para plotar os resultados
plot_results <- function(estimates, native_samples, title) {
  par(mfrow = c(2, 2))

  # Histograma
  hist(estimates, main = paste("Monte Carlo:", title), xlab = "Y", col = "blue", freq = FALSE)
  lines(density(estimates), col = "darkblue", lwd = 2)
  
  hist(native_samples, main = paste("Nativo:", title), xlab = "Y", col = "red", freq = FALSE)
  lines(density(native_samples), col = "darkred", lwd = 2)
  
  # PDF
  plot(density(estimates), main = paste("PDF -", title), xlab = "Y", col = "blue", lwd = 2)
  lines(density(native_samples), col = "red", lwd = 2)

  # CDF
  plot(ecdf(estimates), main = paste("CDF -", title), xlab = "Y", col = "blue", lwd = 2)
  lines(ecdf(native_samples), col = "red", lwd = 2)

  # PPF
  q_estimates <- quantile(estimates, probs = seq(0, 1, by = 0.01))
  q_native <- quantile(native_samples, probs = seq(0, 1, by = 0.01))
  plot(seq(0, 1, by = 0.01), q_estimates, type = "l", main = paste("PPF -", title), xlab = "Probability", ylab = "Quantile", col = "blue", lwd = 2)
  lines(seq(0, 1, by = 0.01), q_native, col = "red", lwd = 2)
}

# Cenário 1: Y = X1 + X2, X1: uniforme (0, 1), X2: normal (0, 1)
set.seed(123)
N <- 10000
f1 <- function(x) {
  u1 <- inverse_transform_sampling(x, "uniform", list(min = 0, max = 1))
  u2 <- inverse_transform_sampling(x, "normal", list(mean = 0, sd = 1))
  return (u1 + u2)
}
estimates1 <- monte_carlo_plot(N, f1)
native_samples1 <- runif(N) + rnorm(N, mean = 0, sd = 1)

plot_results(estimates1, native_samples1, "Y = X1 + X2")

# Cenário 2: Y = X, X: uniforme (0, 1)
f2 <- function(x) {
  u <- inverse_transform_sampling(x, "uniform", list(min = 0, max = 1))
  return (u)
}
estimates2 <- monte_carlo_plot(N, f2)
native_samples2 <- runif(N)

plot_results(estimates2, native_samples2, "Y = X")

# Cenário 3: Y = sum(Xi^2), i = 1,...,100; Xi: uniforme (0, 1)
f3 <- function(x) {
  u <- inverse_transform_sampling(x, "uniform", list(min = 0, max = 1))
  return (sum(u^2))
}
estimates3 <- monte_carlo_plot(N, f3)
native_samples3 <- rowSums(matrix(runif(N * 100), ncol = 100)^2)

plot_results(estimates3, native_samples3, "Y = Sum(Xi^2)")

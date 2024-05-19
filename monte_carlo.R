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

# Função para o Método de Monte Carlo com Critério de Parada por Convergência
monte_carlo <- function(N, f, tol = 1e-6, max_iter = 10000) {
  estimates <- numeric(max_iter)
  for (i in 1:max_iter) {
    x <- runif(N)
    estimates[i] <- mean(f(x))
    if (i > 1 && abs(estimates[i] - estimates[i-1]) < tol) {
      break
    }
  }
  return (estimates[1:i])
}

# Parâmetros padrão do LCG
default_m <- 2^31 - 1
default_a <- 1103515245
default_c <- 12345
default_X0 <- 1

# Número de amostras
N <- 10000

# Cenário 1: Y = X1 + X2, X1: uniforme (0, 1), X2: normal (0, 1)
set.seed(123)
f1 <- function(x) {
  u1 <- inverse_transform_sampling(x, "uniform", list(min = 0, max = 1))
  u2 <- inverse_transform_sampling(x, "normal", list(mean = 0, sd = 1))
  return (u1 + u2)
}
estimates1 <- monte_carlo(N, f1)
mean_est1 <- mean(estimates1)
sd_est1 <- sd(estimates1)

native_samples1 <- runif(N) + rnorm(N, mean = 0, sd = 1)
native_mean1 <- mean(native_samples1)
native_sd1 <- sd(native_samples1)

# Cenário 2: Y = X, X: uniforme (0, 1)
f2 <- function(x) {
  u <- inverse_transform_sampling(x, "uniform", list(min = 0, max = 1))
  return (u)
}
estimates2 <- monte_carlo(N, f2)
mean_est2 <- mean(estimates2)
sd_est2 <- sd(estimates2)

native_samples2 <- runif(N)
native_mean2 <- mean(native_samples2)
native_sd2 <- sd(native_samples2)

# Cenário 3: Y = sum(Xi^2), i = 1,...,100; Xi: uniforme (0, 1)
f3 <- function(x) {
  u <- inverse_transform_sampling(x, "uniform", list(min = 0, max = 1))
  return (sum(u^2))
}
estimates3 <- monte_carlo(N, f3)
mean_est3 <- mean(estimates3)
sd_est3 <- sd(estimates3)

native_samples3 <- rowSums(matrix(runif(N * 100), ncol = 100)^2)
native_mean3 <- mean(native_samples3)
native_sd3 <- sd(native_samples3)

# Comparando os resultados
cat("Comparação - Cenário 1:\n")
cat("Monte Carlo - Média:", mean_est1, "Desvio Padrão:", sd_est1, "\n")
cat("Nativo - Média:", native_mean1, "Desvio Padrão:", native_sd1, "\n\n")

cat("Comparação - Cenário 2:\n")
cat("Monte Carlo - Média:", mean_est2, "Desvio Padrão:", sd_est2, "\n")
cat("Nativo - Média:", native_mean2, "Desvio Padrão:", native_sd2, "\n\n")

cat("Comparação - Cenário 3:\n")
cat("Monte Carlo - Média:", mean_est3, "Desvio Padrão:", sd_est3, "\n")
cat("Nativo - Média:", native_mean3, "Desvio Padrão:", native_sd3, "\n\n")

# Plotando histogramas
par(mfrow = c(3, 2))

hist(estimates1, main = "Monte Carlo: Y = X1 + X2", xlab = "Y", col = "blue")
hist(native_samples1, main = "Nativo: Y = X1 + X2", xlab = "Y", col = "red")

hist(estimates2, main = "Monte Carlo: Y = X", xlab = "Y", col = "blue")
hist(native_samples2, main = "Nativo: Y = X", xlab = "Y", col = "red")

hist(estimates3, main = "Monte Carlo: Y = Sum(Xi^2)", xlab = "Y", col = "blue")
hist(native_samples3, main = "Nativo: Y = Sum(Xi^2)", xlab = "Y", col = "red")
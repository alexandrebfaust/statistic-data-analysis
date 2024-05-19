# Função LCG
lcg <- function(m = 2^31 - 1, a = 1103515245, c = 12345, X0 = 1, N = 100) {
  x <- numeric(N)
  x[1] <- X0
  for (i in 2:N) {
    x[i] <- (a * x[i-1] + c) %% m
  }
  u <- x / m
  return(u)
}

# Função de Transformação Inversa
inverse_transform_sampling <- function(u, distribution = "uniform", ...) {
  if (distribution == "uniform") {
    return(u)
  } else if (distribution == "normal") {
    return(qnorm(u, ...))
  } else {
    stop("Distribuição não suportada.")
  }
}

# Função Método de Monte Carlo
monte_carlo <- function(func, N = 1000, tol = 1e-5, max_iter = 10000) {
  results <- numeric(max_iter)
  for (i in 1:max_iter) {
    results[i] <- func()
    if (i > N && sd(results[1:i]) / sqrt(i) < tol) {
      return(list(mean = mean(results[1:i]), sd = sd(results[1:i]), iter = i))
    }
  }
  return(list(mean = mean(results), sd = sd(results), iter = max_iter))
}

# Cenário 1: Y = X1 + X2, X1: uniforme (0, 1) e X2: normal (0, 1)
set.seed(1)
scenario1 <- function() {
  u1 <- runif(1)
  u2 <- runif(1)
  X1 <- inverse_transform_sampling(u1, distribution = "uniform")
  X2 <- inverse_transform_sampling(u2, distribution = "normal", mean = 0, sd = 1)
  return(X1 + X2)
}
result1 <- monte_carlo(scenario1)

# Cenário 2: Y = X2, X: uniforme (0,1) ou normal (0, 1)
set.seed(1)
scenario2 <- function() {
  u <- runif(1)
  X2 <- inverse_transform_sampling(u, distribution = "normal", mean = 0, sd = 1)
  return(X2)
}
result2 <- monte_carlo(scenario2)

# Cenário 3: Y = Sum(Xi^2), i=1,...,100; Xi: uniforme (0,1)
set.seed(1)
scenario3 <- function() {
  u <- runif(100)
  X <- inverse_transform_sampling(u, distribution = "uniform")
  return(sum(X^2))
}
result3 <- monte_carlo(scenario3)

# Função para plotar os resultados
plot_results <- function(results, scenario_name) {
  cat(scenario_name, "\n")
  cat("Média:", results$mean, "\n")
  cat("Desvio Padrão:", results$sd, "\n")
  cat("Iterações:", results$iter, "\n\n")
}

plot_results(result1, "Cenário 1: Y = X1 + X2")
plot_results(result2, "Cenário 2: Y = X2")
plot_results(result3, "Cenário 3: Y = Sum(Xi^2)")

# Histograma para visualização
par(mfrow=c(3,1))
hist(result1$mean, main = "Histograma do Cenário 1")
hist(result2$mean, main = "Histograma do Cenário 2")
hist(result3$mean, main = "Histograma do Cenário 3")

# Comparação com a função de geração de números aleatórios nativa
compare_native <- function() {
  set.seed(1)
  native_uniform <- runif(1000)
  native_normal <- rnorm(1000)
  
  cat("Uniforme Nativo:\n")
  cat("Média:", mean(native_uniform), "\n")
  cat("Desvio Padrão:", sd(native_uniform), "\n\n")
  
  cat("Normal Nativo:\n")
  cat("Média:", mean(native_normal), "\n")
  cat("Desvio Padrão:", sd(native_normal), "\n\n")
}

compare_native()

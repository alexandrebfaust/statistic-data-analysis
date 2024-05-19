# 1. Implementar o LCG
# Função LCG
set.seed(123)
lcg <- function(m = 2^31 - 1, a = 1664525, c = 1013904223, X0 = 1, N) {
  x <- numeric(N)
  x[1] <- X0
  for (i in 2:N) {
    x[i] <- (a * x[i-1] + c) %% m
  }
  # Normalizar para o intervalo [0, 1]
  u <- x / m
  return(u)
}

# Teste da função LCG
N <- 1000
lcg_vals <- lcg(N = N)
head(lcg_vals)



# 2. Implementar a Inverse Transform Sampling
inverse_transform_sampling <- function(U, dist = "uniform", params = list()) {
  if (dist == "uniform") {
    a <- ifelse(is.null(params$a), 0, params$a)
    b <- ifelse(is.null(params$b), 1, params$b)
    return(a + (b - a) * U)
  } else if (dist == "normal") {
    mu <- ifelse(is.null(params$mu), 0, params$mu)
    sigma <- ifelse(is.null(params$sigma), 1, params$sigma)
    return(qnorm(U, mean = mu, sd = sigma))
  }
  stop("Distribuição não suportada")
}

# Teste da função Inverse Transform Sampling
uniform_samples <- inverse_transform_sampling(lcg_vals, dist = "uniform", params = list(a = 0, b = 1))
normal_samples <- inverse_transform_sampling(lcg_vals, dist = "normal", params = list(mu = 0, sigma = 1))
head(uniform_samples)
head(normal_samples)



# 3. Implementar o Método de Monte Carlo com critério de parada por convergência
monte_carlo <- function(func, n_max = 10000, tol = 1e-6) {
  estimates <- numeric(n_max)
  for (n in 1:n_max) {
    estimates[n] <- func()
    if (n > 1) {
      if (abs(estimates[n] - estimates[n-1]) < tol) {
        return(list(estimate = mean(estimates[1:n]), sd = sd(estimates[1:n]), n = n))
      }
    }
  }
  return(list(estimate = mean(estimates), sd = sd(estimates), n = n_max))
}

# Funções de teste
func_uniform <- function() { sum(runif(100, 0, 1)^2) }
func_normal <- function() { rnorm(1, mean = 0, sd = 1) }

# Teste da função Monte Carlo
result_uniform <- monte_carlo(func_uniform)
result_normal <- monte_carlo(func_normal)

print(result_uniform)
print(result_normal)



# 4. Testar a rotina implementada no método de Monte Carlo nos cenários fornecidos
# Cenário 1: Y=X1+X2, X1: uniforme, (0, 1) e X2: normal, (0,1)
monte_carlo_Y1 <- function() {
  X1 <- runif(1, 0, 1)
  X2 <- rnorm(1, 0, 1)
  return(X1 + X2)
}
result_Y1 <- monte_carlo(monte_carlo_Y1)


# Cenário 2: Y=X2, X: uniforme (0,1) ou normal (0, 1)
monte_carlo_Y2_uniform <- function() { return(runif(1, 0, 1)) }
monte_carlo_Y2_normal <- function() { return(rnorm(1, 0, 1)) }

result_Y2_uniform <- monte_carlo(monte_carlo_Y2_uniform)
result_Y2_normal <- monte_carlo(monte_carlo_Y2_normal)


# Cenário 3: Y=Sum(Xi2), i=1,...,100; Xi: uniforme, (0,1);
monte_carlo_Y3 <- function() {
  X <- runif(100, 0, 1)
  return(sum(X^2))
}
result_Y3 <- monte_carlo(monte_carlo_Y3)


# 5. Obter a média, desvio padrão, histograma e/ou PDF de Y
# Função para obter estatísticas
obter_estatisticas <- function(amostras) {
  media <- mean(amostras)
  desvio_padrao <- sd(amostras)
  hist(amostras, probability = TRUE, main = "Histograma")
  lines(density(amostras), col = "blue")
  return(list(media = media, desvio_padrao = desvio_padrao))
}

# Obter amostras
amostras_Y1 <- replicate(10000, monte_carlo_Y1())
amostras_Y2_uniform <- replicate(10000, monte_carlo_Y2_uniform())
amostras_Y2_normal <- replicate(10000, monte_carlo_Y2_normal())
amostras_Y3 <- replicate(10000, monte_carlo_Y3())

# Estatísticas
estatisticas_Y1 <- obter_estatisticas(amostras_Y1)
estatisticas_Y2_uniform <- obter_estatisticas(amostras_Y2_uniform)
estatisticas_Y2_normal <- obter_estatisticas(amostras_Y2_normal)
estatisticas_Y3 <- obter_estatisticas(amostras_Y3)

print(estatisticas_Y1)
print(estatisticas_Y2_uniform)
print(estatisticas_Y2_normal)
print(estatisticas_Y3)



# 6. Comparar o resultado e a taxa de convergência com a função de geração de números aleatórios nativa em R
# Função Monte Carlo com amostras nativas de R
monte_carlo_native <- function(func, n_max = 10000, tol = 1e-6) {
  estimates <- numeric(n_max)
  for (n in 1:n_max) {
    estimates[n] <- func()
    if (n > 1) {
      if (abs(estimates[n] - estimates[n-1]) < tol) {
        return(list(estimate = mean(estimates[1:n]), sd = sd(estimates[1:n]), n = n))
      }
    }
  }
  return(list(estimate = mean(estimates), sd = sd(estimates), n = n_max))
}

# Teste com funções nativas
result_native_Y1 <- monte_carlo_native(monte_carlo_Y1)
result_native_Y2_uniform <- monte_carlo_native(monte_carlo_Y2_uniform)
result_native_Y2_normal <- monte_carlo_native(monte_carlo_Y2_normal)
result_native_Y3 <- monte_carlo_native(monte_carlo_Y3)


# Comparando os resultados
cat("Comparação - Cenário 1:\n")
cat("Monte Carlo - Média:", result_Y1$estimate, "Desvio Padrão:", result_Y1$sd, "\n")
cat("Nativo - Média:", result_native_Y1$estimate, "Desvio Padrão:", result_native_Y1$sd, "\n\n")

cat("Comparação - Cenário 2 - Uniforme:\n")
cat("Monte Carlo - Média:", result_Y2_uniform$estimate, "Desvio Padrão:", result_Y2_uniform$sd, "\n")
cat("Nativo - Média:", result_native_Y2_uniform$estimate, "Desvio Padrão:", result_native_Y2_uniform$sd, "\n\n")
cat("Comparação - Cenário 2 - Normal:\n")
cat("Monte Carlo - Média:", result_Y2_normal$estimate, "Desvio Padrão:", result_Y2_normal$sd, "\n")
cat("Nativo - Média:", result_native_Y2_normal$estimate, "Desvio Padrão:", result_native_Y2_normal$sd, "\n\n")


cat("Comparação - Cenário 3:\n")
cat("Monte Carlo - Média:", result_Y3$estimate, "Desvio Padrão:", result_Y3$sd, "\n")
cat("Nativo - Média:", result_native_Y3$estimate, "Desvio Padrão:", result_native_Y3$sd, "\n\n")


# Plotando histogramas
par(mfrow = c(2, 1))

hist(amostras_Y1, main = "Monte Carlo: Y = X1 + X2", xlab = "Y", col = "blue")
hist(replicate(10000, monte_carlo_Y1()), main = "Nativo: Y = X1 + X2", xlab = "Y", col = "red")

hist(amostras_Y2_uniform, main = "Monte Carlo: Y = X Uniforme", xlab = "Y", col = "blue")
hist(replicate(10000, monte_carlo_Y2_uniform()), main = "Nativo: Y = X Uniforme", xlab = "Y", col = "red")

hist(amostras_Y2_normal, main = "Monte Carlo: Y = X Normal", xlab = "Y", col = "blue")
hist(replicate(10000, monte_carlo_Y2_normal()), main = "Nativo: Y = X Normal", xlab = "Y", col = "red")

hist(amostras_Y3, main = "Monte Carlo: Y = Sum(Xi^2)", xlab = "Y", col = "blue")
hist(replicate(10000, monte_carlo_Y3()), main = "Nativo: Y = Sum(Xi^2)", xlab = "Y", col = "red")

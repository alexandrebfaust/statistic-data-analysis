
#Passo 1: Implementar o LCG
lcg <- function(m, a, c, X0, N) {
  # Verificar se os parâmetros são fornecidos, caso contrário usar valores default
  if (missing(m)) m <- 2^31 - 1
  if (missing(a)) a <- 1664525
  if (missing(c)) c <- 1013904223
  if (missing(X0)) X0 <- 1
  
  u <- numeric(N)
  X <- X0
  
  for (i in 1:N) {
    X <- (a * X + c) %% m
    u[i] <- X / m
  }
  
  return(u)
}

# Exemplo de uso
set.seed(123)
m <- 2^31 - 1
a <- 1664525
c <- 1013904223
X0 <- 1
N <- 10000
random_numbers <- lcg(m, a, c, X0, N)

#Justificação para a escolha dos valores default
#Os valores default são escolhidos com base em literatura clássica sobre geradores de números pseudoaleatórios:
#m = 2^31 - 1: um grande número primo, que maximiza o período do gerador.
#a = 1664525 e c = 1013904223: parâmetros comumente usados no algoritmo de LCG (ex: ANSI C LCG).


#Passo 2: Implementar a Inverse Transform Sampling
inverse_transform_sampling <- function(u, distribution = "normal") {
  if (distribution == "normal") {
    return(qnorm(u))
  } else if (distribution == "uniform") {
    return(u)  # A amostra já está uniformemente distribuída
  }
}

# Exemplo de uso
uniform_samples <- lcg(m, a, c, X0, N)
normal_samples <- inverse_transform_sampling(uniform_samples, "normal")


#Passo 3: Implementar o método de Monte Carlo com critério de parada por convergência
monte_carlo <- function(func, N_max = 10000, tol = 1e-6) {
  estimates <- numeric(N_max)
  for (i in 1:N_max) {
    estimates[i] <- func()
    if (i > 1) {
      mean_estimate <- mean(estimates[1:i])
      sd_estimate <- sd(estimates[1:i])
      error_estimate <- sd_estimate / sqrt(i)
      if (error_estimate < tol) {
        break
      }
    }
  }
  return(list(mean = mean_estimate, sd = sd_estimate, N = i))
}

# Exemplo de uso com função Y = X1 + X2, onde X1 ~ U(0, 1) e X2 ~ N(0, 1)
monte_carlo_example <- function() {
  X1 <- runif(1)
  X2 <- rnorm(1)
  return(X1 + X2)
}

result <- monte_carlo(monte_carlo_example)
print(result)


#Passo 4: Testar a rotina implementada nos cenários especificados
#Cenário 1: Y = X1 + X2, X1: uniforme (0, 1) e X2: normal (0, 1)
monte_carlo_example1 <- function() {
  X1 <- runif(1)
  X2 <- rnorm(1)
  return(X1 + X2)
}

result1 <- monte_carlo(monte_carlo_example1)
print(result1)

#Cenário 2: Y = X2, X: uniforme (0, 1) ou normal (0, 1)
monte_carlo_example2 <- function() {
  X <- runif(1)
  return(X^2)
}

result2 <- monte_carlo(monte_carlo_example2)
print(result2)

#Cenário 3: Y = Sum(Xi^2), i = 1,...,100; Xi: uniforme (0,1)
monte_carlo_example3 <- function() {
  X <- runif(100)
  return(sum(X^2))
}

result3 <- monte_carlo(monte_carlo_example3)
print(result3)


#Passo 5: Obter a média, desvio padrão, histograma e/ou PDF de Y
# Função para obter estatísticas e plotar histograma
obter_estatisticas <- function(data, title) {
  mean_value <- mean(data)
  sd_value <- sd(data)
  
  hist(data, breaks = 30, main = title, xlab = "Value", ylab = "Frequency")
  return(list(mean = mean_value, sd = sd_value))
}

# Coletar amostras para cada cenário
samples1 <- replicate(10000, monte_carlo_example1())
samples2 <- replicate(10000, monte_carlo_example2())
samples3 <- replicate(10000, monte_carlo_example3())

# Obter estatísticas e plotar histogramas
stats1 <- obter_estatisticas(samples1, "Y = X1 + X2")
stats2 <- obter_estatisticas(samples2, "Y = X^2")
stats3 <- obter_estatisticas(samples3, "Y = Sum(Xi^2)")

print(stats1)
print(stats2)
print(stats3)


#Passo 6: Comparar o resultado e a taxa de convergência com a função de geração de números aleatórios nativa em R
#Para comparar, podemos usar a função runif e rnorm diretamente nas implementações dos métodos de Monte Carlo e comparar os resultados com aqueles gerados pelo LCG.
# Comparação de tempo de convergência
system.time({
  result1_lcg <- monte_carlo(monte_carlo_example1)
})
system.time({
  result1_native <- monte_carlo(function() X1 + X2)
})

print(result1_lcg)
print(result1_native)

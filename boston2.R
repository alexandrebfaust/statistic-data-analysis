# Carregar bibliotecas necessárias
library(MASS)
library(scales)
library(ggplot2)

# Carregar o dataset Boston Housing
data(Boston)

# Preparação dos dados
X <- as.matrix(cbind(Intercept = 1, Boston$rm))  # Adiciona intercepto e seleciona 'rm'
Y <- Boston$medv  # Valor médio de habitações

# Implementação da regressão linear manual
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y

# Regressão linear usando a função nativa lm()
model <- lm(medv ~ rm, data = Boston)

par(mfrow = c(1, 1))

# Plotagem dos resultados da regressão manual e usando lm() no mesmo gráfico
plot(Boston$rm, Boston$medv, main = "Comparação de Regressão Linear", xlab = "Número Médio de Quartos", ylab = "Valor Médio da Habitação",
     pch = 19, col = alpha("black", 0.5))  # pch = 19 para pontos sólidos, usando alpha para transparência

# Adiciona a linha de regressão manual
abline(beta_hat[1], beta_hat[2], col = "red", lwd = 1)  # lwd é a largura da linha

# Adiciona a linha de regressão do lm()
abline(model, col = "blue", lwd = 1)

# Adiciona a legenda
legend("topright", legend = c("Regressão Manual", "Regressão lm()"), col = c("red", "blue"), lwd = 2)

# Impressão dos coeficientes da regressão manual
print("Coeficientes da Regressão Linear Manual:")
print(beta_hat)

# Impressão dos coeficientes da regressão nativa do R
print("Coeficientes da Regressão Linear com lm():")
print(coef(model))


#####################################


# Função para simulação de Monte Carlo
simulate_regression <- function(x, y, n_simulations = 10000) {
  coefficients <- matrix(nrow = n_simulations, ncol = 2)  # Armazenar interceptos e inclinações
  for (i in 1:n_simulations) {
    indices <- sample(length(x), length(x), replace = TRUE)  # Amostragem com reposição
    sample_x <- x[indices]
    sample_y <- y[indices]
    model <- lm(sample_y ~ sample_x)
    coefficients[i, ] <- coef(model)  # Armazenar coeficientes de cada simulação
  }
  return(coefficients)
}

# Executar a simulação de Monte Carlo
set.seed(123)  # Para reprodutibilidade
X <- Boston$rm  # Variável independente
Y <- Boston$medv  # Variável dependente
results <- simulate_regression(X, Y)

# Calcular médias dos coeficientes
mean_coefficients <- colMeans(results)
ci_lower <- apply(results, 2, quantile, probs = 0.025)
ci_upper <- apply(results, 2, quantile, probs = 0.975)

# Histogramas dos coeficientes de Monte Carlo
par(mfrow = c(2, 1))  # Configurar o layout do plot para 1 linha e 2 colunas
hist(results[, 1], breaks = 50, main = "Distribuição do Intercepto", xlab = "Intercepto", col = "gray")
abline(v = mean_coefficients[1], col = "red", lwd = 2)  # Linha da média
hist(results[, 2], breaks = 50, main = "Distribuição da Inclinação", xlab = "Inclinação", col = "gray")
abline(v = mean_coefficients[2], col = "red", lwd = 2)  # Linha da média

print(paste("Média do intercepto:", mean_coefficients[1]))
print(paste("Média da inclinação:", mean_coefficients[2]))
print(paste("IC 95% para intercepto: [", ci_lower[1], ",", ci_upper[1], "]"))
print(paste("IC 95% para inclinação: [", ci_lower[2], ",", ci_upper[2], "]"))


#####################################

lcg <- function(a, c, m, seed, n) {
  x <- numeric(n)
  x[1] <- seed
  for (i in 2:n) {
    x[i] <- (a * x[i-1] + c) %% m
  }
  return(x / m)
}

# Exemplo de uso do LCG
set.seed(123)
random_numbers <- lcg(a = 1103515245, c = 12345, m = 2^31, seed = 123, n = 1000)

# Inverse Transform Sampling para distribuição exponencial
lambda <- 0.5  # taxa da distribuição exponencial
uniform_random <- runif(1000)  # Números uniformemente distribuídos
exponential_samples <- -log(1 - uniform_random) / lambda

# Gerar amostras de X
X_samples <- rnorm(1000, mean = 0, sd = 1)

# Calcular y = exp(X)
Y_samples <- exp(X_samples)

# Calcular média e desvio padrão de Y
mean_Y <- mean(Y_samples)
sd_Y <- sd(Y_samples)

print(paste("Média de Y:", mean_Y))
print(paste("Desvio padrão de Y:", sd_Y))

# Gerar números pseudo-aleatórios usando LCG
random_numbers <- lcg(a = 1103515245, c = 12345, m = 2^31, seed = 123, n = 1000)

# Gerar amostras de uma distribuição exponencial usando Inverse Transform Sampling
lambda <- 0.5
exponential_samples <- -log(1 - random_numbers) / lambda

# Aplicar uma função não-linear e estudar a propagação de incertezas
Y_samples <- exp(exponential_samples)
mean_Y <- mean(Y_samples)
sd_Y <- sd(Y_samples)

print(paste("Média de Y:", mean_Y))
print(paste("Desvio padrão de Y:", sd_Y))


#####################################

# Configuração inicial
set.seed(123)
n <- 39380  # número de amostras
lambda <- 1  # taxa para a distribuição exponencial

# Gerar números aleatórios uniformes
uniform_random <- runif(n)

# Aplicar transformação inversa para obter amostras exponenciais
exponential_samples <- -log(1 - uniform_random) / lambda

# Criar um data frame para armazenar os dados
data <- data.frame(Sample = exponential_samples)

# Criar o histograma dos dados transformados
histogram <- ggplot(data, aes(x = Sample)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "blue", alpha = 0.6) +
  labs(title = "Histograma de distribuição exponencial", x = "Valor", y = "Densidade")

# Calcular a função de distribuição acumulada
data$Cumulative <- ecdf(exponential_samples)(exponential_samples)
data <- data[order(data$Sample), ]
data$Sample_Rank <- rank(data$Sample, ties.method = "first") / n

# Plotar a função de distribuição acumulada
cum_plot <- ggplot(data, aes(x = Sample, y = Cumulative)) +
  geom_step() +
  geom_line(aes(y = Sample_Rank), color = "blue") +
  labs(title = "Função de distribuição cumulativa", x = "Valor", y = "Probabilidade cumulativa")

# Combinação dos plots em um único layout
library(gridExtra)
grid.arrange(histogram, cum_plot, nrow = 2)
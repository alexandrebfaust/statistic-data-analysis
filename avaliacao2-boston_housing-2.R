# Carregar os pacotes necessários
install.packages("MASS")  # Para o dataset Boston
install.packages("ggplot2") # Para visualização
library(MASS)
library(ggplot2)

# Carregar o dataset
data(Boston)
head(Boston)
summary(Boston)

# Dividindo os dados em treino e teste
set.seed(123)  # Para reprodutibilidade
train_index <- sample(1:nrow(Boston), 0.8*nrow(Boston))
train_data <- Boston[train_index,]
test_data <- Boston[-train_index,]

# Ajustar o modelo de regressão linear
linear_model <- lm(medv ~ ., data = train_data)
summary(linear_model)

# Previsão e avaliação do modelo
predictions <- predict(linear_model, test_data)
mse <- mean((predictions - test_data$medv)^2)
print(paste("Mean Squared Error: ", mse))

# Simulação de Monte Carlo para previsões
n_simulations <- 1000
simulated_prices <- numeric(n_simulations)

for (i in 1:n_simulations) {
  simulated_data <- as.data.frame(lapply(train_data, function(x) sample(x, replace = TRUE)))
  fit <- lm(medv ~ ., data = simulated_data)
  simulated_prices[i] <- predict(fit, newdata = data.frame(mean(train_data[, -ncol(train_data)])))
}

# Histograma das previsões simuladas
hist(simulated_prices, breaks = 30, main = "Distribuição das Previsões de Preço Simuladas", xlab = "Preço Simulado", col = "blue")

setwd("I:/mestrado/statistic-data-analysis")

#1. Carregando a base de dados Boston Housing
library(ggplot2)
library(dplyr)
library(gridExtra)

Boston <- read.csv("src/housing.data", header = FALSE, sep = "")
colnames(Boston) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

# Visualize as primeiras linhas da base de dados
head(Boston)

#2. Explorando a base de dados
# Visualize a estrutura da base de dados
str(Boston)

# Resumo estatístico da base de dados
summary(Boston)


# Calcular a média de MEDV
media_medv <- mean(Boston$MEDV)

# Criar o histograma para a variável MEDV com mais barras
hist(Boston$MEDV, 
     breaks = 30,  # Aumentar o número de barras
     main = "Histograma do Valor Médio das Casas (MEDV)",
     xlab = "Valor Médio das Casas (em $1000)",
     ylab = "Densidade",
     col = "skyblue",
     border = "black")

# Calcular e adicionar a linha de densidade
dens <- density(Boston$MEDV)
lines(dens, col = "blue", lwd = 2)

# Adicionar a linha da média de MEDV
abline(v = media_medv, col = "red", lwd = 2, lty = 2)

# Adicionar um texto para indicar a média
text(media_medv, max(dens$y), 
     labels = paste("Média =", round(media_medv, 2)), pos = 4, col = "red")


################################

# Calcular a matriz de correlação
correlation_matrix <- cor(Boston)

# Transformar a matriz de correlação em um dataframe
correlation_df <- as.data.frame(as.table(correlation_matrix))
names(correlation_df) <- c("Var1", "Var2", "Correlation")

# Plotar a matriz de correlação
ggplot(data = correlation_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(Correlation, 2)), color = "black") + # Adicionar os valores de correlação
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(x = "", y = "", title = "Matriz de Correlação - Boston Housing") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################################

# Configurar o layout para mostrar dois gráficos
par(mfrow = c(1, 2))

# Gráfico de dispersão de LSTAT x MEDV
plot(Boston$LSTAT, Boston$MEDV, 
     main = "Relação entre LSTAT e MEDV",
     xlab = "Perc. da população de baixo status (LSTAT)",
     ylab = "Valor médio das casas (MEDV)",
     pch = 19, col = "blue")

# Gráfico de dispersão de RM x MEDV
plot(Boston$RM, Boston$MEDV,
     main = "Relação entre RM e MEDV",
     xlab = "Número médio de quartos por residência (RM)",
     ylab = "Valor médio das casas (MEDV)",
     pch = 19, col = "red")


################################

# Dividir os dados em conjuntos de treinamento (80%) e teste (20%)
set.seed(123)  # Para reprodutibilidade
indices <- sample(1:nrow(Boston), size = 0.8 * nrow(Boston))
train_data <- Boston[indices, ]
test_data <- Boston[-indices, ]

# Implementar a rotina de regressão linear usando notação matricial
regressao_linear <- function(X, y) {
  # Adicionando a coluna de 1s para o intercepto
  X <- as.matrix(cbind(1, X))
  y <- as.matrix(y)
  
  # Calcular os coeficientes
  beta <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta)
}

# Aplicar a rotina na base de dados de treinamento
beta_hat <- regressao_linear(train_data$RM, train_data$MEDV)

# Aplicar a função nativa lm() para comparação
lm_model <- lm(MEDV ~ RM, data = train_data)

# Comparação dos coeficientes
coeficientes <- list(Manual = beta_hat, LM = coef(lm_model))

# Avaliação no conjunto de teste
predicoes_manual <- as.numeric(cbind(1, test_data$RM) %*% beta_hat)
predicoes_lm <- predict(lm_model, newdata = test_data)

# # Calcular RMSE
# rmse_manual <- sqrt(mean((test_data$MEDV - predicoes_manual)^2))
# rmse_lm <- sqrt(mean((test_data$MEDV - predicoes_lm)^2))

# # Mostrar resultados
# list(Coeficientes = coeficientes, RMSE_Manual = rmse_manual, RMSE_LM = rmse_lm)

# Calcular RSS
rss_manual <- sum((test_data$MEDV - predicoes_manual)^2)
rss_lm <- sum((test_data$MEDV - predicoes_lm)^2)

# Calcular RSE
rse_manual <- sqrt(rss_manual / (nrow(test_data) - 2))
rse_lm <- sqrt(rss_lm / (nrow(test_data) - 2))

# Calcular R²
tss <- sum((test_data$MEDV - mean(test_data$MEDV))^2)  # Total Sum of Squares
r_squared_manual <- 1 - (rss_manual / tss)
r_squared_lm <- 1 - (rss_lm / tss)

# Exibir resultados
list(
  RSS = list(Manual = rss_manual, LM = rss_lm),
  RSE = list(Manual = rse_manual, LM = rse_lm),
  R_squared = list(Manual = r_squared_manual, LM = r_squared_lm)
)

# Acessar e imprimir os coeficientes da regressão manual
intercepto_manual <- beta_hat[1,]
inclinação_manual <- beta_hat[2,]

# Acessar e imprimir os coeficientes do modelo lm()
intercepto_lm <- coef(lm_model)[1]
inclinação_lm <- coef(lm_model)[2]

# Exibir os coeficientes
print(paste("Coeficientes do modelo de regressão manual"))
print(paste("Intercepto:", intercepto_manual))
print(paste("Inclinação:", inclinação_manual))
cat(intercepto_manual, inclinação_manual)

print(paste("Coeficientes do modelo de regressão lm() do R"))
print(paste("Intercepto:", intercepto_lm))
print(paste("Inclinação:", inclinação_lm))
cat(intercepto_lm, inclinação_lm)


################################

par(mfrow = c(1, 1))

# Implementação da regressão linear manual
beta_hat <- regressao_linear(Boston$RM, Boston$MEDV)

# Modelo de regressão linear usando lm()
lm_model <- lm(MEDV ~ RM, data = Boston)

# Preparar o gráfico de dispersão
plot(Boston$RM, Boston$MEDV, 
     main = "Relação entre RM e MEDV com Linhas de Regressão",
     xlab = "Número médio de quartos (RM)", 
     ylab = "Valor médio das casas (MEDV, em $1000s)",
     pch = 20, col = "black")

# Adicionar a linha de regressão manual
abline(a = beta_hat[1], b = beta_hat[2], col = "blue", lwd = 2)

# Adicionar a linha de regressão do modelo lm()
abline(lm_model, col = "yellow", lwd = 2, lty = 2)

# Adicionar legenda
legend("topright", legend = c("Regressão Manual", "Regressão LM"), 
       col = c("blue", "yellow"), lty = 1:2, lwd = 2)


################################


#3. Regressão Linear Simples
# Modelo de regressão linear simples
model_simple <- lm(MEDV ~ RM, data=Boston)

# Resumo do modelo
summary(model_simple)

# Modelo de regressão linear múltipla
model_multiple <- lm(MEDV ~ ., data=Boston)

# Resumo do modelo
summary(model_multiple)

#5. Avaliação do Modelo
# Calculando o Residual Sum of Squares (RSS)
rss <- sum(residuals(model_multiple)^2)

# Calculando o Residual Standard Error (RSE)
rse <- sqrt(rss / df.residual(model_multiple))

# Calculando o R-squared
r_squared <- summary(model_multiple)$r.squared

# Exibindo os resultados
cat("RSS:", rss, "\n")
cat("RSE:", rse, "\n")
cat("R-squared:", r_squared, "\n")

regressao_linear_simples <- function(x, y) {
  # Adicionando uma coluna de uns para o intercepto
  X <- cbind(1, x)
  
  # Solução matricial para os coeficientes (beta = (X'X)^-1X'y)
  beta <- solve(t(X) %*% X) %*% t(X) %*% y
  names(beta) <- c("Intercept", "Slope")
  
  return(beta)
}

# Carregando a base de dados Boston
data("Boston", package = "MASS")

# Aplicando a função de regressão linear matricial
coeficientes_custom <- regressao_linear_simples(Boston$rm, Boston$medv)

# Utilizando a função nativa do R para comparação
modelo_lm <- lm(medv ~ rm, data = Boston)

# Exibindo os resultados
cat("Coeficientes da regressão matricial:\n")
print(coeficientes_custom)
cat("\nCoeficientes da função lm():\n")
print(coef(modelo_lm))

# Função para calcular as métricas de avaliação do modelo
avaliacao_modelo <- function(x, y, beta) {
  # Adicionando uma coluna de uns para o intercepto
  X <- cbind(1, x)
  
  # Prevendo os valores y com o modelo
  y_pred <- X %*% beta
  
  # Calculando os resíduos
  residuos <- y - y_pred
  
  # Residual Sum of Squares (RSS)
  rss <- sum(residuos^2)
  
  # Residual Standard Error (RSE)
  rse <- sqrt(rss / (length(y) - 2))
  
  # Total Sum of Squares (TSS)
  tss <- sum((y - mean(y))^2)
  
  # R-squared
  r_squared <- 1 - (rss / tss)
  
  return(list(RSS = rss, RSE = rse, R_squared = r_squared))
}

# Aplicando a função de avaliação ao modelo regressao_linear_simples
metricas_custom <- avaliacao_modelo(Boston$rm, Boston$medv, coeficientes_custom)

# Aplicando a função de avaliação ao modelo nativo lm() para comparação
y_pred_lm <- predict(modelo_lm)
rss_lm <- sum((Boston$medv - y_pred_lm)^2)
rse_lm <- sqrt(rss_lm / (length(Boston$medv) - 2))
tss_lm <- sum((Boston$medv - mean(Boston$medv))^2)
r_squared_lm <- 1 - (rss_lm / tss_lm)

metricas_lm <- list(RSS = rss_lm, RSE = rse_lm, R_squared = r_squared_lm)

# Exibindo as métricas de avaliação
cat("Métricas de avaliação do modelo regressao_linear_simples:\n")
print(metricas_custom)
cat("\nMétricas de avaliação do modelo lm():\n")
print(metricas_lm)


#6. Visualização dos Resultados
# Plotando os resíduos
plot(residuals(model_multiple))

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model_multiple)

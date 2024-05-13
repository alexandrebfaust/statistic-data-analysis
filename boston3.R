#1. Carregando a base de dados Boston Housing
# Instale o pacote MASS, caso ainda não tenha instalado
install.packages("MASS")

# Carregue o pacote MASS
library(MASS)

# Carregue a base de dados Boston
data("Boston")

# Visualize as primeiras linhas da base de dados
head(Boston)

#2. Explorando a base de dados
# Visualize a estrutura da base de dados
str(Boston)

# Resumo estatístico da base de dados
summary(Boston)

#3. Regressão Linear Simples
# Modelo de regressão linear simples
model_simple <- lm(medv ~ rm, data=Boston)

# Resumo do modelo
summary(model_simple)

#4. Regressão Linear Múltipla
# Modelo de regressão linear múltipla
model_multiple <- lm(medv ~ ., data=Boston)

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

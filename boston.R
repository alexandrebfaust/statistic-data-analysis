# Carregar a biblioteca MASS que contém a base de dados Boston
library(MASS)

# Visualizar as primeiras linhas da base de dados
head(Boston)

# Gráfico de correlação usando a biblioteca corrplot
library(corrplot)
correlation_matrix <- cor(Boston)
corrplot(correlation_matrix, method = "circle")

# Histograma do preço das casas
hist(Boston$medv, breaks = 30, main = "Distribuição dos Preços das Casas", xlab = "Preço em $1000s")

# Diagrama de dispersão da taxa de criminalidade versus preço das casas
plot(Boston$crim, Boston$medv, xlab = "Taxa de Criminalidade", ylab = "Preço das Casas", main = "Criminalidade vs. Preço das Casas")
abline(lm(Boston$medv ~ Boston$crim), col = "red")  # Adicionando a linha de regressão

# Gerar uma regressão linear simples
model <- lm(medv ~ lstat, data = Boston)  # medv: preço das casas, lstat: % menor status da população

# Plot com linha de ajuste
plot(Boston$lstat, Boston$medv, main = "LSTAT vs MEDV", xlab = "LSTAT", ylab = "MEDV")
abline(model, col = "blue")

# Gerar uma regressão linear simples
model <- lm(medv ~ lstat, data = Boston)  # medv: preço das casas, lstat: % menor status da população

# Histograma de resíduos
hist(model$residuals, breaks = 30, main = "Histograma de Resíduos", xlab = "Resíduos", col = "gray")


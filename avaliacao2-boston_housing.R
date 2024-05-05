setwd("I:/mestrado/statistic-data-analysis")
library(ggplot2)
library(dplyr)
library(gridExtra)

# Carregar a base de dados - Fonte: https://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data
data <- read.csv("src/housing.data", header = FALSE, sep = "")
colnames(data) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

# Calcular a matriz de correlação
correlation_matrix <- cor(data)

# Transformar a matriz de correlação em um dataframe
correlation_df <- as.data.frame(as.table(correlation_matrix))
names(correlation_df) <- c("Var1", "Var2", "Correlation")

# Plotar a matriz de correlação
ggplot(data = correlation_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "black") +
  scale_fill_gradient(low = "blue", mid = "white", high = "red") +
  labs(x = "", y = "", title = "Matriz de Correlação - Boston Housing") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###################


# Definir a variável alvo
target_variable <- "MEDV"


###################


# Histograma
hist_plot <- ggplot(data, aes_string(x = target_variable)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(x = "Preço Mediano das Casas",
       y = "Frequência",
       title = "Histograma - Boston Housing") +
  theme_minimal()

# Função de densidade de probabilidade (PDF)
pdf_plot <- ggplot(data, aes_string(x = target_variable)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(x = "Preço Mediano das Casas",
       y = "Densidade",
       title = "PDF - Boston Housing") +
  theme_minimal()

# Função de distribuição acumulada (CDF)
cdf_plot <- ggplot(data, aes_string(x = target_variable)) +
  stat_ecdf(geom = "step", color = "blue") +
  labs(x = "Preço Mediano das Casas",
       y = "Probabilidade",
       title = "CDF - Boston Housing") +
  theme_minimal()

# Função inversa da distribuição acumulada (PPF)
ppf_data <- data %>%
  mutate(ppf = qnorm(rank(!!sym(target_variable))/n(), mean = mean(!!sym(target_variable), na.rm = TRUE), sd = sd(!!sym(target_variable), na.rm = TRUE)))

ppf_plot <- ggplot(ppf_data, aes(x = ppf)) +
  geom_line(stat = "ecdf", color = "red") +
  labs(x = "Quantil",
       y = "Valor",
       title = "PPF - Boston Housing") +
  theme_minimal()

# Plotar os gráficos individualmente
hist_plot
pdf_plot
cdf_plot
ppf_plot

# Plotar os gráficos em uma grade 2x2
grid.arrange(hist_plot, pdf_plot, cdf_plot, ppf_plot, ncol = 2)


###################


# Gráfico de dispersão dos dados
ggplot(data, aes(x = RM, y = MEDV)) +
  geom_point() +
  labs(x = "Número Médio de Quartos por Habitação",
       y = "Preço Mediano das Casas",
       title = "Dispersão dos Dados - Boston Housing") +
  theme_minimal()

# Função para plotar a linha de regressão
plot_regression_line <- function(x, y, intercept, slope) {
  df <- data.frame(x = x, y = y)
  df <- df[order(df$x), ]
  df$y_pred <- intercept + slope * df$x
  return(df)
}

# Obter os coeficientes da regressão
intercept <- model$intercept
slope <- model$slope

# Plotar a linha de regressão
regression_data <- plot_regression_line(x, y, intercept, slope)
ggplot(data, aes(x = RM, y = MEDV)) +
  geom_point() +
  geom_line(data = regression_data, aes(x = x, y = y_pred), color = "red") +
  labs(x = "Número Médio de Quartos por Habitação",
       y = "Preço Mediano das Casas",
       title = "Regressão Linear - Boston Housing") +
  theme_minimal()


###################


# Implementação da regressão linear de uma variável
simple_linear_regression <- function(x, y) {
  # Adicionar uma coluna de uns para o termo constante
  X <- cbind(1, x)
  # Calcular o estimador dos coeficientes beta
  beta <- solve(t(X) %*% X) %*% t(X) %*% y
  return(beta)
}

# Separar os dados em variáveis independentes (x) e variável dependente (y)
x <- data$RM  # Número médio de quartos por habitação
y <- data$MEDV  # Preço mediano das casas

# Aplicar a função de regressão linear implementada
beta <- simple_linear_regression(x, y)
cat("Coeficientes obtidos pela minha implementação:\n")
cat("Intercept:", beta[1], "\n")
cat("Slope:", beta[2], "\n")

# Comparar com a função nativa de regressão linear do R
cat("\nCoeficientes obtidos pela função lm():\n")
summary(lm(MEDV ~ RM, data = data))


###################


# Criar os dados para plotagem da regressão linear implementada
regression_data_custom <- data.frame(RM = x, MEDV = y, Predicted_MEDV = beta[1] + beta[2] * x)

# Plotar a regressão linear implementada
ggplot(data, aes(x = RM, y = MEDV)) +
  geom_point() +
  geom_line(data = regression_data_custom, aes(x = RM, y = Predicted_MEDV), color = "blue") +
  labs(x = "Número Médio de Quartos por Habitação",
       y = "Preço Mediano das Casas",
       title = "Regressão Linear Implementada") +
  theme_minimal()

# Comparar com a função nativa de regressão linear do R
ggplot(data, aes(x = RM, y = MEDV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Número Médio de Quartos por Habitação",
       y = "Preço Mediano das Casas",
       title = "Regressão Linear pelo R") +
  theme_minimal()

# Plotar ambas as regressões lineares no mesmo gráfico
ggplot(data, aes(x = RM, y = MEDV)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Função nativa de regressão linear
  geom_abline(intercept = beta[1], slope = beta[2], color = "blue") +  # Nossa implementação de regressão linear
  labs(x = "Número Médio de Quartos por Habitação",
       y = "Preço Mediano das Casas",
       title = "Comparação de Regressões Lineares") +
  theme_minimal()

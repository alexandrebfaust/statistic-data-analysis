setwd("I:/mestrado/statistic-data-analysis")

# Carregando os dados
usair <- read.csv(file="src/usair.csv",head=TRUE,sep=",")

# Resumo das medidas descritivas para as variáveis selecionadas
summary(usair[c("SO2", "temp", "popn")])

# Definir uma função para realizar o teste de Shapiro-Wilk e interpretar os resultados
testar_normalidade <- function(data, variable_name) {
  test_result <- shapiro.test(data)
  cat("\nTeste de Shapiro-Wilk para", variable_name, ":\n")
  print(test_result)
  if (test_result$p.value < 0.05) {
    cat("Resultado:", variable_name, "não segue uma distribuição normal (p < 0.05)\n")
  } else {
    cat("Resultado:", variable_name, "pode seguir uma distribuição normal (p >= 0.05)\n")
  }
}

boxplot(
    usair$SO2, 
    main="Boxplot de SO2", 
    ylab="Níveis de SO2"
)

# Plotando histogramas
par(mfrow = c(3, 1))
hist(
    usair$SO2, 
    main = "Histograma de SO2", 
    xlab = "Níveis de SO2"
)
hist(
    usair$temp, 
    main = "Histograma de Temperatura", 
    xlab = "Temperatura °C"
)
hist(
    usair$popn, 
    main = "Histograma de População", 
    xlab = "População"
)

# Teste de Shapiro-Wilk para SO2
# Aplicar a função para cada variável
testar_normalidade(usair$SO2, "SO2")
testar_normalidade(usair$temp, "Temperatura")
testar_normalidade(usair$popn, "População")

# Q-Q plot para SO2
par(mfrow = c(1, 1))
qqnorm(usair$SO2)
qqline(usair$SO2, col = "red")

# Plotando gráfico de dispersão
plot(
    usair$temp, 
    usair$SO2, 
    main = "Temperatura x SO2", 
    xlab = "Temperatura °C", 
    ylab = "SO2"
)

# Cálculo do Intervalo de Confiança para a média de SO2
n <- length(usair$SO2)
mean_so2 <- mean(usair$SO2)
sd_so2 <- sd(usair$SO2)
error_margin <- qt(0.975, df=n-1) * sd_so2 / sqrt(n)
ci_lower <- mean_so2 - error_margin
ci_upper <- mean_so2 + error_margin

summary(usair[c("SO2", "temp", "popn")])

c("Média SO2:", mean_so2, "Margem de Erro:", error_margin)
cat("Intervalo de Confiança para a média de SO2:", ci_lower, ci_upper)
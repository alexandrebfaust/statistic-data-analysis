setwd("I:/mestrado/statistic-data-analysis")

# Carregando os dados
advertising <- read.csv(file="src/advertising.csv",head=TRUE,sep=",")

# Resumo das medidas descritivas para a variável 'Sales'
summary(advertising$Sales)

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

# Plotando histograma de 'Sales'
hist(
    advertising$Sales, 
    main = "Distribuição de Vendas", 
    xlab = "Vendas", 
    breaks = 10
)

# Plotando gráficos de dispersão para verificar correlações
par(mfrow = c(1, 3))
plot(
    advertising$TV, 
    advertising$Sales, 
    main = "Vendas x TV", 
    xlab = "Gastos em TV", 
    ylab = "Vendas"
    )
plot(
    advertising$Radio, 
    advertising$Sales, 
    main = "Vendas x Radio", 
    xlab = "Gastos em Rádio", 
    ylab = "Vendas"
    )
plot(
    advertising$Newspaper, 
    advertising$Sales, 
    main = "Vendas x Jornal", 
    xlab = "Gastos em Jornal", 
    ylab = "Vendas"
)

# Teste de Shapiro-Wilk para normalidade
testar_normalidade(advertising$Sales, "Sales")

# Q-Q plot para Sales
par(mfrow = c(1, 1))
qqnorm(advertising$Sales)
qqline(advertising$Sales, col = "red")

# Intervalo de confiança para a média de 'Sales'
mean_sales <- mean(advertising$Sales)
sd_sales <- sd(advertising$Sales)
n <- length(advertising$Sales)
error_margin <- qt(0.975, df = n-1) * (sd_sales / sqrt(n))

ci_lower <- mean_sales - error_margin
ci_upper <- mean_sales + error_margin

c("Média Vendas:", mean_sales, "Margem de Erro:", error_margin)
cat("Intervalo de Confiança para a média de Sales:", ci_lower, ci_upper)
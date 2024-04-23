setwd("I:/mestrado/statistic-data-analysis")

# Carregando os dados
advertising <- read.csv(file="src/advertising.csv",head=TRUE,sep=",")

# Resumo das medidas descritivas para a variável 'Sales'
summary(advertising$Sales)

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
shapiro.test(advertising$Sales)

# Intervalo de confiança para a média de 'Sales'
media_sales <- mean(advertising$Sales)
sd_sales <- sd(advertising$Sales)
n <- length(advertising$Sales)
error_margin <- qt(0.975, df = n-1) * (sd_sales / sqrt(n))

ci_lower <- media_sales - error_margin
ci_upper <- media_sales + error_margin

c("Média Vendas:",mean_sales, "Margem de Erro:", error_margin)
c(ci_lower, ci_upper)

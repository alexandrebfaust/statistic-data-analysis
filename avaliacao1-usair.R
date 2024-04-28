setwd("I:/mestrado/statistic-data-analysis")

# Carregando os dados
usair <- read.csv(file="src/usair.csv",head=TRUE,sep=",")

# Resumo das medidas descritivas para as variáveis selecionadas
summary(usair[c("SO2", "temp", "popn")])

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

# Teste de Shapiro-Wilk para normalidade
shapiro.test(usair$SO2)

# Plotando gráfico de dispersão
par(mfrow = c(1, 1))
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

c("Média Vendas:", mean_so2, "Margem de Erro:", error_margin)
cat("Intervalo de Confiança para a média de SO2:", ci_lower, ci_upper)
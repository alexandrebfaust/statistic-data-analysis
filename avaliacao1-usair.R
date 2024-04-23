setwd("I:/mestrado/statistic-data-analysis")

# Carregando os dados
usair <- read.csv(file="src/usair.csv",head=TRUE,sep=",")

# Resumo das medidas descritivas para as variáveis selecionadas
summary(usair[c("SO2", "temp", "popn")])

# Plotando histogramas
par(mfrow = c(3, 1))
hist(usair$SO2, main = "Histograma de SO2", xlab = "Níveis de SO2")
hist(usair$temp, main = "Histograma de Temperatura", xlab = "Temperatura °C")
hist(usair$popn, main = "Histograma de População", xlab = "População")

# Plotando gráfico de dispersão
par(mfrow = c(1, 1))
plot(usair$temp, usair$SO2, main = "Temperatura vs. SO2", xlab = "Temperatura °C", ylab = "SO2")

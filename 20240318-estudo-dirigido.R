install.packages("HH") #Instalar pacote HH
library(HH) #2. Carregar pacote HH
#Escolher o dataset "usair"; contém dados sobre poluição do ar em diversas cidades dos EUA, meados de 1900.
#3. Acessar os dados:
data(usair)

#4. Acessar algumas das variáveis individualmente, p.ex.: temperatura, concentração de SO2, precipitação anual média, etc
#Ex1.: usando notação comum
temp <- usair$temp
so2 <- usair$SO2
precip <- usair$precip

#Ex2.: usando attach
attach(usair)

#5. Plotar diagrama de dispersão com duas variáveis escolhidas:  plot(x,y), Configurar gráfico com título e legendas no eixos
plot(temp, so2, main="Dispersão de SO2 x Temperatura", xlab="Temperatura (F°)", ylab="Conc. de SO2")


#6. Editar o dataset: Ler Dalgaard 2.1.8
#Converter temperaturas de farenheit para Celsius - usar transform
#Obs.: após usar o comando transform, caso deseje, é necessário usar o comando attach novamente.
usair <- transform(usair, temp = (temp - 32) * 5/9)
attach(usair)

#Plotando em Celsius
temp <- usair$temp
so2 <- usair$SO2
precip <- usair$precip
plot(temp, so2, main="Dispersão de SO2 x Temperatura", xlab="Temperatura (C°)", ylab="Conc. de SO2")


#7. Explorar associações entre variáveis por meio de uma matriz de gráficos de dispersão (scatterplot matrix), splom(usair)
data(usair)
splom(~usair, main="Matriz de Dispersão usair")


summary(so2) # Para média, mediana, mín., máx., 1º e 3º quartis
sd(so2) # Para desvio-padrão

hist(so2, main="Histograma da Concentração de SO2", xlab="Concentração de SO2")

boxplot(so2, main="Boxplot da Concentração de SO2", ylab="Concentração de SO2")

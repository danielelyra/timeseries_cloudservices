#####
### Libraries
#####

library(readxl)
library(dplyr)
library(forecast)
library(lubridate)
library(zoo)
library(urca)


#####
### Importar Dados
#####

Consumo <- read_excel("C:/Users/ACMELO/OneDrive/Current/SIntelectual/0MBA/08 - Análise de Series Temporai (2)/Trabalho 2/Consumo.xlsx", 
                      col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric"))
View(Consumo)


#####
### Analise Inicial
#####

#Plotar Total
plot(Consumo$Total~Consumo$Periodo, xlab="Tempo", ylab="Consumo", bty="l")

# Analise Estatistica
SumCs <- summary(Consumo$Total)               ## Armazena Summary
AmpCs <- SumCs[6] - SumCs[1]                   ## Calcula Amplitude
TRUE %in% is.na(Consumo$Total)                ## Display se tem nulo -> Variavel COM Nulo
SumCs                                          ## Display Summary
AmpCs                                          ## Demonstra Amplitude
sd(Consumo$Total, na.rm=TRUE)                 ## Demonstra Desvio Padrao
boxplot (Consumo$Total, col=8); grid(col=1)   ## Gera BoxPlot -> Sem Outliers
hist(Consumo$Total, col=8);grid(col=1)        ## Gera Histograma, aproximacao com normal, leve desvio a esquerda

#coverte a serie de vendas no formato de serie temporal
cs_ts <- ts(Consumo$Total, start=c(2018,6), end=c(2020,5), frequency = 12)

# Plotar o grafico da serie temporal
plot(cs_ts, xlab="Tempo", ylab="Comsuno", bty="l")

# Calcular o resumo estatistico da base
summary(Consumo)
summary(cs_ts)


#####
### Quebra em Desenvolvimento e Teste
#####

#Desenvolvimento: 2018/6 até 2019/11 e Teste: 2019/12 até 2020/5
cs_amostra_teste <- 6
cs_amostra_treinamento <- length(cs_ts) - cs_amostra_teste

#cria a serie temporal de treinamento e teste
treinamento_cs <- window(cs_ts, start=c(2018,  6), end=c(2019,11))
teste_cs   <- window(cs_ts, start=c(2019, 12), end=c(2020, 5))

#plota o grafico da serie temporal de treinamento e teste
plot(treinamento_cs, xlab="Tempo", ylab="Consumo", xaxt="n" , xlim=c(2018, 2021),  ylim=c(5000, 7000000), bty="l")
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(teste_cs, bty="l", col="red")


#####
###Calcula modelo naive
#####

md_naive <- naive(treinamento_cs, level=0, h=cs_amostra_teste)
summary(md_naive)

#Verifica a acuracia do modelo naive
accuracy(md_naive, teste_cs)

#plot serie temporal de treinamento e teste
plot(md_naive, xlab="Tempo", ylab="Consumo", xaxt="n" , xlim=c(2018, 2021),  ylim=c(5000, 7000000), bty="l", flty=2)
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(teste_cs, bty="l", col="red")


#####
### Calcula modelo linear
#####

md_tslm <- tslm(treinamento_cs ~ trend)
summary(md_tslm)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(md_tslm, test="LB")

# Calcula modelo período de validacao
md_tslm_fc <- forecast(md_tslm, h = cs_amostra_teste, level=0.95)

#Verifica a acuracia do modelo
accuracy(md_tslm_fc, teste_cs)

#plota o grafico periodo validacao
plot(md_tslm_fc, xlab="Tempo", ylab="Consumo", xaxt="n" , xlim=c(2018, 2021),  ylim=c(5000, 7000000),bty="l", flty=2)
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(teste_cs, col="red")
lines(md_tslm_fc$fitted, lwd=2, col="blue")


#####
### Calcula modelo Quadratica
#####

md_tsq <- tslm(treinamento_cs ~ trend + I(trend^2))
summary(md_tsq)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(md_tsq, test="LB")

# Calcula modelo período de validacao
md_tsq_fc <- forecast(md_tsq, h = cs_amostra_teste, level=0.95)

#Verifica a acuracia do modelo
accuracy(md_tsq_fc, teste_cs)

#plota o grafico periodo validacao
plot(md_tsq_fc, xlab="Tempo", ylab="Consumo", xaxt="n" , xlim=c(2018, 2021),  ylim=c(5000, 7000000),bty="l", flty=2)
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(teste_cs, col="red")
lines(md_tsq_fc$fitted, lwd=2, col="blue")


#####
### Calcula Media Movel Simples (MA)
#####

md_simples <- rollmean(cs_ts, k=3, align="right") # Media Simples
md_centrada <- ma(cs_ts, order=3)                 # Media Centrada

#plota as medias 
plot(cs_ts, xlab="Tempo", ylab="Consumo", xaxt="n", xlim=c(2018, 2021),  ylim=c(5000, 10000000), bty="l")
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(md_simples, lwd=2, lty=2, col="orange")
lines(md_centrada, lwd=2, col="green")

#Estima modelo de MA em treinamento
md_simples <- rollmean(treinamento_cs, k=3, align="right")

#obtem a média da última janela movel de 2 meses para projeção
ultima_md <- tail(md_simples, 1)

#cria uma projeção que é a repeticao da ultima media da janela para o periodo de validacao
md_simples_fc <- ts(rep(ultima_md, cs_amostra_teste), start=c(2019, 12), end = c(2020, 5), freq=12)

#valida a precisao da estimacao no periodo de treinamento
accuracy(md_simples_fc, teste_cs)

#plota o grafica da projecao
plot(treinamento_cs, xlab="Tempo", ylab="Consumo", xaxt="n", xlim=c(2018, 2021),  ylim=c(5000, 10000000), bty="l")
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(md_simples, lwd=2, lty=2, col="orange")
lines(md_simples_fc, lwd=2, lty=2, col="blue")
lines(teste_cs, col="red")


#####
### Calcula Suavizacao
#####

#estima o modelo de suavizacao na base de treinamento
md_ses <- ets(treinamento_cs, model = "ANN")
summary(md_ses)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(md_ses, test="LB")

#Calcula modelo período de validacao
md_ses_fc <- forecast(md_ses, h=cs_amostra_teste, level=0.95)

#valida precisao
accuracy(md_ses_fc, teste_cs)

#plota o grafica da projecao
plot(md_ses_fc, xlab="Tempo", ylab="Consumo", xaxt="n", xlim=c(2018, 2021),  ylim=c(5000, 10000000), bty="l")
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(md_ses$fitted, lwd=2, col="blue")
lines(teste_cs, col="red")


#####
### Calcula Tendencia Aditiva
#####

md_ses_ta <- ets(treinamento_cs, model = "AAN")
summary(md_ses_ta)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(md_ses_ta, test="LB")

#Calcula modelo período de validacao
md_ses_ta_fc <- forecast(md_ses_ta, h=cs_amostra_teste, level=0.95)

#valida precisao
accuracy(md_ses_ta_fc, teste_cs)

#plota o grafica da projecao
plot(md_ses_ta_fc, xlab="Tempo", ylab="Consumo", xaxt="n", xlim=c(2018, 2021),  ylim=c(5000, 10000000), bty="l")
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(md_ses_ta$fitted, lwd=2, col="blue")
lines(teste_cs, col="red")


#####
### Calcula Suavizacao Tendencia Multiplicativa
#####

md_ses_tm <- ets(treinamento_cs, model = "MMN")
summary(md_ses_tm)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(md_ses_tm, test="LB")

#Calcula modelo período de validacao
md_ses_tm_fc <- forecast(md_ses_tm, h=cs_amostra_teste, level=0.95)

#valida precisao
accuracy(md_ses_tm_fc, teste_cs)

#plota o grafica da projecao
plot(md_ses_tm_fc, xlab="Tempo", ylab="Consumo", xaxt="n", xlim=c(2018, 2021),  ylim=c(5000, 10000000), bty="l")
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(md_ses_tm$fitted, lwd=2, col="blue")
lines(teste_cs, col="red")


#####
### Calcula Suavizacao - Busca melhor Modelo Automatico
#####

md_ses_auto <- ets(treinamento_cs, model = "ZZZ", restrict = FALSE, allow.multiplicative.trend = TRUE)
summary(md_ses_auto)

#verifica os resíduos com teste de Ljung-Box
checkresiduals(md_ses_auto, test="LB")

#Calcula modelo período de validacao
md_ses_auto_fc <- forecast(md_ses_auto, h=cs_amostra_teste, level=0.95)

#verifica precisao
accuracy(md_ses_auto_fc, teste_cs)

#plota o grafica da projecao
plot(md_ses_auto_fc, xlab="Tempo", ylab="Consumo", xaxt="n", xlim=c(2018, 2021),  ylim=c(5000, 10000000), bty="l")
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(md_ses_auto$fitted, lwd=2, col="blue")
lines(teste_cs, col="red")


#####
### Estacionando Serie Temporal
#####

cs_diff <- diff(treinamento_cs, lag=3) # Testei ate ajustar KPSS e ADF
cs_tst_diff <- diff(teste_cs, lag=3) # Testei ate ajustar KPSS e ADF
plot(treinamento_cs, xlab="Tempo", ylab="Consumo", xaxt="n", xlim=c(2018, 2021),  ylim=c(5000, 10000000), bty="l")
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(cs_diff, lwd=2, col="blue")

#checar estacionariedade
checkresiduals(cs_diff)
Box.test(cs_diff, type="Ljung")

#executa o teste de KPSS
summary(ur.kpss(cs_diff))

#executa o teste de ADF
summary(ur.df(cs_diff))


#####
### Calcula ARIMA
#####

#Paramentros Arima
par(mfrow=c(1,2))
Acf(cs_diff)
Pacf(cs_diff)
par(mfrow=c(1,1))

#Modelo Arima
md_arima <- Arima(cs_diff, order = c(1,1,1))
summary(md_arima)

#Calcula modelo período de validacao
md_arima_fc <- forecast(md_arima, h=cs_amostra_teste, level=0.95)

#verifica precisao
accuracy(md_arima_fc, teste_cs)

#plota o grafica da projecao
plot(md_arima_fc, xlab="Tempo", ylab="Consumo", xaxt="n", xlim=c(2018, 2021),  ylim=c(5000, 10000000), bty="l")
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(md_arima_fc$fitted, lwd=2, col="blue")


#####
### Calcula Auto ARIMA
#####

md_at_arima <- auto.arima(cs_diff, seasonal = FALSE, stepwise=FALSE, approximation = FALSE)
summary(md_at_arima)

#Calcula modelo período de validacao
md_at_arima_fc <- forecast(md_at_arima, h=cs_amostra_teste, level=0.95)

#verifica precisao
accuracy(md_at_arima_fc, teste_cs)

#plota o grafica da projecao
plot(md_at_arima_fc, xlab="Tempo", ylab="Consumo", xaxt="n", xlim=c(2018, 2021),  ylim=c(5000, 10000000), bty="l")
axis(1, at=seq(2018, 2021, 1), labels=format(seq(2018, 2021, 1)))
lines(md_at_arima_fc$fitted, lwd=2, col="blue")


#####
### Comparacao
#####

accuracy(md_naive,       teste_cs)  # Naive
accuracy(md_tslm_fc,     teste_cs)  # Linear
accuracy(md_tsq_fc,      teste_cs)  # Quadratica
accuracy(md_simples_fc,  teste_cs)  # MA Test
accuracy(md_ses_fc,      teste_cs)  # Suavizacao Simples
accuracy(md_ses_ta_fc,   teste_cs)  # Suavizacao Tendencia Aditiva
accuracy(md_ses_tm_fc,   teste_cs)  # Suavizacao Tendencia Multiplicativa
accuracy(md_ses_auto_fc, teste_cs)  # Suavizacao Automatico
accuracy(md_arima_fc,    teste_cs)  # Arima 1, 1, 1
accuracy(md_at_arima_fc, teste_cs)  #  Auto Arima


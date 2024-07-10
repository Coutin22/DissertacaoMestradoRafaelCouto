# Diretorio
setwd("~/Rafael_Estudo/PUC/Dissertação/Resultados")

# Pacotes
{library(readxl)
library(moments)
library(xtable)
library(ggplot2)
library(ggfortify)
library(grid)
library(forecast)
library(tseries)
library(corrplot)
library(openxlsx)
library(gridExtra)
library(sf)
library(tidyverse)
library(readxl)
library(ggmap)
library(viridis)
library(ggthemes)
library(Rssa)
library(mgcv)
}



### Modelo PAR(p)
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,-1], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")
detach("package:dplyr", unload = TRUE)

## JANELAS DE PREVISÃO 
{janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_2_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2014,12),freq=12)
  janela_2_valida_Ventos = window(Ventos,start=c(2015,1),end=c(2019,12),freq=12)
  
  janela_3_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2015,12),freq=12)
  janela_3_valida_Ventos = window(Ventos,start=c(2016,1),end=c(2020,12),freq=12)
  
  janela_4_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2016,12),freq=12)
  janela_4_valida_Ventos = window(Ventos,start=c(2017,1),end=c(2021,12),freq=12)
  
  janela_5_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2017,12),freq=12)
  janela_5_valida_Ventos = window(Ventos,start=c(2018,1),end=c(2022,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_2 = apply(janela_2_fit_Ventos,2,max_abs)
  max_Ventos_janela_3 = apply(janela_3_fit_Ventos,2,max_abs)
  max_Ventos_janela_4 = apply(janela_4_fit_Ventos,2,max_abs)
  max_Ventos_janela_5 = apply(janela_5_fit_Ventos,2,max_abs)

  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_2_fit_Ventos = janela_2_fit_Ventos
  norm_janela_3_fit_Ventos = janela_3_fit_Ventos
  norm_janela_4_fit_Ventos = janela_4_fit_Ventos
  norm_janela_5_fit_Ventos = janela_5_fit_Ventos

  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_2_fit_Ventos[,j] = janela_2_fit_Ventos[,j]/max_Ventos_janela_2[j]
    norm_janela_3_fit_Ventos[,j] = janela_3_fit_Ventos[,j]/max_Ventos_janela_3[j]
    norm_janela_4_fit_Ventos[,j] = janela_4_fit_Ventos[,j]/max_Ventos_janela_4[j]
    norm_janela_5_fit_Ventos[,j] = janela_5_fit_Ventos[,j]/max_Ventos_janela_5[j]
  }
}


## Calculando as ordens do modelo
par_ordem = matrix(NA, ncol = 12, nrow = 7)
modelo = 1
par_ordem_janela_2 = par_ordem_janela_3 = par_ordem_janela_4 = par_ordem_janela_5  = par_ordem


# Calculando as ordens do modelo
for(usina in 1:7){
  
  par_ordem_janela_2[usina,] = escolha_ordem(norm_janela_2_fit_Ventos[,usina], lag.max=6)
  par_ordem_janela_3[usina,] = escolha_ordem(norm_janela_3_fit_Ventos[,usina], lag.max=6)
  par_ordem_janela_4[usina,] = escolha_ordem(norm_janela_4_fit_Ventos[,usina], lag.max=6)
  par_ordem_janela_5[usina,] = escolha_ordem(norm_janela_5_fit_Ventos[,usina], lag.max=6)

}


# Calculando o modelo
# Erros
nome = colnames(Ventos)
erros_RMSE = matrix(NA,ncol=5,nrow=7,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_MAE = matrix(NA,ncol=5,nrow=7,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_R2 = matrix(NA,ncol=5,nrow=7,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))


# Modelo
# Caso
# janela_2 = cenarios_par_LogNormal(norm_janela_2_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_2)
# prev_2 = colMeans(janela_2[[1]])*max_Ventos_janela_2[1]
# erros_RMSE[1,1] = RMSE(janela_2_valida_Ventos[,1],prev_2)
# erros_MAE[1,1] = MAE(janela_2_valida_Ventos[,1],prev_2)
# erros_R2[1,1] = R2(janela_2_valida_Ventos[,1],prev_2)


# Completo
janela_2 = cenarios_par_LogNormal(norm_janela_2_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_2)
janela_3 = cenarios_par_LogNormal(norm_janela_3_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_3)
janela_4 = cenarios_par_LogNormal(norm_janela_4_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_4)
janela_5 = cenarios_par_LogNormal(norm_janela_5_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_5)

for(usina in 1:7){
  prev_2 = colMeans(janela_2[[usina]])*max_Ventos_janela_2[usina]
  prev_3 = colMeans(janela_3[[usina]])*max_Ventos_janela_3[usina]
  prev_4 = colMeans(janela_4[[usina]])*max_Ventos_janela_4[usina]
  prev_5 = colMeans(janela_5[[usina]])*max_Ventos_janela_5[usina]

  erros_RMSE[usina,1] = RMSE(janela_2_valida_Ventos[,usina],prev_2)
  erros_RMSE[usina,2] = RMSE(janela_3_valida_Ventos[,usina],prev_3)
  erros_RMSE[usina,3] = RMSE(janela_4_valida_Ventos[,usina],prev_4)
  erros_RMSE[usina,4] = RMSE(janela_5_valida_Ventos[,usina],prev_5)
  erros_RMSE[usina,5] = mean(erros_RMSE[usina,1:4])
  
  erros_MAE[usina,1] = MAE(janela_2_valida_Ventos[,usina],prev_2)
  erros_MAE[usina,2] = MAE(janela_3_valida_Ventos[,usina],prev_3)
  erros_MAE[usina,3] = MAE(janela_4_valida_Ventos[,usina],prev_4)
  erros_MAE[usina,4] = MAE(janela_5_valida_Ventos[,usina],prev_5)
  erros_MAE[usina,5] = mean(erros_MAE[usina,1:4])
  
  erros_R2[usina,1] = R2(janela_2_valida_Ventos[,usina],prev_2)
  erros_R2[usina,2] = R2(janela_3_valida_Ventos[,usina],prev_3)
  erros_R2[usina,3] = R2(janela_4_valida_Ventos[,usina],prev_4)
  erros_R2[usina,4] = R2(janela_5_valida_Ventos[,usina],prev_5)
  erros_R2[usina,5] = mean(erros_R2[usina,1:4])
  
  print(usina)
  
  if(usina == 7){
    erros = cbind(erros_RMSE[,5],erros_MAE[,5],erros_R2[,5])
    colnames(erros) = c("RMSE","MAE","R2")
    write.csv2(erros,paste("ERROS_RESULTADOS//Erros_Modelo_",modelo,".csv",sep=""))
  }
  
}
#






### Modelo PAR-Cov
## Nordeste
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,c(2:5,8)], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")

## JANELAS DE PREVISÃO 
{ janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_2_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2014,12),freq=12)
  janela_2_valida_Ventos = window(Ventos,start=c(2015,1),end=c(2019,12),freq=12)
  
  janela_3_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2015,12),freq=12)
  janela_3_valida_Ventos = window(Ventos,start=c(2016,1),end=c(2020,12),freq=12)
  
  janela_4_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2016,12),freq=12)
  janela_4_valida_Ventos = window(Ventos,start=c(2017,1),end=c(2021,12),freq=12)
  
  janela_5_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2017,12),freq=12)
  janela_5_valida_Ventos = window(Ventos,start=c(2018,1),end=c(2022,12),freq=12)

  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_2 = apply(janela_2_fit_Ventos,2,max_abs)
  max_Ventos_janela_3 = apply(janela_3_fit_Ventos,2,max_abs)
  max_Ventos_janela_4 = apply(janela_4_fit_Ventos,2,max_abs)
  max_Ventos_janela_5 = apply(janela_5_fit_Ventos,2,max_abs)

  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_2_fit_Ventos = janela_2_fit_Ventos
  norm_janela_3_fit_Ventos = janela_3_fit_Ventos
  norm_janela_4_fit_Ventos = janela_4_fit_Ventos
  norm_janela_5_fit_Ventos = janela_5_fit_Ventos

  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_2_fit_Ventos[,j] = janela_2_fit_Ventos[,j]/max_Ventos_janela_2[j]
    norm_janela_3_fit_Ventos[,j] = janela_3_fit_Ventos[,j]/max_Ventos_janela_3[j]
    norm_janela_4_fit_Ventos[,j] = janela_4_fit_Ventos[,j]/max_Ventos_janela_4[j]
    norm_janela_5_fit_Ventos[,j] = janela_5_fit_Ventos[,j]/max_Ventos_janela_5[j]
  }
}



## Calculando as ordens do modelo
par_ordem = matrix(NA, ncol = 12, nrow = 5)
modelo = 16
par_ordem_janela_2 = par_ordem_janela_3 = par_ordem_janela_4 = par_ordem_janela_5 = par_ordem


# Calculando as ordens do modelo
for(usina in 1:5){
  
  par_ordem_janela_2[usina,] = escolha_ordem(norm_janela_2_fit_Ventos[,usina], lag.max=6)
  par_ordem_janela_3[usina,] = escolha_ordem(norm_janela_3_fit_Ventos[,usina], lag.max=6)
  par_ordem_janela_4[usina,] = escolha_ordem(norm_janela_4_fit_Ventos[,usina], lag.max=6)
  par_ordem_janela_5[usina,] = escolha_ordem(norm_janela_5_fit_Ventos[,usina], lag.max=6)

}


# Calculando o modelo
# Erros
nome = colnames(Ventos)
erros_RMSE = matrix(NA,ncol=5,nrow=5,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_MAE = matrix(NA,ncol=5,nrow=5,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_R2 = matrix(NA,ncol=5,nrow=5,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))


# Modelo
# Caso
# janela_2 = cenarios_par_LogNormal_cov(norm_janela_2_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_2)
# prev_2 = colMeans(janela_2[[1]])*max_Ventos_janela_2[1]
# erros_RMSE[1,1] = RMSE(janela_2_valida_Ventos[,1],prev_2)
# erros_MAE[1,1] = MAE(janela_2_valida_Ventos[,1],prev_2)
# erros_R2[1,1] = R2(janela_2_valida_Ventos[,1],prev_2)


# Completo
janela_2 = cenarios_par_LogNormal_cov(norm_janela_2_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_2)
janela_3 = cenarios_par_LogNormal_cov(norm_janela_3_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_3)
janela_4 = cenarios_par_LogNormal_cov(norm_janela_4_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_4)
janela_5 = cenarios_par_LogNormal_cov(norm_janela_5_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_5)

for(usina in 1:5){
  prev_2 = colMeans(janela_2[[usina]])*max_Ventos_janela_2[usina]
  prev_3 = colMeans(janela_3[[usina]])*max_Ventos_janela_3[usina]
  prev_4 = colMeans(janela_4[[usina]])*max_Ventos_janela_4[usina]
  prev_5 = colMeans(janela_5[[usina]])*max_Ventos_janela_5[usina]

  erros_RMSE[usina,1] = RMSE(janela_2_valida_Ventos[,usina],prev_2)
  erros_RMSE[usina,2] = RMSE(janela_3_valida_Ventos[,usina],prev_3)
  erros_RMSE[usina,3] = RMSE(janela_4_valida_Ventos[,usina],prev_4)
  erros_RMSE[usina,4] = RMSE(janela_5_valida_Ventos[,usina],prev_5)
  erros_RMSE[usina,5] = mean(erros_RMSE[usina,1:4])
  
  erros_MAE[usina,1] = MAE(janela_2_valida_Ventos[,usina],prev_2)
  erros_MAE[usina,2] = MAE(janela_3_valida_Ventos[,usina],prev_3)
  erros_MAE[usina,3] = MAE(janela_4_valida_Ventos[,usina],prev_4)
  erros_MAE[usina,4] = MAE(janela_5_valida_Ventos[,usina],prev_5)
  erros_MAE[usina,5] = mean(erros_MAE[usina,1:4])
  
  erros_R2[usina,1] = R2(janela_2_valida_Ventos[,usina],prev_2)
  erros_R2[usina,2] = R2(janela_3_valida_Ventos[,usina],prev_3)
  erros_R2[usina,3] = R2(janela_4_valida_Ventos[,usina],prev_4)
  erros_R2[usina,4] = R2(janela_5_valida_Ventos[,usina],prev_5)
  erros_R2[usina,5] = mean(erros_R2[usina,1:4])
  
  print(usina)
  
  if(usina == 5){
    erros = cbind(erros_RMSE[,5],erros_MAE[,5],erros_R2[,5])
    colnames(erros) = c("RMSE","MAE","R2")
    write.csv2(erros,paste("ERROS_RESULTADOS//Erros_Modelo_",modelo,".csv",sep=""))
  }
  
}
#





## Sul
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,c(6,7)], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")

## JANELAS DE PREVISÃO 
{ janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_2_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2014,12),freq=12)
  janela_2_valida_Ventos = window(Ventos,start=c(2015,1),end=c(2019,12),freq=12)
  
  janela_3_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2015,12),freq=12)
  janela_3_valida_Ventos = window(Ventos,start=c(2016,1),end=c(2020,12),freq=12)
  
  janela_4_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2016,12),freq=12)
  janela_4_valida_Ventos = window(Ventos,start=c(2017,1),end=c(2021,12),freq=12)
  
  janela_5_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2017,12),freq=12)
  janela_5_valida_Ventos = window(Ventos,start=c(2018,1),end=c(2022,12),freq=12)

  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_2 = apply(janela_2_fit_Ventos,2,max_abs)
  max_Ventos_janela_3 = apply(janela_3_fit_Ventos,2,max_abs)
  max_Ventos_janela_4 = apply(janela_4_fit_Ventos,2,max_abs)
  max_Ventos_janela_5 = apply(janela_5_fit_Ventos,2,max_abs)

  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_2_fit_Ventos = janela_2_fit_Ventos
  norm_janela_3_fit_Ventos = janela_3_fit_Ventos
  norm_janela_4_fit_Ventos = janela_4_fit_Ventos
  norm_janela_5_fit_Ventos = janela_5_fit_Ventos

  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_2_fit_Ventos[,j] = janela_2_fit_Ventos[,j]/max_Ventos_janela_2[j]
    norm_janela_3_fit_Ventos[,j] = janela_3_fit_Ventos[,j]/max_Ventos_janela_3[j]
    norm_janela_4_fit_Ventos[,j] = janela_4_fit_Ventos[,j]/max_Ventos_janela_4[j]
    norm_janela_5_fit_Ventos[,j] = janela_5_fit_Ventos[,j]/max_Ventos_janela_5[j]
  }
}



## Calculando as ordens do modelo
par_ordem = matrix(NA, ncol = 12, nrow = 2)
modelo = 17
par_ordem_janela_2 = par_ordem_janela_3 = par_ordem_janela_4 = par_ordem_janela_5 = par_ordem


# Calculando as ordens do modelo
for(usina in 1:2){
  
  par_ordem_janela_2[usina,] = escolha_ordem(norm_janela_2_fit_Ventos[,usina], lag.max=6)
  par_ordem_janela_3[usina,] = escolha_ordem(norm_janela_3_fit_Ventos[,usina], lag.max=6)
  par_ordem_janela_4[usina,] = escolha_ordem(norm_janela_4_fit_Ventos[,usina], lag.max=6)
  par_ordem_janela_5[usina,] = escolha_ordem(norm_janela_5_fit_Ventos[,usina], lag.max=6)

}


# Calculando o modelo
# Erros
nome = colnames(Ventos)
erros_RMSE = matrix(NA,ncol=5,nrow=2,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_MAE = matrix(NA,ncol=5,nrow=2,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_R2 = matrix(NA,ncol=5,nrow=2,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))


# Modelo
# Caso
# janela_2 = cenarios_par_LogNormal_cov(norm_janela_2_fit_Ventos[,1], num_periodos=60, num_cenarios=2000, par_ordem_janela_2[1,], janela_2_matriz_dec, nEstados=2)
# prev_2 = colMeans(janela_2$Cenarios)*max_Ventos_janela_2[1]
# erros_RMSE[1,1] = RMSE(janela_2_valida_Ventos[,1],prev_2)
# erros_MAE[1,1] = MAE(janela_2_valida_Ventos[,1],prev_2)
# erros_R2[1,1] = R2(janela_2_valida_Ventos[,1],prev_2)


# Completo
janela_2 = cenarios_par_LogNormal_cov(norm_janela_2_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_2)
janela_3 = cenarios_par_LogNormal_cov(norm_janela_3_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_3)
janela_4 = cenarios_par_LogNormal_cov(norm_janela_4_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_4)
janela_5 = cenarios_par_LogNormal_cov(norm_janela_5_fit_Ventos, num_periodos=60, num_cenarios=2000, par_ordem_janela_5)

for(usina in 1:2){
  prev_2 = colMeans(janela_2[[usina]])*max_Ventos_janela_2[usina]
  prev_3 = colMeans(janela_3[[usina]])*max_Ventos_janela_3[usina]
  prev_4 = colMeans(janela_4[[usina]])*max_Ventos_janela_4[usina]
  prev_5 = colMeans(janela_5[[usina]])*max_Ventos_janela_5[usina]

  erros_RMSE[usina,1] = RMSE(janela_2_valida_Ventos[,usina],prev_2)
  erros_RMSE[usina,2] = RMSE(janela_3_valida_Ventos[,usina],prev_3)
  erros_RMSE[usina,3] = RMSE(janela_4_valida_Ventos[,usina],prev_4)
  erros_RMSE[usina,4] = RMSE(janela_5_valida_Ventos[,usina],prev_5)
  erros_RMSE[usina,5] = mean(erros_RMSE[usina,1:4])
  
  erros_MAE[usina,1] = MAE(janela_2_valida_Ventos[,usina],prev_2)
  erros_MAE[usina,2] = MAE(janela_3_valida_Ventos[,usina],prev_3)
  erros_MAE[usina,3] = MAE(janela_4_valida_Ventos[,usina],prev_4)
  erros_MAE[usina,4] = MAE(janela_5_valida_Ventos[,usina],prev_5)
  erros_MAE[usina,5] = mean(erros_MAE[usina,1:4])
  
  erros_R2[usina,1] = R2(janela_2_valida_Ventos[,usina],prev_2)
  erros_R2[usina,2] = R2(janela_3_valida_Ventos[,usina],prev_3)
  erros_R2[usina,3] = R2(janela_4_valida_Ventos[,usina],prev_4)
  erros_R2[usina,4] = R2(janela_5_valida_Ventos[,usina],prev_5)
  erros_R2[usina,5] = mean(erros_R2[usina,1:4])
  
  print(usina)
  
  if(usina == 2){
    erros = cbind(erros_RMSE[,5],erros_MAE[,5],erros_R2[,5])
    colnames(erros) = c("RMSE","MAE","R2")
    write.csv2(erros,paste("ERROS_RESULTADOS//Erros_Modelo_",modelo,".csv",sep=""))
  }
  
}
#











### Modelo PAR(X) + ENSO 
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,-1], start=1980, freq=12)
ENSO <- ts(read_excel("ENSO_Completo.xlsx")[c(589:1116),-1], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")

## JANELAS DE PREVISÃO VENTO
{janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_2_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2014,12),freq=12)
  janela_2_valida_Ventos = window(Ventos,start=c(2015,1),end=c(2019,12),freq=12)
  
  janela_3_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2015,12),freq=12)
  janela_3_valida_Ventos = window(Ventos,start=c(2016,1),end=c(2020,12),freq=12)
  
  janela_4_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2016,12),freq=12)
  janela_4_valida_Ventos = window(Ventos,start=c(2017,1),end=c(2021,12),freq=12)
  
  janela_5_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2017,12),freq=12)
  janela_5_valida_Ventos = window(Ventos,start=c(2018,1),end=c(2022,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_2 = apply(janela_2_fit_Ventos,2,max_abs)
  max_Ventos_janela_3 = apply(janela_3_fit_Ventos,2,max_abs)
  max_Ventos_janela_4 = apply(janela_4_fit_Ventos,2,max_abs)
  max_Ventos_janela_5 = apply(janela_5_fit_Ventos,2,max_abs)

  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_2_fit_Ventos = janela_2_fit_Ventos
  norm_janela_3_fit_Ventos = janela_3_fit_Ventos
  norm_janela_4_fit_Ventos = janela_4_fit_Ventos
  norm_janela_5_fit_Ventos = janela_5_fit_Ventos

  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_2_fit_Ventos[,j] = janela_2_fit_Ventos[,j]/max_Ventos_janela_2[j]
    norm_janela_3_fit_Ventos[,j] = janela_3_fit_Ventos[,j]/max_Ventos_janela_3[j]
    norm_janela_4_fit_Ventos[,j] = janela_4_fit_Ventos[,j]/max_Ventos_janela_4[j]
    norm_janela_5_fit_Ventos[,j] = janela_5_fit_Ventos[,j]/max_Ventos_janela_5[j]
  }
}


## JANELAS DE PREVISÃO ENSO
{janela_1_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_ENSO = window(ENSO,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_2_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2014,12),freq=12)
  janela_2_valida_ENSO = window(ENSO,start=c(2015,1),end=c(2019,12),freq=12)
  
  janela_3_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2015,12),freq=12)
  janela_3_valida_ENSO = window(ENSO,start=c(2016,1),end=c(2020,12),freq=12)
  
  janela_4_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2016,12),freq=12)
  janela_4_valida_ENSO = window(ENSO,start=c(2017,1),end=c(2021,12),freq=12)
  
  janela_5_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2017,12),freq=12)
  janela_5_valida_ENSO = window(ENSO,start=c(2018,1),end=c(2022,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_ENSO_janela_1 = apply(janela_1_fit_ENSO,2,max_abs)
  max_ENSO_janela_2 = apply(janela_2_fit_ENSO,2,max_abs)
  max_ENSO_janela_3 = apply(janela_3_fit_ENSO,2,max_abs)
  max_ENSO_janela_4 = apply(janela_4_fit_ENSO,2,max_abs)
  max_ENSO_janela_5 = apply(janela_5_fit_ENSO,2,max_abs)

  norm_janela_1_fit_ENSO = janela_1_fit_ENSO
  norm_janela_2_fit_ENSO = janela_2_fit_ENSO
  norm_janela_3_fit_ENSO = janela_3_fit_ENSO
  norm_janela_4_fit_ENSO = janela_4_fit_ENSO
  norm_janela_5_fit_ENSO = janela_5_fit_ENSO

  norm_janela_1_valida_ENSO = janela_1_valida_ENSO
  norm_janela_2_valida_ENSO = janela_2_valida_ENSO
  norm_janela_3_valida_ENSO = janela_3_valida_ENSO
  norm_janela_4_valida_ENSO = janela_4_valida_ENSO
  norm_janela_5_valida_ENSO = janela_5_valida_ENSO

  for(j in 1:ncol(janela_1_fit_ENSO)){
    norm_janela_1_fit_ENSO[,j] = janela_1_fit_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_2_fit_ENSO[,j] = janela_2_fit_ENSO[,j]/max_ENSO_janela_2[j]
    norm_janela_3_fit_ENSO[,j] = janela_3_fit_ENSO[,j]/max_ENSO_janela_3[j]
    norm_janela_4_fit_ENSO[,j] = janela_4_fit_ENSO[,j]/max_ENSO_janela_4[j]
    norm_janela_5_fit_ENSO[,j] = janela_5_fit_ENSO[,j]/max_ENSO_janela_5[j]

    norm_janela_1_valida_ENSO[,j] = janela_1_valida_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_2_valida_ENSO[,j] = janela_2_valida_ENSO[,j]/max_ENSO_janela_2[j]
    norm_janela_3_valida_ENSO[,j] = janela_3_valida_ENSO[,j]/max_ENSO_janela_3[j]
    norm_janela_4_valida_ENSO[,j] = janela_4_valida_ENSO[,j]/max_ENSO_janela_4[j]
    norm_janela_5_valida_ENSO[,j] = janela_5_valida_ENSO[,j]/max_ENSO_janela_5[j]
  }
}



# Calculando o modelo
# Erros
nome = colnames(Ventos)
erros_RMSE = matrix(NA,ncol=5,nrow=7,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_MAE = matrix(NA,ncol=5,nrow=7,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_R2 = matrix(NA,ncol=5,nrow=7,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))

modelo = 100
MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=7)


# Modelo
# Caso
# Procurando melhor p e m para janela 1
dados1 = norm_janela_1_fit_Ventos
dados2 = window(norm_janela_1_fit_ENSO[,1],start=1980)
dados1_valida = janela_1_valida_Ventos
dados2_valida = norm_janela_1_valida_ENSO[,1]
max_dados1 = max_Ventos_janela_1

aux = PARX_melhor_p_m(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=2)
MAE_p_m_janela_1 = aux[[1]]

write.csv2(MAE_p_m_janela_1,paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))

# Buscando arquivo de erros para janela 1
aux = read.csv2(paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))[,-1]

# Calculando métricas para cada janela
dados1_janela_2 = norm_janela_2_fit_Ventos
dados2_janela_2 = window(norm_janela_2_fit_ENSO[,1],start=1980)
dados1_novo_janela_2 = window(dados1_janela_2,start=start(dados2_janela_2))
dados1_valida_janela_2 = janela_2_valida_Ventos
dados2_valida_janela_2 = norm_janela_2_valida_ENSO[,1]
janela_2 = cenarios_parX_LogNormal(dados1_novo_janela_2,dados2_janela_2,dados2_valida_janela_2,max_p=aux[,1],max_m=aux[,2],num_cenarios=20)
# prev_2 = colMeans(janela_2[[1]])*max_Ventos_janela_2[1]
# erros_RMSE[1,1] = RMSE(janela_2_valida_Ventos[,1],prev_2)
# erros_MAE[1,1] = MAE(janela_2_valida_Ventos[,1],prev_2)
# erros_R2[1,1] = R2(janela_2_valida_Ventos[,1],prev_2)




# Rascunho
# Cenários
library(reshape2)
library(stringr)

# Funções de tratamento dos cenários
cenarios_tratamento_aux <- function(dados,n_sim) {
  
  dados = janela_2[[1]]

  cenarios <- data.frame(dados)[,c(1:12)]

  cenarios <- data.frame(t(cenarios))
  
  cenarios <- melt(cenarios, measure.vars = names(cenarios))
  cenarios$variable <- as.factor(str_sub(cenarios$variable,start = 9))
  
  cenarios$mes <- rep(1:12,n_sim)
  cenarios <- cenarios[,c(1,3)]

  return(cenarios)
  
}


cenarios_tratamento_final <- function(dados1,dados2) {
  
#  dados1 = janela_2[[1]]
#  dados2 = max_Ventos_janela_2[1]
  
  cenarios <- data.frame(dados1)[,c(1:12)]
  cenarios <- cenarios*dados2
  
  cenarios <- data.frame(t(cenarios))
  
  cenarios <- melt(cenarios, measure.vars = names(cenarios))
  
  cenarios <- cenarios[,2]

  return(cenarios)
  
}


# Rodando
n_sim = 20
PARX_cenarios <- matrix(NA, ncol = 9, nrow = 12*n_sim)
colnames(PARX_cenarios) <- c(colnames(Ventos),"Cenário","Mês")


# Fixo
cenarios_aux <- cenarios_tratamento_aux(janela_2[[1]],n_sim)
PARX_cenarios[,8] <- sort(cenarios_aux[,1])
PARX_cenarios[,9] <- cenarios_aux[,2]

# For
for(usina in 1:7){
cenarios_final <- cenarios_tratamento_final(janela_2[[usina]],max_Ventos_janela_2[usina])
PARX_cenarios[,usina] <- cenarios_final
}



# Previsão
previsao <- data.frame(colMeans(janela_2[[1]])*max_Ventos_janela_2[1])
previsao <- data.frame(previsao[c(1:12),])
previsao$mes <- rep(1:12,1)
previsao_final <- previsao[,c(2,1)]
colnames(previsao_final) <- c("Mês","Vento_Média")



# Gráfico
ggplot() + 
  
  geom_line(data = cenarios_final, aes(x = Mês, y = Vento, group = as.factor(Cenário)), color = "lightskyblue") +
  geom_line(data = previsao_final, aes(x = Mês, y = Vento_Média), color = "forestgreen") +
  
  scale_x_continuous(breaks = 1:12, labels = c("","Fev/24","","","Maio/24","","","Ago/24","","","Nov/24","")) +

  ggtitle("Alagoas - PARX + ONI ACUM") +
  ylab("Velocidade do Vento (m/s)") + xlab("") +

  theme(axis.text = element_text(size=10),
        axis.title = element_text(size=10),
        legend.text = element_text(size=8),
        plot.title = element_text(hjust=0.5,size=10),
        legend.position = "none",
        plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
  theme_classic() 
  



# Histórico
# cenarios_rascunho_3$mes <- rep(4:15,50)
# previsao$mes <- rep(4:15,1)
# historico <- data.frame(janela_2_valida_Ventos[,1])
# historico <- data.frame(historico[c(58:60),])
# historico$mes <- rep(1:3,1)
# historico_final <- historico[,c(2,1)]
# colnames(historico_final) <- c("Mês","Vento_Histórico")
# geom_line(data = historico_final, aes(x = Mês, y = Vento_Histórico), color = "black") +
#












# Completo
for(ENSO in 1:14){
  
  MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=7)
  
  #Procurando melhor p e m para janela 1

    dados1 = norm_janela_1_fit_Ventos
    dados2 = window(norm_janela_1_fit_ENSO[,ENSO],start=1980)
    dados1_valida = janela_1_valida_Ventos
    dados2_valida = norm_janela_1_valida_ENSO[,ENSO]
    max_dados1 = max_Ventos_janela_1
    
    aux = PARX_melhor_p_m(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=500)
    MAE_p_m_janela_1 = aux[[1]]
    
    write.csv2(MAE_p_m_janela_1,paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))
    
  #Buscando arquivo de erros para janela 1
  aux = read.csv2(paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))[,-1]
  
  
  #Calculando métricas para cada janela
  
    dados1_janela_2 = norm_janela_2_fit_Ventos
    dados1_janela_3 = norm_janela_3_fit_Ventos
    dados1_janela_4 = norm_janela_4_fit_Ventos
    dados1_janela_5 = norm_janela_5_fit_Ventos

    dados2_janela_2 = window(norm_janela_2_fit_ENSO[,ENSO],start=1980)
    dados2_janela_3 = window(norm_janela_3_fit_ENSO[,ENSO],start=1980)
    dados2_janela_4 = window(norm_janela_4_fit_ENSO[,ENSO],start=1980)
    dados2_janela_5 = window(norm_janela_5_fit_ENSO[,ENSO],start=1980)

    dados1_novo_janela_2 = window(dados1_janela_2,start=start(dados2_janela_2))
    dados1_novo_janela_3 = window(dados1_janela_3,start=start(dados2_janela_3))
    dados1_novo_janela_4 = window(dados1_janela_4,start=start(dados2_janela_4))
    dados1_novo_janela_5 = window(dados1_janela_5,start=start(dados2_janela_5))

    dados1_valida_janela_2 = janela_2_valida_Ventos
    dados1_valida_janela_3 = janela_3_valida_Ventos
    dados1_valida_janela_4 = janela_4_valida_Ventos
    dados1_valida_janela_5 = janela_5_valida_Ventos

    dados2_valida_janela_2 = norm_janela_2_valida_ENSO[,ENSO]
    dados2_valida_janela_3 = norm_janela_3_valida_ENSO[,ENSO]
    dados2_valida_janela_4 = norm_janela_4_valida_ENSO[,ENSO]
    dados2_valida_janela_5 = norm_janela_5_valida_ENSO[,ENSO]

    janela_2 = cenarios_parX_LogNormal(dados1_novo_janela_2,dados2_janela_2,dados2_valida_janela_2,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
    janela_3 = cenarios_parX_LogNormal(dados1_novo_janela_3,dados2_janela_3,dados2_valida_janela_3,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
    janela_4 = cenarios_parX_LogNormal(dados1_novo_janela_4,dados2_janela_4,dados2_valida_janela_4,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
    janela_5 = cenarios_parX_LogNormal(dados1_novo_janela_5,dados2_janela_5,dados2_valida_janela_5,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)

    for(usina in 1:7){
      
    prev_2 = colMeans(janela_2[[usina]])*max_Ventos_janela_2[usina]
    prev_3 = colMeans(janela_3[[usina]])*max_Ventos_janela_3[usina]
    prev_4 = colMeans(janela_4[[usina]])*max_Ventos_janela_4[usina]
    prev_5 = colMeans(janela_5[[usina]])*max_Ventos_janela_5[usina]

    erros_RMSE[usina,1] = RMSE(janela_2_valida_Ventos[,usina],prev_2)
    erros_RMSE[usina,2] = RMSE(janela_3_valida_Ventos[,usina],prev_3)
    erros_RMSE[usina,3] = RMSE(janela_4_valida_Ventos[,usina],prev_4)
    erros_RMSE[usina,4] = RMSE(janela_5_valida_Ventos[,usina],prev_5)
    erros_RMSE[usina,5] = mean(erros_RMSE[usina,1:4])
    
    erros_MAE[usina,1] = MAE(janela_2_valida_Ventos[,usina],prev_2)
    erros_MAE[usina,2] = MAE(janela_3_valida_Ventos[,usina],prev_3)
    erros_MAE[usina,3] = MAE(janela_4_valida_Ventos[,usina],prev_4)
    erros_MAE[usina,4] = MAE(janela_5_valida_Ventos[,usina],prev_5)
    erros_MAE[usina,5] = mean(erros_MAE[usina,1:4])
    
    erros_R2[usina,1] = R2(janela_2_valida_Ventos[,usina],prev_2)
    erros_R2[usina,2] = R2(janela_3_valida_Ventos[,usina],prev_3)
    erros_R2[usina,3] = R2(janela_4_valida_Ventos[,usina],prev_4)
    erros_R2[usina,4] = R2(janela_5_valida_Ventos[,usina],prev_5)
    erros_R2[usina,5] = mean(erros_R2[usina,1:4])
    
   
    print(usina)
    
    if(usina == 7){
      erros = cbind(erros_RMSE[,5],erros_MAE[,5],erros_R2[,5])
      colnames(erros) = c("RMSE","MAE","R2")
      write.csv2(erros,paste("ERROS_RESULTADOS//Erros_Modelo_",modelo,".csv",sep=""))
    }
    
  }
  
  modelo = modelo + 1
  
}
#










### Modelo PAR(X) + ENSO - Considerando a Covariância entre os Dados
## Nordeste
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,c(2:5,8)], start=1980, freq=12)
ENSO <- ts(read_excel("ENSO_Completo.xlsx")[c(589:1116),-1], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")


## JANELAS DE PREVISÃO VENTO
{janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_2_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2014,12),freq=12)
  janela_2_valida_Ventos = window(Ventos,start=c(2015,1),end=c(2019,12),freq=12)
  
  janela_3_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2015,12),freq=12)
  janela_3_valida_Ventos = window(Ventos,start=c(2016,1),end=c(2020,12),freq=12)
  
  janela_4_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2016,12),freq=12)
  janela_4_valida_Ventos = window(Ventos,start=c(2017,1),end=c(2021,12),freq=12)
  
  janela_5_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2017,12),freq=12)
  janela_5_valida_Ventos = window(Ventos,start=c(2018,1),end=c(2022,12),freq=12)

  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_2 = apply(janela_2_fit_Ventos,2,max_abs)
  max_Ventos_janela_3 = apply(janela_3_fit_Ventos,2,max_abs)
  max_Ventos_janela_4 = apply(janela_4_fit_Ventos,2,max_abs)
  max_Ventos_janela_5 = apply(janela_5_fit_Ventos,2,max_abs)

  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_2_fit_Ventos = janela_2_fit_Ventos
  norm_janela_3_fit_Ventos = janela_3_fit_Ventos
  norm_janela_4_fit_Ventos = janela_4_fit_Ventos
  norm_janela_5_fit_Ventos = janela_5_fit_Ventos

  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_2_fit_Ventos[,j] = janela_2_fit_Ventos[,j]/max_Ventos_janela_2[j]
    norm_janela_3_fit_Ventos[,j] = janela_3_fit_Ventos[,j]/max_Ventos_janela_3[j]
    norm_janela_4_fit_Ventos[,j] = janela_4_fit_Ventos[,j]/max_Ventos_janela_4[j]
    norm_janela_5_fit_Ventos[,j] = janela_5_fit_Ventos[,j]/max_Ventos_janela_5[j]
  }
}


## JANELAS DE PREVISÃO ENSO
{janela_1_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_ENSO = window(ENSO,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_2_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2014,12),freq=12)
  janela_2_valida_ENSO = window(ENSO,start=c(2015,1),end=c(2019,12),freq=12)
  
  janela_3_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2015,12),freq=12)
  janela_3_valida_ENSO = window(ENSO,start=c(2016,1),end=c(2020,12),freq=12)
  
  janela_4_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2016,12),freq=12)
  janela_4_valida_ENSO = window(ENSO,start=c(2017,1),end=c(2021,12),freq=12)
  
  janela_5_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2017,12),freq=12)
  janela_5_valida_ENSO = window(ENSO,start=c(2018,1),end=c(2022,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_ENSO_janela_1 = apply(janela_1_fit_ENSO,2,max_abs)
  max_ENSO_janela_2 = apply(janela_2_fit_ENSO,2,max_abs)
  max_ENSO_janela_3 = apply(janela_3_fit_ENSO,2,max_abs)
  max_ENSO_janela_4 = apply(janela_4_fit_ENSO,2,max_abs)
  max_ENSO_janela_5 = apply(janela_5_fit_ENSO,2,max_abs)

  norm_janela_1_fit_ENSO = janela_1_fit_ENSO
  norm_janela_2_fit_ENSO = janela_2_fit_ENSO
  norm_janela_3_fit_ENSO = janela_3_fit_ENSO
  norm_janela_4_fit_ENSO = janela_4_fit_ENSO
  norm_janela_5_fit_ENSO = janela_5_fit_ENSO

  norm_janela_1_valida_ENSO = janela_1_valida_ENSO
  norm_janela_2_valida_ENSO = janela_2_valida_ENSO
  norm_janela_3_valida_ENSO = janela_3_valida_ENSO
  norm_janela_4_valida_ENSO = janela_4_valida_ENSO
  norm_janela_5_valida_ENSO = janela_5_valida_ENSO

  for(j in 1:ncol(janela_1_fit_ENSO)){
    norm_janela_1_fit_ENSO[,j] = janela_1_fit_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_2_fit_ENSO[,j] = janela_2_fit_ENSO[,j]/max_ENSO_janela_2[j]
    norm_janela_3_fit_ENSO[,j] = janela_3_fit_ENSO[,j]/max_ENSO_janela_3[j]
    norm_janela_4_fit_ENSO[,j] = janela_4_fit_ENSO[,j]/max_ENSO_janela_4[j]
    norm_janela_5_fit_ENSO[,j] = janela_5_fit_ENSO[,j]/max_ENSO_janela_5[j]

    norm_janela_1_valida_ENSO[,j] = janela_1_valida_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_2_valida_ENSO[,j] = janela_2_valida_ENSO[,j]/max_ENSO_janela_2[j]
    norm_janela_3_valida_ENSO[,j] = janela_3_valida_ENSO[,j]/max_ENSO_janela_3[j]
    norm_janela_4_valida_ENSO[,j] = janela_4_valida_ENSO[,j]/max_ENSO_janela_4[j]
    norm_janela_5_valida_ENSO[,j] = janela_5_valida_ENSO[,j]/max_ENSO_janela_5[j]
  }
}





# Calculando o modelo
# Erros
nome = colnames(Ventos)
erros_RMSE = matrix(NA,ncol=5,nrow=5,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_MAE = matrix(NA,ncol=5,nrow=5,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_R2 = matrix(NA,ncol=5,nrow=5,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))

modelo = 18

# # Modelo
# # Caso
# # Procurando melhor p e m para janela 1
# dados1 = norm_janela_1_fit_Ventos
# dados2 = window(norm_janela_1_fit_ENSO[,1],start=1980)
# dados1_valida = janela_1_valida_Ventos
# dados2_valida = norm_janela_1_valida_ENSO[,1]
# max_dados1 = max_Ventos_janela_1
# 
# aux = PARX_melhor_p_m_cov(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=5)
# MAE_p_m_janela_1 = aux[[1]]
# 
# write.csv2(MAE_p_m_janela_1,paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))
# 
# # Buscando arquivo de erros para janela 1
# aux = read.csv2(paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))[,-1]
# 
# # Calculando métricas para cada janela
# dados1_janela_2 = norm_janela_2_fit_Ventos
# dados2_janela_2 = window(norm_janela_2_fit_ENSO[,1],start=1980)
# dados1_novo_janela_2 = window(dados1_janela_2,start=start(dados2_janela_2))
# dados1_valida_janela_2 = janela_2_valida_Ventos
# dados2_valida_janela_2 = norm_janela_2_valida_ENSO[,1]
# janela_2 = cenarios_parX_LogNormal(dados1_novo_janela_2,dados2_janela_2,dados2_valida_janela_2,max_p=aux[,1],max_m=aux[,2],num_cenarios=20)
# 
# prev_2 = colMeans(janela_2[[1]])*max_Ventos_janela_2[1]
# erros_RMSE[1,1] = RMSE(janela_2_valida_Ventos[,1],prev_2)
# erros_MAE[1,1] = MAE(janela_2_valida_Ventos[,1],prev_2)
# erros_R2[1,1] = R2(janela_2_valida_Ventos[,1],prev_2)





# Completo
for(ENSO in 1:14){
  
  MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=5)
  
  #Procurando melhor p e m para janela 1

    dados1 = norm_janela_1_fit_Ventos
    dados2 = window(norm_janela_1_fit_ENSO[,ENSO],start=1980)
    dados1_valida = janela_1_valida_Ventos
    dados2_valida = norm_janela_1_valida_ENSO[,ENSO]
    max_dados1 = max_Ventos_janela_1
    
    aux = PARX_melhor_p_m_cov(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=500)
    MAE_p_m_janela_1 = aux[[1]]
    
    write.csv2(MAE_p_m_janela_1,paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))
    
  #Buscando arquivo de erros para janela 1
  aux = read.csv2(paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))[,-1]
  
  
  #Calculando métricas para cada janela
  
  dados1_janela_2 = norm_janela_2_fit_Ventos
  dados1_janela_3 = norm_janela_3_fit_Ventos
  dados1_janela_4 = norm_janela_4_fit_Ventos
  dados1_janela_5 = norm_janela_5_fit_Ventos

  dados2_janela_2 = window(norm_janela_2_fit_ENSO[,ENSO],start=1980)
  dados2_janela_3 = window(norm_janela_3_fit_ENSO[,ENSO],start=1980)
  dados2_janela_4 = window(norm_janela_4_fit_ENSO[,ENSO],start=1980)
  dados2_janela_5 = window(norm_janela_5_fit_ENSO[,ENSO],start=1980)

  dados1_novo_janela_2 = window(dados1_janela_2,start=start(dados2_janela_2))
  dados1_novo_janela_3 = window(dados1_janela_3,start=start(dados2_janela_3))
  dados1_novo_janela_4 = window(dados1_janela_4,start=start(dados2_janela_4))
  dados1_novo_janela_5 = window(dados1_janela_5,start=start(dados2_janela_5))

  dados1_valida_janela_2 = janela_2_valida_Ventos
  dados1_valida_janela_3 = janela_3_valida_Ventos
  dados1_valida_janela_4 = janela_4_valida_Ventos
  dados1_valida_janela_5 = janela_5_valida_Ventos

  dados2_valida_janela_2 = norm_janela_2_valida_ENSO[,ENSO]
  dados2_valida_janela_3 = norm_janela_3_valida_ENSO[,ENSO]
  dados2_valida_janela_4 = norm_janela_4_valida_ENSO[,ENSO]
  dados2_valida_janela_5 = norm_janela_5_valida_ENSO[,ENSO]

  janela_2 = cenarios_parX_LogNormal_cov(dados1_novo_janela_2,dados2_janela_2,dados2_valida_janela_2,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
  janela_3 = cenarios_parX_LogNormal_cov(dados1_novo_janela_3,dados2_janela_3,dados2_valida_janela_3,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
  janela_4 = cenarios_parX_LogNormal_cov(dados1_novo_janela_4,dados2_janela_4,dados2_valida_janela_4,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
  janela_5 = cenarios_parX_LogNormal_cov(dados1_novo_janela_5,dados2_janela_5,dados2_valida_janela_5,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)

  
  for(usina in 1:5){
    
    prev_2 = colMeans(janela_2[[usina]])*max_Ventos_janela_2[usina]
    prev_3 = colMeans(janela_3[[usina]])*max_Ventos_janela_3[usina]
    prev_4 = colMeans(janela_4[[usina]])*max_Ventos_janela_4[usina]
    prev_5 = colMeans(janela_5[[usina]])*max_Ventos_janela_5[usina]

    erros_RMSE[usina,1] = RMSE(janela_2_valida_Ventos[,usina],prev_2)
    erros_RMSE[usina,2] = RMSE(janela_3_valida_Ventos[,usina],prev_3)
    erros_RMSE[usina,3] = RMSE(janela_4_valida_Ventos[,usina],prev_4)
    erros_RMSE[usina,4] = RMSE(janela_5_valida_Ventos[,usina],prev_5)
    erros_RMSE[usina,5] = mean(erros_RMSE[usina,1:4])
    
    erros_MAE[usina,1] = MAE(janela_2_valida_Ventos[,usina],prev_2)
    erros_MAE[usina,2] = MAE(janela_3_valida_Ventos[,usina],prev_3)
    erros_MAE[usina,3] = MAE(janela_4_valida_Ventos[,usina],prev_4)
    erros_MAE[usina,4] = MAE(janela_5_valida_Ventos[,usina],prev_5)
    erros_MAE[usina,5] = mean(erros_MAE[usina,1:4])
    
    erros_R2[usina,1] = R2(janela_2_valida_Ventos[,usina],prev_2)
    erros_R2[usina,2] = R2(janela_3_valida_Ventos[,usina],prev_3)
    erros_R2[usina,3] = R2(janela_4_valida_Ventos[,usina],prev_4)
    erros_R2[usina,4] = R2(janela_5_valida_Ventos[,usina],prev_5)
    erros_R2[usina,5] = mean(erros_R2[usina,1:4])
    
    
    print(usina)
    
    if(usina == 5){
      erros = cbind(erros_RMSE[,5],erros_MAE[,5],erros_R2[,5])
      colnames(erros) = c("RMSE","MAE","R2")
      write.csv2(erros,paste("ERROS_RESULTADOS//Erros_Modelo_",modelo,".csv",sep=""))
    }
    
  }
  
  modelo = modelo + 1
  
}
#








## Sul
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,c(6,7)], start=1980, freq=12)
ENSO <- ts(read_excel("ENSO_Completo.xlsx")[c(589:1116),-1], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")


## JANELAS DE PREVISÃO VENTO
{janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_2_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2014,12),freq=12)
  janela_2_valida_Ventos = window(Ventos,start=c(2015,1),end=c(2019,12),freq=12)
  
  janela_3_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2015,12),freq=12)
  janela_3_valida_Ventos = window(Ventos,start=c(2016,1),end=c(2020,12),freq=12)
  
  janela_4_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2016,12),freq=12)
  janela_4_valida_Ventos = window(Ventos,start=c(2017,1),end=c(2021,12),freq=12)
  
  janela_5_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2017,12),freq=12)
  janela_5_valida_Ventos = window(Ventos,start=c(2018,1),end=c(2022,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_2 = apply(janela_2_fit_Ventos,2,max_abs)
  max_Ventos_janela_3 = apply(janela_3_fit_Ventos,2,max_abs)
  max_Ventos_janela_4 = apply(janela_4_fit_Ventos,2,max_abs)
  max_Ventos_janela_5 = apply(janela_5_fit_Ventos,2,max_abs)

  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_2_fit_Ventos = janela_2_fit_Ventos
  norm_janela_3_fit_Ventos = janela_3_fit_Ventos
  norm_janela_4_fit_Ventos = janela_4_fit_Ventos
  norm_janela_5_fit_Ventos = janela_5_fit_Ventos

  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_2_fit_Ventos[,j] = janela_2_fit_Ventos[,j]/max_Ventos_janela_2[j]
    norm_janela_3_fit_Ventos[,j] = janela_3_fit_Ventos[,j]/max_Ventos_janela_3[j]
    norm_janela_4_fit_Ventos[,j] = janela_4_fit_Ventos[,j]/max_Ventos_janela_4[j]
    norm_janela_5_fit_Ventos[,j] = janela_5_fit_Ventos[,j]/max_Ventos_janela_5[j]
  }
}


## JANELAS DE PREVISÃO ENSO
{janela_1_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_ENSO = window(ENSO,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_2_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2014,12),freq=12)
  janela_2_valida_ENSO = window(ENSO,start=c(2015,1),end=c(2019,12),freq=12)
  
  janela_3_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2015,12),freq=12)
  janela_3_valida_ENSO = window(ENSO,start=c(2016,1),end=c(2020,12),freq=12)
  
  janela_4_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2016,12),freq=12)
  janela_4_valida_ENSO = window(ENSO,start=c(2017,1),end=c(2021,12),freq=12)
  
  janela_5_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2017,12),freq=12)
  janela_5_valida_ENSO = window(ENSO,start=c(2018,1),end=c(2022,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_ENSO_janela_1 = apply(janela_1_fit_ENSO,2,max_abs)
  max_ENSO_janela_2 = apply(janela_2_fit_ENSO,2,max_abs)
  max_ENSO_janela_3 = apply(janela_3_fit_ENSO,2,max_abs)
  max_ENSO_janela_4 = apply(janela_4_fit_ENSO,2,max_abs)
  max_ENSO_janela_5 = apply(janela_5_fit_ENSO,2,max_abs)

  norm_janela_1_fit_ENSO = janela_1_fit_ENSO
  norm_janela_2_fit_ENSO = janela_2_fit_ENSO
  norm_janela_3_fit_ENSO = janela_3_fit_ENSO
  norm_janela_4_fit_ENSO = janela_4_fit_ENSO
  norm_janela_5_fit_ENSO = janela_5_fit_ENSO

  norm_janela_1_valida_ENSO = janela_1_valida_ENSO
  norm_janela_2_valida_ENSO = janela_2_valida_ENSO
  norm_janela_3_valida_ENSO = janela_3_valida_ENSO
  norm_janela_4_valida_ENSO = janela_4_valida_ENSO
  norm_janela_5_valida_ENSO = janela_5_valida_ENSO

  for(j in 1:ncol(janela_1_fit_ENSO)){
    norm_janela_1_fit_ENSO[,j] = janela_1_fit_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_2_fit_ENSO[,j] = janela_2_fit_ENSO[,j]/max_ENSO_janela_2[j]
    norm_janela_3_fit_ENSO[,j] = janela_3_fit_ENSO[,j]/max_ENSO_janela_3[j]
    norm_janela_4_fit_ENSO[,j] = janela_4_fit_ENSO[,j]/max_ENSO_janela_4[j]
    norm_janela_5_fit_ENSO[,j] = janela_5_fit_ENSO[,j]/max_ENSO_janela_5[j]

    norm_janela_1_valida_ENSO[,j] = janela_1_valida_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_2_valida_ENSO[,j] = janela_2_valida_ENSO[,j]/max_ENSO_janela_2[j]
    norm_janela_3_valida_ENSO[,j] = janela_3_valida_ENSO[,j]/max_ENSO_janela_3[j]
    norm_janela_4_valida_ENSO[,j] = janela_4_valida_ENSO[,j]/max_ENSO_janela_4[j]
    norm_janela_5_valida_ENSO[,j] = janela_5_valida_ENSO[,j]/max_ENSO_janela_5[j]
  }
}





# Calculando o modelo
# Erros
nome = colnames(Ventos)
erros_RMSE = matrix(NA,ncol=5,nrow=2,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_MAE = matrix(NA,ncol=5,nrow=2,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))
erros_R2 = matrix(NA,ncol=5,nrow=2,dimnames = list(nome,c("Janela 2","Janela 3","Janela 4","Janela 5","Média")))

modelo = 32

# # Modelo
# # Caso
# # Procurando melhor p e m para janela 1
# dados1 = norm_janela_1_fit_Ventos
# dados2 = window(norm_janela_1_fit_ENSO[,1],start=1980)
# dados1_valida = janela_1_valida_Ventos
# dados2_valida = norm_janela_1_valida_ENSO[,1]
# max_dados1 = max_Ventos_janela_1
# 
# aux = PARX_melhor_p_m_cov(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=5)
# MAE_p_m_janela_1 = aux[[1]]
# 
# write.csv2(MAE_p_m_janela_1,paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))
# 
# # Buscando arquivo de erros para janela 1
# aux = read.csv2(paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))[,-1]
# 
# # Calculando métricas para cada janela
# dados1_janela_2 = norm_janela_2_fit_Ventos
# dados2_janela_2 = window(norm_janela_2_fit_ENSO[,1],start=1980)
# dados1_novo_janela_2 = window(dados1_janela_2,start=start(dados2_janela_2))
# dados1_valida_janela_2 = janela_2_valida_Ventos
# dados2_valida_janela_2 = norm_janela_2_valida_ENSO[,1]
# janela_2 = cenarios_parX_LogNormal(dados1_novo_janela_2,dados2_janela_2,dados2_valida_janela_2,max_p=aux[,1],max_m=aux[,2],num_cenarios=20)
# 
# prev_2 = colMeans(janela_2[[1]])*max_Ventos_janela_2[1]
# erros_RMSE[1,1] = RMSE(janela_2_valida_Ventos[,1],prev_2)
# erros_MAE[1,1] = MAE(janela_2_valida_Ventos[,1],prev_2)
# erros_R2[1,1] = R2(janela_2_valida_Ventos[,1],prev_2)





# Completo
for(ENSO in 1:14){
  
  MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=2)
  
  #Procurando melhor p e m para janela 1
  
  dados1 = norm_janela_1_fit_Ventos
  dados2 = window(norm_janela_1_fit_ENSO[,ENSO],start=1980)
  dados1_valida = janela_1_valida_Ventos
  dados2_valida = norm_janela_1_valida_ENSO[,ENSO]
  max_dados1 = max_Ventos_janela_1
  
  aux = PARX_melhor_p_m_cov(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=500)
  MAE_p_m_janela_1 = aux[[1]]
  
  write.csv2(MAE_p_m_janela_1,paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))
  
  #Buscando arquivo de erros para janela 1
  aux = read.csv2(paste("ERROS_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))[,-1]
  
  
  #Calculando métricas para cada janela
  
  dados1_janela_2 = norm_janela_2_fit_Ventos
  dados1_janela_3 = norm_janela_3_fit_Ventos
  dados1_janela_4 = norm_janela_4_fit_Ventos
  dados1_janela_5 = norm_janela_5_fit_Ventos

  dados2_janela_2 = window(norm_janela_2_fit_ENSO[,ENSO],start=1980)
  dados2_janela_3 = window(norm_janela_3_fit_ENSO[,ENSO],start=1980)
  dados2_janela_4 = window(norm_janela_4_fit_ENSO[,ENSO],start=1980)
  dados2_janela_5 = window(norm_janela_5_fit_ENSO[,ENSO],start=1980)

  dados1_novo_janela_2 = window(dados1_janela_2,start=start(dados2_janela_2))
  dados1_novo_janela_3 = window(dados1_janela_3,start=start(dados2_janela_3))
  dados1_novo_janela_4 = window(dados1_janela_4,start=start(dados2_janela_4))
  dados1_novo_janela_5 = window(dados1_janela_5,start=start(dados2_janela_5))

  dados1_valida_janela_2 = janela_2_valida_Ventos
  dados1_valida_janela_3 = janela_3_valida_Ventos
  dados1_valida_janela_4 = janela_4_valida_Ventos
  dados1_valida_janela_5 = janela_5_valida_Ventos

  dados2_valida_janela_2 = norm_janela_2_valida_ENSO[,ENSO]
  dados2_valida_janela_3 = norm_janela_3_valida_ENSO[,ENSO]
  dados2_valida_janela_4 = norm_janela_4_valida_ENSO[,ENSO]
  dados2_valida_janela_5 = norm_janela_5_valida_ENSO[,ENSO]

  janela_2 = cenarios_parX_LogNormal_cov(dados1_novo_janela_2,dados2_janela_2,dados2_valida_janela_2,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
  janela_3 = cenarios_parX_LogNormal_cov(dados1_novo_janela_3,dados2_janela_3,dados2_valida_janela_3,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
  janela_4 = cenarios_parX_LogNormal_cov(dados1_novo_janela_4,dados2_janela_4,dados2_valida_janela_4,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
  janela_5 = cenarios_parX_LogNormal_cov(dados1_novo_janela_5,dados2_janela_5,dados2_valida_janela_5,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)

  
  for(usina in 1:2){
    
    prev_2 = colMeans(janela_2[[usina]])*max_Ventos_janela_2[usina]
    prev_3 = colMeans(janela_3[[usina]])*max_Ventos_janela_3[usina]
    prev_4 = colMeans(janela_4[[usina]])*max_Ventos_janela_4[usina]
    prev_5 = colMeans(janela_5[[usina]])*max_Ventos_janela_5[usina]

    erros_RMSE[usina,1] = RMSE(janela_2_valida_Ventos[,usina],prev_2)
    erros_RMSE[usina,2] = RMSE(janela_3_valida_Ventos[,usina],prev_3)
    erros_RMSE[usina,3] = RMSE(janela_4_valida_Ventos[,usina],prev_4)
    erros_RMSE[usina,4] = RMSE(janela_5_valida_Ventos[,usina],prev_5)
    erros_RMSE[usina,5] = mean(erros_RMSE[usina,1:4])
    
    erros_MAE[usina,1] = MAE(janela_2_valida_Ventos[,usina],prev_2)
    erros_MAE[usina,2] = MAE(janela_3_valida_Ventos[,usina],prev_3)
    erros_MAE[usina,3] = MAE(janela_4_valida_Ventos[,usina],prev_4)
    erros_MAE[usina,4] = MAE(janela_5_valida_Ventos[,usina],prev_5)
    erros_MAE[usina,5] = mean(erros_MAE[usina,1:4])
    
    erros_R2[usina,1] = R2(janela_2_valida_Ventos[,usina],prev_2)
    erros_R2[usina,2] = R2(janela_3_valida_Ventos[,usina],prev_3)
    erros_R2[usina,3] = R2(janela_4_valida_Ventos[,usina],prev_4)
    erros_R2[usina,4] = R2(janela_5_valida_Ventos[,usina],prev_5)
    erros_R2[usina,5] = mean(erros_R2[usina,1:4])
    
    
    print(usina)
    
    if(usina == 2){
      erros = cbind(erros_RMSE[,5],erros_MAE[,5],erros_R2[,5])
      colnames(erros) = c("RMSE","MAE","R2")
      write.csv2(erros,paste("ERROS_RESULTADOS//Erros_Modelo_",modelo,".csv",sep=""))
    }
    
  }
  
  modelo = modelo + 1
  
}
#

















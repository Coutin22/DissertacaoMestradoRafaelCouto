# Diretorio
setwd("~/Rafael_Estudo/PUC/Dissertação/Resultados")

# Pacotes
{
library(readxl)
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
library(reshape2)
library(stringr)  
}



# Grafico da Previsão
# PAR 
# Geral
# Janela 6
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,-1], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")
detach("package:dplyr", unload = TRUE)

## JANELAS DE PREVISÃO 
{ 
  janela_6_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2022,12),freq=12)
  janela_6_valida_Ventos = window(Ventos,start=c(2023,1),end=c(2023,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}

  max_Ventos_janela_6 = apply(janela_6_fit_Ventos,2,max_abs)

  norm_janela_6_fit_Ventos = janela_6_fit_Ventos
  
  for(j in 1:ncol(janela_6_fit_Ventos)){
    norm_janela_6_fit_Ventos[,j] = janela_6_fit_Ventos[,j]/max_Ventos_janela_6[j]
  }
}

modelo = 1

## Calculando as ordens do modelo
par_ordem = matrix(NA, ncol = 12, nrow = 7)
par_ordem_janela_6 = par_ordem


# Calculando as ordens do modelo
for(usina in 1:7){
  
  par_ordem_janela_6[usina,] = escolha_ordem(norm_janela_6_fit_Ventos[,usina], lag.max=6)
  
}


# Guardando resultados da previsão
PAR_previsao <- matrix(NA, ncol = 7, nrow = 12)
colnames(PAR_previsao) <- colnames(Ventos)


# Calculando o modelo
# Modelo
janela_6 = cenarios_par_LogNormal(norm_janela_6_fit_Ventos, num_periodos=12, num_cenarios=2000, par_ordem_janela_6)

for(usina in 1:7){
  
  prev_6 = colMeans(janela_6[[usina]])*max_Ventos_janela_6[usina]

  PAR_previsao[,usina] <- prev_6 
  
  print(usina)
  
  if(usina == 7){
    PAR_previsao <- data.frame(PAR_previsao)
    write.xlsx(PAR_previsao,paste("PREVISAO_RESULTADOS//PAR_Janela_6_Modelo_",modelo,".xlsx",sep=""))
  }
  
 
}

# Histórico
write.xlsx(janela_6_valida_Ventos, "PREVISAO_RESULTADOS//Validação_Janela_6.xlsx")
#





# PAR 
# Geral
# Janela 7
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,-1], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")
detach("package:dplyr", unload = TRUE)

## JANELAS DE PREVISÃO 
{ 
  janela_7_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2023,12),freq=12)
  #janela_7_valida_Ventos = window(Ventos,start=c(2024,1),end=c(2024,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_7 = apply(janela_7_fit_Ventos,2,max_abs)
  
  norm_janela_7_fit_Ventos = janela_7_fit_Ventos
  
  for(j in 1:ncol(janela_7_fit_Ventos)){
    norm_janela_7_fit_Ventos[,j] = janela_7_fit_Ventos[,j]/max_Ventos_janela_7[j]
  }
}

modelo = 100

## Calculando as ordens do modelo
par_ordem = matrix(NA, ncol = 12, nrow = 7)
par_ordem_janela_7 = par_ordem


# Calculando as ordens do modelo
for(usina in 1:7){
  par_ordem_janela_7[usina,] = escolha_ordem(norm_janela_7_fit_Ventos[,usina], lag.max=6)
}


# Guardando resultados da previsão
PAR_previsao <- matrix(NA, ncol = 7, nrow = 12)
colnames(PAR_previsao) <- colnames(Ventos)


# Calculando o modelo
# Modelo
janela_7 = cenarios_par_LogNormal(norm_janela_7_fit_Ventos, num_periodos=12, num_cenarios=2000, par_ordem_janela_7)

for(usina in 1:7){
  
  prev_7 = colMeans(janela_7[[usina]])*max_Ventos_janela_7[usina]
  
  PAR_previsao[,usina] <- prev_7 
  
  print(usina)
  
  if(usina == 7){
    PAR_previsao <- data.frame(PAR_previsao)
    PAR_previsao$mes <- rep(1:12,1)
    write.xlsx(PAR_previsao,paste("PREVISAO_RESULTADOS//PAR_Janela_7_Modelo_",modelo,".xlsx",sep=""))
  }
}
# 









# PARX 
# Geral (Alagoas, Paraíba e Sergipe / ONI ACUM e NINO4 ACUM)
# Janela 6
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,-1], start=1980, freq=12)
ENSO <- ts(read_excel("Previsão_ENSO_Completa_2023.xlsx")[c(589:1116),c(15,13)], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")

## JANELAS DE PREVISÃO VENTO
{janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_6_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2022,12),freq=12)
  janela_6_valida_Ventos = window(Ventos,start=c(2023,1),end=c(2023,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_6 = apply(janela_6_fit_Ventos,2,max_abs)
  
  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_6_fit_Ventos = janela_6_fit_Ventos
  
  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_6_fit_Ventos[,j] = janela_6_fit_Ventos[,j]/max_Ventos_janela_6[j]
  }
}


## JANELAS DE PREVISÃO ENSO
{janela_1_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_ENSO = window(ENSO,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_6_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2022,12),freq=12)
  janela_6_valida_ENSO = window(ENSO,start=c(2023,1),end=c(2023,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_ENSO_janela_1 = apply(janela_1_fit_ENSO,2,max_abs)
  max_ENSO_janela_6 = apply(janela_6_fit_ENSO,2,max_abs)
  
  norm_janela_1_fit_ENSO = janela_1_fit_ENSO
  norm_janela_6_fit_ENSO = janela_6_fit_ENSO
  
  norm_janela_1_valida_ENSO = janela_1_valida_ENSO
  norm_janela_6_valida_ENSO = janela_6_valida_ENSO
  
  for(j in 1:ncol(janela_1_fit_ENSO)){
    norm_janela_1_fit_ENSO[,j] = janela_1_fit_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_6_fit_ENSO[,j] = janela_6_fit_ENSO[,j]/max_ENSO_janela_6[j]
    
    norm_janela_1_valida_ENSO[,j] = janela_1_valida_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_6_valida_ENSO[,j] = janela_6_valida_ENSO[,j]/max_ENSO_janela_6[j]
  }
}

modelo = 2
MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=7)

PARX_previsao <- matrix(NA, ncol = 7, nrow = 12)
colnames(PARX_previsao) <- colnames(Ventos)
                             

# Completo
for(ENSO in 1:2){
  
  MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=7)
  
  #Procurando melhor p e m para janela 1
  
  dados1 = norm_janela_1_fit_Ventos
  dados2 = window(norm_janela_1_fit_ENSO[,ENSO],start=1980)
  dados1_valida = janela_1_valida_Ventos
  dados2_valida = norm_janela_1_valida_ENSO[,ENSO]
  max_dados1 = max_Ventos_janela_1
  
  aux = PARX_melhor_p_m(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=500)
  MAE_p_m_janela_1 = aux[[1]]
  
  write.csv2(MAE_p_m_janela_1,paste("PREVISAO_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))
  
  #Buscando arquivo de erros para janela 1
  aux = read.csv2(paste("PREVISAO_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))[,-1]
  
  #Calculando métricas para cada janela
  
  dados1_janela_6 = norm_janela_6_fit_Ventos
  
  dados2_janela_6 = window(norm_janela_6_fit_ENSO[,ENSO],start=1980)
  
  dados1_novo_janela_6 = window(dados1_janela_6,start=start(dados2_janela_6))
  
  dados1_valida_janela_6 = janela_6_valida_Ventos
  
  dados2_valida_janela_6 = norm_janela_6_valida_ENSO[,ENSO]
  
  janela_6 = cenarios_parX_LogNormal(dados1_novo_janela_6,dados2_janela_6,dados2_valida_janela_6,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
  
  for(usina in 1:7){
    
    prev_6 = colMeans(janela_6[[usina]])*max_Ventos_janela_6[usina]
    
    PARX_previsao[,usina] <- prev_6 
    
    print(usina)
    
    if(usina == 7){
      PARX_previsao <- ts(PARX_previsao, start=1980, freq=12)
      write.xlsx(PARX_previsao,paste("PREVISAO_RESULTADOS//PARX_Janela_6_Modelo_",modelo,".xlsx",sep=""))
    }
    
  }
  
  modelo = modelo + 1
  
}
#




# PARX
# Geral (Alagoas, Paraíba e Sergipe / ONI ACUM e NINO4 ACUM)
# Janela 7
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,-1], start=1980, freq=12)
ENSO <- ts(read_excel("Previsão_ENSO_Completa_2024.xlsx")[c(589:1128),c(15,13)], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")
source("cenarios_tratamento.R")

## JANELAS DE PREVISÃO VENTO
{
  janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)

  janela_7_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2023,12),freq=12)
  #janela_7_valida_Ventos = window(Ventos,start=c(2024,1),end=c(2024,12),freq=12)

  max_abs = function(X){max(abs(X),na.rm=T)}

  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_7 = apply(janela_7_fit_Ventos,2,max_abs)

  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_7_fit_Ventos = janela_7_fit_Ventos

  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_7_fit_Ventos[,j] = janela_7_fit_Ventos[,j]/max_Ventos_janela_7[j]
  }
}


## JANELAS DE PREVISÃO ENSO
{
  janela_1_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_ENSO = window(ENSO,start=c(2014,1),end=c(2018,12),freq=12)

  janela_7_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2023,12),freq=12)
  janela_7_valida_ENSO = window(ENSO,start=c(2024,1),end=c(2024,12),freq=12)

  max_abs = function(X){max(abs(X),na.rm=T)}

  max_ENSO_janela_1 = apply(janela_1_fit_ENSO,2,max_abs)
  max_ENSO_janela_7 = apply(janela_7_fit_ENSO,2,max_abs)

  norm_janela_1_fit_ENSO = janela_1_fit_ENSO
  norm_janela_7_fit_ENSO = janela_7_fit_ENSO

  norm_janela_1_valida_ENSO = janela_1_valida_ENSO
  norm_janela_7_valida_ENSO = janela_7_valida_ENSO

  for(j in 1:ncol(janela_1_fit_ENSO)){
    norm_janela_1_fit_ENSO[,j] = janela_1_fit_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_7_fit_ENSO[,j] = janela_7_fit_ENSO[,j]/max_ENSO_janela_7[j]

    norm_janela_1_valida_ENSO[,j] = janela_1_valida_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_7_valida_ENSO[,j] = janela_7_valida_ENSO[,j]/max_ENSO_janela_7[j]
  }
}

modelo = 4                                                          
MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=7)

PARX_previsao <- matrix(NA, ncol = 7, nrow = 12)
colnames(PARX_previsao) <- colnames(Ventos)

n_sim = 2000
PARX_cenarios <- matrix(NA, ncol = 9, nrow = 12*n_sim)
colnames(PARX_cenarios) <- c(colnames(Ventos),"Cenário","Mês")



# Completo
for(ENSO in 1:2){

  MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=7)

  #Procurando melhor p e m para janela 1

  dados1 = norm_janela_1_fit_Ventos
  dados2 = window(norm_janela_1_fit_ENSO[,ENSO],start=1980)
  dados1_valida = janela_1_valida_Ventos
  dados2_valida = norm_janela_1_valida_ENSO[,ENSO]
  max_dados1 = max_Ventos_janela_1

  aux = PARX_melhor_p_m(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=500)
  MAE_p_m_janela_1 = aux[[1]]

  write.csv2(MAE_p_m_janela_1,paste("PREVISAO_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))

  #Buscando arquivo de erros para janela 1
  aux = read.csv2(paste("PREVISAO_RESULTADOS//MAE_Janela_1_Modelo_",modelo,".csv",sep=""))[,-1]

  #Calculando métricas para cada janela

  dados1_janela_7 = norm_janela_7_fit_Ventos

  dados2_janela_7 = window(norm_janela_7_fit_ENSO[,ENSO],start=1980)

  dados1_novo_janela_7 = window(dados1_janela_7,start=start(dados2_janela_7))

  #dados1_valida_janela_7 = janela_7_valida_Ventos

  dados2_valida_janela_7 = norm_janela_7_valida_ENSO[,ENSO]

  janela_7 = cenarios_parX_LogNormal(dados1_novo_janela_7,dados2_janela_7,dados2_valida_janela_7,max_p=aux[,1],max_m=aux[,2],num_cenarios=n_sim)

  cenarios_aux <- cenarios_tratamento_aux(janela_7[[1]],n_sim)
  PARX_cenarios[,8] <- sort(cenarios_aux[,1])
  PARX_cenarios[,9] <- cenarios_aux[,2]
  
  
  for(usina in 1:7){

    prev_7 = colMeans(janela_7[[usina]])*max_Ventos_janela_7[usina]
    PARX_previsao[,usina] <- prev_7
    
    cenarios_final <- cenarios_tratamento_final(janela_7[[usina]],max_Ventos_janela_7[usina])
    PARX_cenarios[,usina] <- cenarios_final

    print(usina)

    if(usina == 7){
      PARX_previsao <- data.frame(PARX_previsao)
      PARX_previsao$mes <-  rep(1:12,1)
      write.xlsx(PARX_previsao,paste("PREVISAO_RESULTADOS//PARX_Previsão_Janela_7_Modelo_",modelo,".xlsx",sep=""))
      
      PARX_cenarios <- data.frame(PARX_cenarios)
      write.xlsx(PARX_cenarios,paste("PREVISAO_RESULTADOS//PARX_Cenários_Janela_7_Modelo_",modelo,".xlsx",sep=""))
    }

  }

  modelo = modelo + 1

}
#










# PARX-Cov  
# Nordeste (Pernambuco e Rio Grande do Norte / ONI ACUM e SOI)
# Janela 6
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,c(2:5,8)], start=1980, freq=12)
ENSO <- ts(read_excel("Previsão_ENSO_Completa_2023.xlsx")[c(589:1116),c(15,2)], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")


## JANELAS DE PREVISÃO VENTO
{janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_6_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2022,12),freq=12)
  janela_6_valida_Ventos = window(Ventos,start=c(2023,1),end=c(2023,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_6 = apply(janela_6_fit_Ventos,2,max_abs)
  
  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_6_fit_Ventos = janela_6_fit_Ventos
  
  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_6_fit_Ventos[,j] = janela_6_fit_Ventos[,j]/max_Ventos_janela_6[j]
  }
}


## JANELAS DE PREVISÃO ENSO
{janela_1_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_ENSO = window(ENSO,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_6_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2022,12),freq=12)
  janela_6_valida_ENSO = window(ENSO,start=c(2023,1),end=c(2023,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_ENSO_janela_1 = apply(janela_1_fit_ENSO,2,max_abs)
  max_ENSO_janela_6 = apply(janela_6_fit_ENSO,2,max_abs)
  
  norm_janela_1_fit_ENSO = janela_1_fit_ENSO
  norm_janela_6_fit_ENSO = janela_6_fit_ENSO
  
  norm_janela_1_valida_ENSO = janela_1_valida_ENSO
  norm_janela_6_valida_ENSO = janela_6_valida_ENSO
  
  for(j in 1:ncol(janela_1_fit_ENSO)){
    norm_janela_1_fit_ENSO[,j] = janela_1_fit_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_6_fit_ENSO[,j] = janela_6_fit_ENSO[,j]/max_ENSO_janela_6[j]
    
    norm_janela_1_valida_ENSO[,j] = janela_1_valida_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_6_valida_ENSO[,j] = janela_6_valida_ENSO[,j]/max_ENSO_janela_6[j]
  }
}


modelo = 6
MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=5)

PARX_Cov_previsao <- matrix(NA, ncol = 5, nrow = 12)
colnames(PARX_Cov_previsao) <- colnames(Ventos)


# Completo
for(ENSO in 1:2){
  
  MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=5)
  
  #Procurando melhor p e m para janela 1
  
  dados1 = norm_janela_1_fit_Ventos
  dados2 = window(norm_janela_1_fit_ENSO[,ENSO],start=1980)
  dados1_valida = janela_1_valida_Ventos
  dados2_valida = norm_janela_1_valida_ENSO[,ENSO]
  max_dados1 = max_Ventos_janela_1
  
  aux = PARX_melhor_p_m_cov(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=500)
  MAE_p_m_janela_1 = aux[[1]]
  
  write.csv2(MAE_p_m_janela_1,paste("PREVISAO_RESULTADOS//MAE_Janela_1_Nordeste_Modelo_",modelo,".csv",sep=""))
  
  #Buscando arquivo de erros para janela 1
  aux = read.csv2(paste("PREVISAO_RESULTADOS//MAE_Janela_1_Nordeste_Modelo_",modelo,".csv",sep=""))[,-1]
  
  
  #Calculando métricas para cada janela
    
    dados1_janela_6 = norm_janela_6_fit_Ventos
    
    dados2_janela_6 = window(norm_janela_6_fit_ENSO[,ENSO],start=1980)
    
    dados1_novo_janela_6 = window(dados1_janela_6,start=start(dados2_janela_6))
    
    dados1_valida_janela_6 = janela_6_valida_Ventos
    
    dados2_valida_janela_6 = norm_janela_6_valida_ENSO[,ENSO]
    
    janela_6 = cenarios_parX_LogNormal_cov(dados1_novo_janela_6,dados2_janela_6,dados2_valida_janela_6,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
    
    
    for(usina in 1:5){
    
    prev_6 = colMeans(janela_6[[usina]])*max_Ventos_janela_6[usina]
      
    PARX_Cov_previsao[,usina] <- prev_6 
    
    print(usina)
    
    if(usina == 5){
      PARX_Cov_previsao <- ts(PARX_Cov_previsao, start=1980, freq=12)
      write.xlsx(PARX_Cov_previsao,paste("PREVISAO_RESULTADOS//PARX_Cov_Janela_6_Nordeste_Modelo_",modelo,".xlsx",sep=""))
    }
    
  }
  
  modelo = modelo + 1
  
}
# 




# PARX-Cov  
# Nordeste (Pernambuco e Rio Grande do Norte / ONI ACUM e SOI)
# Janela 7
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,c(2:5,8)], start=1980, freq=12)
ENSO <- ts(read_excel("Previsão_ENSO_Completa_2024.xlsx")[c(589:1128),c(15,2)], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")
source("cenarios_tratamento.R")

## JANELAS DE PREVISÃO VENTO
{janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_7_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2023,12),freq=12)
#  janela_7_valida_Ventos = window(Ventos,start=c(2024,1),end=c(2024,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_7 = apply(janela_7_fit_Ventos,2,max_abs)
  
  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_7_fit_Ventos = janela_7_fit_Ventos
  
  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_7_fit_Ventos[,j] = janela_7_fit_Ventos[,j]/max_Ventos_janela_7[j]
  }
}


## JANELAS DE PREVISÃO ENSO
{janela_1_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_ENSO = window(ENSO,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_7_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2023,12),freq=12)
  janela_7_valida_ENSO = window(ENSO,start=c(2024,1),end=c(2024,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_ENSO_janela_1 = apply(janela_1_fit_ENSO,2,max_abs)
  max_ENSO_janela_7 = apply(janela_7_fit_ENSO,2,max_abs)
  
  norm_janela_1_fit_ENSO = janela_1_fit_ENSO
  norm_janela_7_fit_ENSO = janela_7_fit_ENSO
  
  norm_janela_1_valida_ENSO = janela_1_valida_ENSO
  norm_janela_7_valida_ENSO = janela_7_valida_ENSO
  
  for(j in 1:ncol(janela_1_fit_ENSO)){
    norm_janela_1_fit_ENSO[,j] = janela_1_fit_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_7_fit_ENSO[,j] = janela_7_fit_ENSO[,j]/max_ENSO_janela_7[j]
    
    norm_janela_1_valida_ENSO[,j] = janela_1_valida_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_7_valida_ENSO[,j] = janela_7_valida_ENSO[,j]/max_ENSO_janela_7[j]
  }
}


modelo = 8
MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=5)

PARX_Cov_previsao <- matrix(NA, ncol = 5, nrow = 12)
colnames(PARX_Cov_previsao) <- colnames(Ventos)

n_sim = 2000
PARX_Cov_cenarios <- matrix(NA, ncol = 7, nrow = 12*n_sim)
colnames(PARX_Cov_cenarios) <- c(colnames(Ventos),"Cenário","Mês")


# Completo
for(ENSO in 1:2){
  
  MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=5)
  
  #Procurando melhor p e m para janela 1
  
  dados1 = norm_janela_1_fit_Ventos
  dados2 = window(norm_janela_1_fit_ENSO[,ENSO],start=1980)
  dados1_valida = janela_1_valida_Ventos
  dados2_valida = norm_janela_1_valida_ENSO[,ENSO]
  max_dados1 = max_Ventos_janela_1
  
  aux = PARX_melhor_p_m_cov(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=500)
  MAE_p_m_janela_1 = aux[[1]]
  
  write.csv2(MAE_p_m_janela_1,paste("PREVISAO_RESULTADOS//MAE_Janela_1_Nordeste_Modelo_",modelo,".csv",sep=""))
  
  #Buscando arquivo de erros para janela 1
  aux = read.csv2(paste("PREVISAO_RESULTADOS//MAE_Janela_1_Nordeste_Modelo_",modelo,".csv",sep=""))[,-1]
  
  
  #Calculando métricas para cada janela
  
  dados1_janela_7 = norm_janela_7_fit_Ventos
  
  dados2_janela_7 = window(norm_janela_7_fit_ENSO[,ENSO],start=1980)
  
  dados1_novo_janela_7 = window(dados1_janela_7,start=start(dados2_janela_7))
  
#  dados1_valida_janela_7 = janela_7_valida_Ventos
  
  dados2_valida_janela_7 = norm_janela_7_valida_ENSO[,ENSO]
  
  janela_7 = cenarios_parX_LogNormal_cov(dados1_novo_janela_7,dados2_janela_7,dados2_valida_janela_7,max_p=aux[,1],max_m=aux[,2],num_cenarios=n_sim)
  
  cenarios_aux <- cenarios_tratamento_aux(janela_7[[1]],n_sim)
  PARX_Cov_cenarios[,6] <- sort(cenarios_aux[,1])
  PARX_Cov_cenarios[,7] <- cenarios_aux[,2]
  
  
  for(usina in 1:5){
    
    prev_7 = colMeans(janela_7[[usina]])*max_Ventos_janela_7[usina]
    PARX_Cov_previsao[,usina] <- prev_7 
    
    cenarios_final <- cenarios_tratamento_final(janela_7[[usina]],max_Ventos_janela_7[usina])
    PARX_Cov_cenarios[,usina] <- cenarios_final
    
    print(usina)
    
    if(usina == 5){
      PARX_Cov_previsao <- data.frame(PARX_Cov_previsao)
      PARX_Cov_previsao$mes <-  rep(1:12,1)
      write.xlsx(PARX_Cov_previsao,paste("PREVISAO_RESULTADOS//PARX_Cov_Previsão_Janela_7_Nordeste_Modelo_",modelo,".xlsx",sep=""))
    
      PARX_Cov_cenarios <- data.frame(PARX_Cov_cenarios)
      write.xlsx(PARX_Cov_cenarios,paste("PREVISAO_RESULTADOS//PARX_Cov_Cenários_Janela_7_Nordeste_Modelo_",modelo,".xlsx",sep=""))
      }
    
  }
  
  modelo = modelo + 1
  
}
# 








# Sul (Rio Grande do Sul e Santa Catarina / ONI Aumulado)
# Janela 6
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,c(6,7)], start=1980, freq=12)
ENSO <- ts(read_excel("Previsão_ENSO_Completa_2023.xlsx")[c(589:1116),15], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")

## JANELAS DE PREVISÃO VENTO
{janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_6_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2022,12),freq=12)
  janela_6_valida_Ventos = window(Ventos,start=c(2023,1),end=c(2023,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_6 = apply(janela_6_fit_Ventos,2,max_abs)
  
  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_6_fit_Ventos = janela_6_fit_Ventos
  
  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_6_fit_Ventos[,j] = janela_6_fit_Ventos[,j]/max_Ventos_janela_6[j]
  }
}


## JANELAS DE PREVISÃO ENSO
{janela_1_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_ENSO = window(ENSO,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_6_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2022,12),freq=12)
  janela_6_valida_ENSO = window(ENSO,start=c(2023,1),end=c(2023,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_ENSO_janela_1 = apply(janela_1_fit_ENSO,2,max_abs)
  max_ENSO_janela_6 = apply(janela_6_fit_ENSO,2,max_abs)
  
  norm_janela_1_fit_ENSO = janela_1_fit_ENSO
  norm_janela_6_fit_ENSO = janela_6_fit_ENSO
  
  norm_janela_1_valida_ENSO = janela_1_valida_ENSO
  norm_janela_6_valida_ENSO = janela_6_valida_ENSO
  
  for(j in 1:ncol(janela_1_fit_ENSO)){
    norm_janela_1_fit_ENSO[,j] = janela_1_fit_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_6_fit_ENSO[,j] = janela_6_fit_ENSO[,j]/max_ENSO_janela_6[j]
    
    norm_janela_1_valida_ENSO[,j] = janela_1_valida_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_6_valida_ENSO[,j] = janela_6_valida_ENSO[,j]/max_ENSO_janela_6[j]
  }
}

modelo = 10
MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=2)

PARX_Cov_previsao <- matrix(NA, ncol = 2, nrow = 12)
colnames(PARX_Cov_previsao) <- colnames(Ventos)


# Completo
for(ENSO in 1:1){
  
  MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=2)
  
  #Procurando melhor p e m para janela 1
  
  dados1 = norm_janela_1_fit_Ventos
  dados2 = window(norm_janela_1_fit_ENSO[,ENSO],start=1980)
  dados1_valida = janela_1_valida_Ventos
  dados2_valida = norm_janela_1_valida_ENSO[,ENSO]
  max_dados1 = max_Ventos_janela_1
  
  aux = PARX_melhor_p_m_cov(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=500)
  MAE_p_m_janela_1 = aux[[1]]
  
  write.csv2(MAE_p_m_janela_1,paste("PREVISAO_RESULTADOS//MAE_Janela_1_Sul_Modelo_",modelo,".csv",sep=""))
  
  #Buscando arquivo de erros para janela 1
  aux = read.csv2(paste("PREVISAO_RESULTADOS//MAE_Janela_1_Sul_Modelo_",modelo,".csv",sep=""))[,-1]
  
  
  #Calculando métricas para cada janela
    
    dados1_janela_6 = norm_janela_6_fit_Ventos
    
    dados2_janela_6 = window(norm_janela_6_fit_ENSO[,ENSO],start=1980)
    
    dados1_novo_janela_6 = window(dados1_janela_6,start=start(dados2_janela_6))
    
    dados1_valida_janela_6 = janela_6_valida_Ventos
    
    dados2_valida_janela_6 = norm_janela_6_valida_ENSO[,ENSO]
    
    janela_6 = cenarios_parX_LogNormal_cov(dados1_novo_janela_6,dados2_janela_6,dados2_valida_janela_6,max_p=aux[,1],max_m=aux[,2],num_cenarios=2000)
    
    
    for(usina in 1:2){
      
      prev_6 = colMeans(janela_6[[usina]])*max_Ventos_janela_6[usina]
      
      PARX_Cov_previsao[,usina] <- prev_6 
      
      print(usina)
      
      if(usina == 2){
        PARX_Cov_previsao <- ts(PARX_Cov_previsao, start=1980, freq=12)
        write.xlsx(PARX_Cov_previsao,paste("PREVISAO_RESULTADOS//PARX_Cov_Janela_6_Sul_Modelo_",modelo,".xlsx",sep=""))
      }
      
    }
    
    modelo = modelo + 1
    
}
# 






# Sul (Rio Grande do Sul e Santa Catarin / ONI Aumulado)
# Janela 7
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,c(6,7)], start=1980, freq=12)
ENSO <- ts(read_excel("Previsão_ENSO_Completa_2024.xlsx")[c(589:1128),15], start=1980, freq=12)

source("Funcoes_Pear.R")
source("Modelos_Funcoes_Rafael.R")
source("cenarios_tratamento.R")

## JANELAS DE PREVISÃO VENTO
{janela_1_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_Ventos = window(Ventos,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_7_fit_Ventos = window(Ventos,start=c(1980,1),end=c(2023,12),freq=12)
#  janela_7_valida_Ventos = window(Ventos,start=c(2024,1),end=c(2024,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_Ventos_janela_1 = apply(janela_1_fit_Ventos,2,max_abs)
  max_Ventos_janela_7 = apply(janela_7_fit_Ventos,2,max_abs)
  
  norm_janela_1_fit_Ventos = janela_1_fit_Ventos
  norm_janela_7_fit_Ventos = janela_7_fit_Ventos
  
  for(j in 1:ncol(janela_1_fit_Ventos)){
    norm_janela_1_fit_Ventos[,j] = janela_1_fit_Ventos[,j]/max_Ventos_janela_1[j]
    norm_janela_7_fit_Ventos[,j] = janela_7_fit_Ventos[,j]/max_Ventos_janela_7[j]
  }
}


## JANELAS DE PREVISÃO ENSO
{janela_1_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2013,12),freq=12)
  janela_1_valida_ENSO = window(ENSO,start=c(2014,1),end=c(2018,12),freq=12)
  
  janela_7_fit_ENSO = window(ENSO,start=c(1980,1),end=c(2023,12),freq=12)
  janela_7_valida_ENSO = window(ENSO,start=c(2024,1),end=c(2024,12),freq=12)
  
  max_abs = function(X){max(abs(X),na.rm=T)}
  
  max_ENSO_janela_1 = apply(janela_1_fit_ENSO,2,max_abs)
  max_ENSO_janela_7 = apply(janela_7_fit_ENSO,2,max_abs)
  
  norm_janela_1_fit_ENSO = janela_1_fit_ENSO
  norm_janela_7_fit_ENSO = janela_7_fit_ENSO
  
  norm_janela_1_valida_ENSO = janela_1_valida_ENSO
  norm_janela_7_valida_ENSO = janela_7_valida_ENSO
  
  for(j in 1:ncol(janela_1_fit_ENSO)){
    norm_janela_1_fit_ENSO[,j] = janela_1_fit_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_7_fit_ENSO[,j] = janela_7_fit_ENSO[,j]/max_ENSO_janela_7[j]
    
    norm_janela_1_valida_ENSO[,j] = janela_1_valida_ENSO[,j]/max_ENSO_janela_1[j]
    norm_janela_7_valida_ENSO[,j] = janela_7_valida_ENSO[,j]/max_ENSO_janela_7[j]
  }
}

modelo = 11
MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=2)

PARX_Cov_previsao <- matrix(NA, ncol = 2, nrow = 12)
colnames(PARX_Cov_previsao) <- colnames(Ventos)

n_sim = 2000
PARX_Cov_cenarios <- matrix(NA, ncol = 4, nrow = 12*n_sim)
colnames(PARX_Cov_cenarios) <- c(colnames(Ventos),"Cenário","Mês")


# Completo
for(ENSO in 1:1){
  
  MAE_p_m_janela_1 = matrix(NA,ncol=2,nrow=2)
  
  #Procurando melhor p e m para janela 1
  
  dados1 = norm_janela_1_fit_Ventos
  dados2 = window(norm_janela_1_fit_ENSO[,ENSO],start=1980)
  dados1_valida = janela_1_valida_Ventos
  dados2_valida = norm_janela_1_valida_ENSO[,ENSO]
  max_dados1 = max_Ventos_janela_1
  
  aux = PARX_melhor_p_m_cov(dados1,dados2,dados1_valida,max_dados1,dados2_valida,max_p=6,max_m=11,num_cenarios_in=500)
  MAE_p_m_janela_1 = aux[[1]]
  
  write.csv2(MAE_p_m_janela_1,paste("PREVISAO_RESULTADOS//MAE_Janela_1_Sul_Modelo_",modelo,".csv",sep=""))
  
  #Buscando arquivo de erros para janela 1
  aux = read.csv2(paste("PREVISAO_RESULTADOS//MAE_Janela_1_Sul_Modelo_",modelo,".csv",sep=""))[,-1]
  
  
  #Calculando métricas para cada janela
  
  dados1_janela_7 = norm_janela_7_fit_Ventos
  
  dados2_janela_7 = window(norm_janela_7_fit_ENSO[,ENSO],start=1980)
  
  dados1_novo_janela_7 = window(dados1_janela_7,start=start(dados2_janela_7))
  
#  dados1_valida_janela_7 = janela_7_valida_Ventos
  
  dados2_valida_janela_7 = norm_janela_7_valida_ENSO[,ENSO]
  
  janela_7 = cenarios_parX_LogNormal_cov(dados1_novo_janela_7,dados2_janela_7,dados2_valida_janela_7,max_p=aux[,1],max_m=aux[,2],num_cenarios=n_sim)
  
  cenarios_aux <- cenarios_tratamento_aux(janela_7[[1]],n_sim)
  PARX_Cov_cenarios[,3] <- sort(cenarios_aux[,1])
  PARX_Cov_cenarios[,4] <- cenarios_aux[,2]
  
  
  for(usina in 1:2){
    
    prev_7 = colMeans(janela_7[[usina]])*max_Ventos_janela_7[usina]
    PARX_Cov_previsao[,usina] <- prev_7 
    
    cenarios_final <- cenarios_tratamento_final(janela_7[[usina]],max_Ventos_janela_7[usina])
    PARX_Cov_cenarios[,usina] <- cenarios_final
    
    print(usina)
    
    if(usina == 2){
      PARX_Cov_previsao <- data.frame(PARX_Cov_previsao)
      PARX_Cov_previsao$mes <-  rep(1:12,1)
      write.xlsx(PARX_Cov_previsao,paste("PREVISAO_RESULTADOS//PARX_Cov_Previsão_Janela_7_Sul_Modelo_",modelo,".xlsx",sep=""))
      
      PARX_Cov_cenarios <- data.frame(PARX_Cov_cenarios)
      write.xlsx(PARX_Cov_cenarios,paste("PREVISAO_RESULTADOS//PARX_Cov_Cenários_Janela_7_Sul_Modelo_",modelo,".xlsx",sep=""))
    }
    
  }
  
  modelo = modelo + 1
  
}
# 












# Chamando bases
# Janela 6
Data <- c(1:12)
  
Validacao <- read_excel("PREVISAO_RESULTADOS/Validação_Janela_6.xlsx")
PAR <- read_excel("PREVISAO_RESULTADOS/PAR_Janela_6_Modelo_1.xlsx")
  
PARX_ONI_ACUM <- read_excel("PREVISAO_RESULTADOS/PARX_Janela_6_Modelo_2.xlsx")
PARX_NINO4_ACUM <- read_excel("PREVISAO_RESULTADOS/PARX_Janela_6_Modelo_3.xlsx")
  
PARX_Cov_ONI_ACUM_Nordeste <- read_excel("PREVISAO_RESULTADOS/PARX_Cov_Janela_6_Nordeste_Modelo_6.xlsx")
PARX_Cov_SOI_Nordeste <- read_excel("PREVISAO_RESULTADOS/PARX_Cov_Janela_6_Nordeste_Modelo_7.xlsx")
  
PARX_Cov_ONI_ACUM_Sul <- read_excel("PREVISAO_RESULTADOS/PARX_Cov_Janela_6_Sul_Modelo_10.xlsx")
  

# Organizando as bases
Previsao_Alagoas      <-        cbind(Data, Validacao[,1], PAR[,1], PARX_ONI_ACUM[,1])
Previsao_Paraiba      <-        cbind(Data, Validacao[,2], PAR[,2], PARX_NINO4_ACUM[,2])
Previsao_Pernambuco   <-        cbind(Data, Validacao[,3], PAR[,3], PARX_Cov_ONI_ACUM_Nordeste[,3])
Previsao_RGNorte      <-        cbind(Data, Validacao[,4], PAR[,4], PARX_Cov_SOI_Nordeste[,4])
Previsao_RGSul        <-        cbind(Data, Validacao[,5], PAR[,5], PARX_Cov_ONI_ACUM_Sul[,1])
Previsao_SC           <-        cbind(Data, Validacao[,6], PAR[,6], PARX_Cov_ONI_ACUM_Sul[,2])
Previsao_Sergipe      <-        cbind(Data, Validacao[,7], PAR[,7], PARX_ONI_ACUM[,7])
  
  
colnames(Previsao_Alagoas) = colnames(Previsao_Paraiba) = colnames(Previsao_Pernambuco) = colnames(Previsao_RGNorte) =  
 colnames(Previsao_RGSul) = colnames(Previsao_SC) = colnames(Previsao_Sergipe) =  c("Data","Observado","PAR","Melhor")
#




# Plotando os Gráficos
# Alagoas
g1 <- ggplot() + 
  
  geom_line(data = Previsao_Alagoas, aes(x = Data, y = Observado), color = "black", size = 0.5, lty = 2) +
  geom_line(data = Previsao_Alagoas, aes(x = Data, y = PAR), color = "red", size = 0.75) +
  geom_line(data = Previsao_Alagoas, aes(x = Data, y = Melhor), color = "darkblue", size = 0.75) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/23","May/23","Aug/23","Nov/23")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/23","Maio/23","Ago/23","Nov/23")) +
  
  ggtitle("Alagoas - PARX + CUM ONI") +
  # ggtitle("Alagoas - PARX + ONI ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
    ) 
  
  # theme(axis.text = element_text(size=10),
  #       axis.title = element_text(size=10),
  #       legend.text = element_text(size=8),
  #       plot.title = element_text(hjust=0.5,size=10),
  #       legend.position = "none") +
  
 
    



# Paraiba
g2 <- ggplot() + 
  
  geom_line(data = Previsao_Paraiba, aes(x = Data, y = Observado), color = "black", size = 0.5, lty = 2) +
  geom_line(data = Previsao_Paraiba, aes(x = Data, y = PAR), color = "red", size = 0.75) +
  geom_line(data = Previsao_Paraiba, aes(x = Data, y = Melhor), color = "darkblue", size = 0.75) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/23","May/23","Aug/23","Nov/23")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/23","Maio/23","Ago/23","Nov/23")) +
  
  ggtitle("Paraíba - PARX + CUM NINO4") +
  # ggtitle("Paraíba - PARX + NINO4 ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) 







# Pernambuco
g3 <- ggplot() + 
  
  geom_line(data = Previsao_Pernambuco, aes(x = Data, y = Observado), color = "black", size = 0.5, lty = 2) +
  geom_line(data = Previsao_Pernambuco, aes(x = Data, y = PAR), color = "red", size = 0.75) +
  geom_line(data = Previsao_Pernambuco, aes(x = Data, y = Melhor), color = "darkblue", size = 0.75) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/23","May/23","Aug/23","Nov/23")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/23","Maio/23","Ago/23","Nov/23")) +
  
  ggtitle("Pernambuco - PARX-Cov + CUM ONI") +
  # ggtitle("Pernambuco - PARX-Cov + ONI ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) 








# Rio Grande do Norte
g4 <- ggplot() + 
  
  geom_line(data = Previsao_RGNorte, aes(x = Data, y = Observado), color = "black", size = 0.5, lty = 2) +
  geom_line(data = Previsao_RGNorte, aes(x = Data, y = PAR), color = "red", size = 0.75) +
  geom_line(data = Previsao_RGNorte, aes(x = Data, y = Melhor), color = "darkblue", size = 0.75) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/23","May/23","Aug/23","Nov/23")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/23","Maio/23","Ago/23","Nov/23")) +
  
  ggtitle("Rio Grande do Norte - PARX-Cov + SOI") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) 


# # Exemplo_Simulação Doutorado
# setwd("~/Rafael_Estudo/PUC/Dissertação/Doutorado/Etapas")
# ggplot() + 
#   
#   geom_line(data = Previsao_RGNorte, aes(x = Data, y = Observado), color = "black", size = 0.5, lty = 2) +
#   geom_line(data = Previsao_RGNorte, aes(x = Data, y = Melhor), color = "darkblue", size = 0.75) +
#   
#   scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/23","Maio/23","Ago/23","Nov/23")) +
#   
#   ggtitle("") +
#   ylab("Velocidade do Vento (m/s)") + xlab("") +
#   
#   theme_classic()
# 
# 
# ggsave("Exemplo_Simulação.jpeg", width = 15, height = 8, dpi = 300, units = "cm")




# Rio Grande do Sul
g5 <- ggplot() + 
  
  geom_line(data = Previsao_RGSul, aes(x = Data, y = Observado), color = "black", size = 0.5, lty = 2) +
  geom_line(data = Previsao_RGSul, aes(x = Data, y = PAR), color = "red", size = 0.75) +
  geom_line(data = Previsao_RGSul, aes(x = Data, y = Melhor), color = "darkblue", size = 0.75) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/23","May/23","Aug/23","Nov/23")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/23","Maio/23","Ago/23","Nov/23")) +
  
  ggtitle("Rio Grande do Sul - PARX-Cov + CUM ONI") +
  # ggtitle("Rio Grande do Sul - PARX-Cov + ONI ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) 






# Santa Catarina
g6 <- ggplot() + 
  
  geom_line(data = Previsao_SC, aes(x = Data, y = Observado), color = "black", size = 0.5, lty = 2) +
  geom_line(data = Previsao_SC, aes(x = Data, y = PAR), color = "red", size = 0.75) +
  geom_line(data = Previsao_SC, aes(x = Data, y = Melhor), color = "darkblue", size = 0.75) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/23","May/23","Aug/23","Nov/23")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/23","Maio/23","Ago/23","Nov/23")) +
  
  ggtitle("Santa Catarina - PARX-Cov + CUM ONI") +
  # ggtitle("Santa Catarina - PARX-Cov + ONI ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) 





# Sergipe
g7 <- ggplot() + 
  
  geom_line(data = Previsao_Sergipe, aes(x = Data, y = Observado), color = "black", size = 0.5, lty = 2) +
  geom_line(data = Previsao_Sergipe, aes(x = Data, y = PAR), color = "red", size = 0.75) +
  geom_line(data = Previsao_Sergipe, aes(x = Data, y = Melhor), color = "darkblue", size = 0.75) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/23","May/23","Aug/23","Nov/23")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/23","Maio/23","Ago/23","Nov/23")) +
  
  ggtitle("Sergipe - PARX + CUM ONI") +
  # ggtitle("Sergipe - PARX + ONI ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) 







# Juntando
source("multiplot.R")

pdf("RESULTADOS_GRAFICOS//Artigo//Previsão_Geral_Janela_6.pdf",height=14,width=18)
# pdf("RESULTADOS_GRAFICOS//Previsão_Geral_Janela_6.pdf",height=14,width=18)

multiplot(g1,g2,g3,g4,g5,g6,g7,cols=2)   

dev.off()








# Janela 7
# Chamando bases
# PAR
PAR <- read_excel("PREVISAO_RESULTADOS/PAR_Janela_7_Modelo_100.xlsx")

# PARX
PARX_Pre_ONI_ACUM <- read_excel("PREVISAO_RESULTADOS/PARX_Previsão_Janela_7_Modelo_4.xlsx")
PARX_Cen_ONI_ACUM <- read_excel("PREVISAO_RESULTADOS/PARX_Cenários_Janela_7_Modelo_4.xlsx")

PARX_Pre_NINO4_ACUM <- read_excel("PREVISAO_RESULTADOS/PARX_Previsão_Janela_7_Modelo_5.xlsx")
PARX_Cen_NINO4_ACUM <- read_excel("PREVISAO_RESULTADOS/PARX_Cenários_Janela_7_Modelo_5.xlsx")


# PARX-Cov Nordeste
PARX_Cov_Pre_ONI_ACUM_Nordeste <- read_excel("PREVISAO_RESULTADOS/PARX_Cov_Previsão_Janela_7_Nordeste_Modelo_8.xlsx")
PARX_Cov_Cen_ONI_ACUM_Nordeste <- read_excel("PREVISAO_RESULTADOS/PARX_Cov_Cenários_Janela_7_Nordeste_Modelo_8.xlsx")

PARX_Cov_Pre_SOI_Nordeste <- read_excel("PREVISAO_RESULTADOS/PARX_Cov_Previsão_Janela_7_Nordeste_Modelo_9.xlsx")
PARX_Cov_Cen_SOI_Nordeste <- read_excel("PREVISAO_RESULTADOS/PARX_Cov_Cenários_Janela_7_Nordeste_Modelo_9.xlsx")


# PARX-Cov Sul
PARX_Cov_Pre_ONI_ACUM_Sul <- read_excel("PREVISAO_RESULTADOS/PARX_Cov_Previsão_Janela_7_Sul_Modelo_11.xlsx")
PARX_Cov_Cen_ONI_ACUM_Sul <- read_excel("PREVISAO_RESULTADOS/PARX_Cov_Cenários_Janela_7_Sul_Modelo_11.xlsx")



# Organizando bases
PAR_Alagoas        <-        PAR[,c(8,1)]
Cen_Alagoas        <-        PARX_Cen_ONI_ACUM[,c(8,9,1)]
Pre_Alagoas        <-        PARX_Pre_ONI_ACUM[,c(8,1)]

PAR_Paraiba        <-        PAR[,c(8,2)]
Cen_Paraiba        <-        PARX_Cen_NINO4_ACUM[,c(8,9,2)]
Pre_Paraiba        <-        PARX_Pre_NINO4_ACUM[,c(8,2)]

PAR_Pernambuco     <-        PAR[,c(8,3)]
Cen_Pernambuco     <-        PARX_Cov_Cen_ONI_ACUM_Nordeste[,c(6,7,3)]
Pre_Pernambuco     <-        PARX_Cov_Pre_ONI_ACUM_Nordeste[,c(6,3)]

PAR_RGNorte        <-        PAR[,c(8,4)]
Cen_RGNorte        <-        PARX_Cov_Cen_SOI_Nordeste[,c(6,7,4)]
Pre_RGNorte        <-        PARX_Cov_Pre_SOI_Nordeste[,c(6,4)]

PAR_RGSul          <-        PAR[,c(8,5)]
Cen_RGSul          <-        PARX_Cov_Cen_ONI_ACUM_Sul[,c(3,4,1)]
Pre_RGSul          <-        PARX_Cov_Pre_ONI_ACUM_Sul[,c(3,1)]

PAR_SC             <-        PAR[,c(8,6)]
Cen_SC             <-        PARX_Cov_Cen_ONI_ACUM_Sul[,c(3,4,2)]
Pre_SC             <-        PARX_Cov_Pre_ONI_ACUM_Sul[,c(3,2)]

PAR_Sergipe        <-        PAR[,c(8,7)]
Cen_Sergipe        <-        PARX_Cen_ONI_ACUM[,c(8,9,7)]
Pre_Sergipe        <-        PARX_Pre_ONI_ACUM[,c(8,7)]



colnames(PAR_Alagoas) = colnames(PAR_Paraiba) = colnames(PAR_Pernambuco) = colnames(PAR_RGNorte) =  
  colnames(PAR_RGSul) = colnames(PAR_SC) = colnames(PAR_Sergipe) = c("Mês","Vento_Média_PAR")

colnames(Cen_Alagoas) = colnames(Cen_Paraiba) = colnames(Cen_Pernambuco) = colnames(Cen_RGNorte) =  
  colnames(Cen_RGSul) = colnames(Cen_SC) = colnames(Cen_Sergipe) = c("Cenário","Mês","Vento")

colnames(Pre_Alagoas) = colnames(Pre_Paraiba) = colnames(Pre_Pernambuco) = colnames(Pre_RGNorte) =  
  colnames(Pre_RGSul) = colnames(Pre_SC) = colnames(Pre_Sergipe) = c("Mês","Vento_Média")
#





# Plotando os Gráficos
# Alagoas
g1 <- ggplot() + 
  
  geom_line(data = Cen_Alagoas, aes(x = Mês, y = Vento, group = as.factor(Cenário)), color = "lightskyblue", size = 0.25) +
  geom_line(data = PAR_Alagoas, aes(x = Mês, y = Vento_Média_PAR), color = "red", size = 1) +
  geom_line(data = Pre_Alagoas, aes(x = Mês, y = Vento_Média), color = "darkblue", size = 1) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/24","May/24","Aug/24","Nov/24")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/24","Maio/24","Ago/24","Nov/24")) +
  
  ggtitle("Alagoas - PARX + CUM ONI") +
  # ggtitle("Alagoas - PARX + ONI ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) 







# Paraiba
g2 <- ggplot() + 
  
  geom_line(data = Cen_Paraiba, aes(x = Mês, y = Vento, group = as.factor(Cenário)), color = "lightskyblue", size = 0.5) +
  geom_line(data = PAR_Paraiba, aes(x = Mês, y = Vento_Média_PAR), color = "red", size = 1) +
  geom_line(data = Pre_Paraiba, aes(x = Mês, y = Vento_Média), color = "darkblue", size = 1) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/24","May/24","Aug/24","Nov/24")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/24","Maio/24","Ago/24","Nov/24")) +
  
  ggtitle("Paraíba - PARX + CUM NINO4") +
  # ggtitle("Paraíba - PARX + NINO4 ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  )  





# Pernambuco
g3 <- ggplot() + 
  
  geom_line(data = Cen_Pernambuco, aes(x = Mês, y = Vento, group = as.factor(Cenário)), color = "lightskyblue", size = 0.5) +
  geom_line(data = PAR_Pernambuco, aes(x = Mês, y = Vento_Média_PAR), color = "red", size = 1) +
  geom_line(data = Pre_Pernambuco, aes(x = Mês, y = Vento_Média), color = "darkblue", size = 1) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/24","May/24","Aug/24","Nov/24")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/24","Maio/24","Ago/24","Nov/24")) +
  
  ggtitle("Pernambuco - PARX-Cov + CUM ONI") +
  # ggtitle("Pernambuco - PARX-Cov + ONI ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  ) 







# Rio Grande do Norte
g4 <- ggplot() + 
  
  geom_line(data = Cen_RGNorte, aes(x = Mês, y = Vento, group = as.factor(Cenário)), color = "lightskyblue", size = 0.5) +
  geom_line(data = PAR_RGNorte, aes(x = Mês, y = Vento_Média_PAR), color = "red", size = 1) +
  geom_line(data = Pre_RGNorte, aes(x = Mês, y = Vento_Média), color = "darkblue", size = 1) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/24","May/24","Aug/24","Nov/24")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/24","Maio/24","Ago/24","Nov/24")) +
  
  ggtitle("Rio Grande do Norte - PARX-Cov + SOI") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  )







# Rio Grande do Sul
g5 <- ggplot() + 
  
  geom_line(data = Cen_RGSul, aes(x = Mês, y = Vento, group = as.factor(Cenário)), color = "lightskyblue", size = 0.5) +
  geom_line(data = PAR_RGSul, aes(x = Mês, y = Vento_Média_PAR), color = "red", size = 1) +
  geom_line(data = Pre_RGSul, aes(x = Mês, y = Vento_Média), color = "darkblue", size = 1) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/24","May/24","Aug/24","Nov/24")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/24","Maio/24","Ago/24","Nov/24")) +
  
  ggtitle("Rio Grande do Sul - PARX-Cov + CUM ONI") +
  # ggtitle("Rio Grande do Sul - PARX-Cov + ONI ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  )





# Santa Catarina
g6 <- ggplot() + 
  
  geom_line(data = Cen_SC, aes(x = Mês, y = Vento, group = as.factor(Cenário)), color = "lightskyblue", size = 0.5) +
  geom_line(data = PAR_SC, aes(x = Mês, y = Vento_Média_PAR), color = "red", size = 1) +
  geom_line(data = Pre_SC, aes(x = Mês, y = Vento_Média), color = "darkblue", size = 1) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/24","May/24","Aug/24","Nov/24")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/24","Maio/24","Ago/24","Nov/24")) +
  
  ggtitle("Santa Catarina - PARX-Cov + CUM ONI") +
  # ggtitle("Santa Catarina - PARX-Cov + ONI ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  )




# Sergipe
g7 <- ggplot() + 
  
  geom_line(data = Cen_Sergipe, aes(x = Mês, y = Vento, group = as.factor(Cenário)), color = "lightskyblue", size = 0.5) +
  geom_line(data = PAR_Sergipe, aes(x = Mês, y = Vento_Média_PAR), color = "red", size = 1) +
  geom_line(data = Pre_Sergipe, aes(x = Mês, y = Vento_Média), color = "darkblue", size = 1) +
  
  scale_x_continuous(breaks = c(2,5,8,11), labels = c("Feb/24","May/24","Aug/24","Nov/24")) +
  # scale_x_continuous(breaks = c(2,5,8,11), labels = c("Fev/24","Maio/24","Ago/24","Nov/24")) +
  
  ggtitle("Sergipe - PARX + CUM ONI") +
  # ggtitle("Sergipe - PARX + ONI ACUM") +
  
  ylab("Wind Speed (m/s)") + xlab("") +
  # ylab("Velocidade do Vento (m/s)") + xlab("") +
  
  theme_classic() +
  
  theme(
    plot.title = element_text(size = 21), 
    axis.text.x = element_text(size = 18), 
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 15),
    legend.position = "none"
  )




# Juntando
source("multiplot.R")

pdf("RESULTADOS_GRAFICOS//Artigo//Previsão_Geral_Janela_7.pdf",height=14,width=18)
# pdf("RESULTADOS_GRAFICOS//Previsão_Geral_Janela_7.pdf",height=14,width=18)

multiplot(g1,g2,g3,g4,g5,g6,g7,cols=2)   

dev.off()















# Diretório
setwd("~/Rafael_Estudo/PUC/Dissertação/Resultados/Previsões ENSO/Regressão")

# Pacotes
{
library(rstan)
library(readxl)
library(tseries)
library(seastests)
library(ggplot2)
library(fpp3)
library(tibble)
library(tidyverse)
library(tsibbledata)
library(fable)
library(tsibble)
library(feasts)
library(dplyr)
library(lubridate)
library(lmtest)
library(car)
library(gridExtra)
library(grid)
library(readr)
library(openxlsx)
library(reshape2)
library(here)
}


# Modelos de Regressao ENSO
dados_ENSO <- read_excel("ENSO_Anomalias.xlsx")
dados_ENSO <- dados_ENSO[c(1:1107),]

dados_ENSO = dados_ENSO %>%
  mutate(Data = yearmonth(Data)) %>% 
  select(c(1:length(dados_ENSO)))%>%
  as_tsibble(
    index = Data)

indice_ENSO <- colnames(dados_ENSO)[-1]

colnames(dados_ENSO) <- c("Data","SOI","EQSOI","NINO12","NINO3","NINO4","NINO34","ONI")
variaveis_ENSO <- colnames(dados_ENSO)[-1]

lista_modelos_ENSO <- list()


for (indice in 1:6) {
  
#indice=1
  
  #Selecionar os dados do ONI com cada indice
  dados <- dados_ENSO[,c(1,indice+1,8)]
  dados <- dados[which(dados[,length(dados)]!=""),]
  dados <- dados[which(dados[,length(dados)-1]!=""),]
  
  formula_string <- paste(variaveis_ENSO[indice], "~", variaveis_ENSO[7])
  formula <- as.formula(formula_string)
  
  #Estimar o modelo
  fit_dados <- dados %>%
    as_tsibble(index = Data) %>%
    model(lm = TSLM(formula))

  tabela <- coefficients(fit_dados)[,-c(1,5)]
  tabela$estimate <- round(tabela$estimate,3)
  tabela$std.error <- round(tabela$std.error,3)
  tabela$p.value <- round(tabela$p.value,3)
  tabela$R2 <- round(glance(fit_dados)$r_squared,3)
  tabela$Indices <- indice_ENSO[indice]
  tabela <- select(tabela, Indices, everything())
  colnames(tabela) <- c("Índice","Coeficientes","Valor Estimado","Desvio Padrão","P-valor","R²")
  lista_modelos_ENSO[[indice]] <- tabela
  
  # Criar um dataframe com as datas e os valores ajustados pelo modelo
  df_grafico <- data.frame(
    Data = dados$Data,
    Observado = dados[,length(dados)-1], 
    Ajustado = fitted(fit_dados)$.fitted  # Valores ajustados pelo modelo TSLM
  )
  
  names(df_grafico)[2] <- "Observado"
  
  
  # Gráfico de valores observados junto aos ajustados
  
  ggplot(df_grafico, aes(x = Data)) +
    geom_line(aes(y = Observado, color = "Observado"), size = 0.8) +
    geom_line(aes(y = Ajustado, color = "Ajustado"), size = 1, linetype = "dashed") +
    scale_color_manual(values = c("Observado" = "darkblue", "Ajustado" = "aquamarine4")) +
    labs(title = indice_ENSO[indice],
         x = element_blank(),
         y = "Anomalia") +
    theme_minimal()+
    theme(legend.title=element_blank(),
          legend.position = "bottom")
  
  ggsave(paste0("Ajuste//2023//Sobreposto//",indice_ENSO[indice],".jpeg"), width = 15, height = 8, dpi = 300, units = "cm")
  
  
  # Gráfico de Dispersão com Reta Diagonal de valores Observado x Ajustado
  
  ggplot(df_grafico, aes(x = Observado, y = Ajustado)) +
    geom_point(color = "darkblue") +  # Adicionar os pontos de dispersão
    geom_abline(color = "aquamarine4", linewidth = 0.75) +  # Adicionar a reta diagonal
    labs(x = "Observado",
         y = "Ajustado",
         title = paste(indice_ENSO[indice],"- Observado x Ajustado"),
         subtitle = paste("R² = ", lista_modelos_ENSO[[indice]][1,6])) 

  ggsave(paste0("Ajuste//2023//Dispersão//",indice_ENSO[indice],".jpeg"), height=8.27,width=11.69)
  
  
  # Dispersão pro Overleaf
  
  plot1 <- ggplot(df_grafico, aes(x = Observado, y = Ajustado)) +
    geom_point(color = "darkblue") +  # Adicionar os pontos de dispersão
    geom_abline(color = "aquamarine4", linewidth = 0.75) +  # Adicionar a reta diagonal
    labs(x = "Observado",
         y = "Ajustado",
         title = paste(indice_ENSO[indice], "- R² = ", lista_modelos_ENSO[[indice]][1,6])) +
    
    theme(
      plot.title = element_text(size = 15),   
      axis.text.x = element_text(size = 15), 
      axis.text.y = element_text(size = 15),
      axis.title.x = element_text(size = 15),  
      axis.title.y = element_text(size = 15)
    )
  
  
  plot1
  
  if(indice == 1){plot_aux = list(plot1)} else {plot_aux=c(plot_aux,list(plot1))}
  
  ggsave(paste0("Ajuste//2023//Dispersão//Overleaf//",indice_ENSO[indice],".pdf"), height=14,width=18)
  
  
}


# Juntando os gráficos
source("multiplot.R")

pdf("Ajuste//2023//Dispersão//Overleaf//Ajuste_ENSO_2023_Geral.pdf",height=18,width=18)

multiplot(plot_aux[[1]],plot_aux[[2]],plot_aux[[3]],plot_aux[[4]],plot_aux[[5]],
          plot_aux[[6]],cols=2)                                           

dev.off()


# Tabela Ajuste
tabela_final <- rbind(lista_modelos_ENSO[[1]],lista_modelos_ENSO[[2]],
                      lista_modelos_ENSO[[3]],lista_modelos_ENSO[[4]],
                      lista_modelos_ENSO[[5]],lista_modelos_ENSO[[6]])

write.xlsx(tabela_final, "Ajuste//2023//Modelos.xlsx")
#

















# Reconstrução ENSO
dados_ENSO <- read_excel("ENSO_Anomalias.xlsx")

ONI_At <- c(0.038,0.261,0.493,0.704,0.779,0.762,0.772,0.724,0.730)                

SOI_At <- EqSOI_At <- NINO12_At <- NINO3_At <- NINO4_At <- NINO34_At <- c(rep(NA,9))

ENSO_At <- cbind(dados_ENSO[c(1108:1116),1],SOI_At,EqSOI_At,NINO12_At,NINO3_At,NINO4_At,NINO34_At,ONI_At)

colnames(ENSO_At) <- colnames(dados_ENSO)

dados_ENSO <- rbind(dados_ENSO[c(1:1107),],ENSO_At)

dados_ENSO = dados_ENSO %>%
  mutate(Data = yearmonth(Data)) %>% 
  select(c(1:length(dados_ENSO)))%>%
  as_tsibble(
    index = Data)

indice_ENSO <- colnames(dados_ENSO)[-1]

colnames(dados_ENSO) <- c("Data","SOI","EQSOI","NINO12","NINO3","NINO4","NINO34","ONI")
variaveis_ENSO <- colnames(dados_ENSO)[-1]

modelos <- read_excel("Ajuste/2023/Modelos.xlsx")

lista_reconstrucao_ENSO <- list()


for (indice in 1:6) {
  
  #indice=1
  dados <- dados_ENSO[,c(1,indice+1,8)]
  
  # Base com elementos a serem previstos
  reconstrucao <- dados[c(1108:1116),]
  
  for (i in 1:9) {
    #i=1
    reconstrucao[i,2] <- modelos[(2*indice-1),3] + 
                        (modelos[(2*indice),3] * reconstrucao[i,3])     
  }
  
  #Salvando estatisticas (dados observados e reconstruidos)
  observado <- dados[-c(1108:1116),c(1,2)]
  observado <- observado[which(observado[,length(observado)]!=""),]

  resumo_completo <- data.frame(
    Tipo = character(0),
    Mínimo = numeric(0),
    'Primeiro Quartil' = numeric(0),
    Mediana = numeric(0),
    Média = numeric(0),
    'Terceiro Quartil' = numeric(0),
    Máximo = numeric(0)
  )
  
  resumo_completo[1,1]<-"Histórico"
  resumo_completo[2,1]<-"Previsão"
  
  extrair_valores_estatisticas <- function(resumo) {
    padrao <- "\\b[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?\\b"
    correspondencias <- regmatches(resumo, gregexpr(padrao, resumo))
    
    valores <- sapply(correspondencias, function(correspondencia) {
      as.numeric(correspondencia)
    })
    
    return(valores)
  }
  
  valores_estatisticas_observado <- extrair_valores_estatisticas(summary(observado[,2]))
  valores_estatisticas_reconstruido <- extrair_valores_estatisticas(summary(reconstrucao[,2]))
  
  for (i in 2:length(resumo_completo)) {
    resumo_completo[1,i] <- valores_estatisticas_observado[i-1]
    resumo_completo[2,i] <- valores_estatisticas_reconstruido[i-1]
  }
  
  write.xlsx(resumo_completo, paste0("Previsão//2023//Resumo//",indice_ENSO[indice],".xlsx"))
  
  
  # Salvando serie reconstruida 
  lista_reconstrucao_ENSO[[indice]] <- reconstrucao
  
  # Plotar grafico com historico reconstruido + historico observado
  # Adicione uma coluna "Serie" para indicar a série
  observado$Serie <- "Histórico"
  
  reconst <- reconstrucao[,c(1,2)]
  reconst$Serie <- "Previsão"
  
  
  # União dos dados (Novidade)
  ultimo_mes <- observado %>%
    filter(year(Data) == max(year(Data))) %>%
    filter(month(Data) == max(month(Data)))
  ultimo_mes$Serie = "Previsão"
  
  
  # Combine os dataframes
  dados_combinados <- tsibble(bind_rows(observado, reconst), key = Serie)
  dados_combinados <- dados_combinados[c((length(dados_combinados$Data)-167):length(dados_combinados$Data)),] 
  y <- colnames(dados_combinados)[2]
  
  tsibble_completo <- bind_rows(dados_combinados, tsibble(ultimo_mes, key = Serie))
  tsibble_completo$Data_aux <- c(1:159,159,160:168)
  
  
# Gráfico Previsão
  
  ggplot(tsibble_completo, aes(x = Data_aux, color = Serie, linetype = Serie)) +
    
    geom_line(aes_string(y = y), size = ifelse(tsibble_completo$Serie == "Previsão", 1, 0.5)) +
    
    scale_color_manual(values = c("Previsão" = "tomato", "Histórico" = "darkblue")) +
    
    scale_linetype_manual(values = c("Previsão" = "solid", "Histórico" = "solid"),
                          guide = guide_legend(override.aes = list(size = c(0.7, 0.7))))+
    
    scale_x_continuous(breaks = c(1,73,157), labels = c("Jan/2010","Jan/2016","Jan/2023")) +
    
    labs(title = paste0(indice_ENSO[indice]),
         x = element_blank(),
         y = "Anomalia") +
    
    theme_minimal() +
    
    theme(legend.title=element_blank(),
          legend.position = "bottom")
    
  ggsave(paste0("Previsão//2023//",indice_ENSO[indice],".jpeg"), height=8.27,width=11.69)
  
  
  
  # Previsão pro Overleaf
  
  plot1 = ggplot(tsibble_completo, aes(x = Data_aux, color = Serie, linetype = Serie)) +
      
    geom_line(aes_string(y = y), size = ifelse(tsibble_completo$Serie == "Previsão", 1, 0.5)) +
      
    scale_color_manual(values = c("Previsão" = "tomato", "Histórico" = "darkblue")) +
      
    scale_linetype_manual(values = c("Previsão" = "solid", "Histórico" = "solid"),
                          guide = guide_legend(override.aes = list(size = c(0.7, 0.7))))+
      
    scale_x_continuous(breaks = c(1,73,157), labels = c("Jan/2010","Jan/2016","Jan/2023")) +
      
    labs(x = element_blank(),
         y = "Anomalia") +
      
    theme_minimal() +
      
      theme(
        plot.title = element_text(size = 15),
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 15),  
        legend.text = element_text(size = 12),
        legend.key.size = unit(1.5, "lines"),
        legend.title = element_blank(),
        legend.position = "bottom"
      )
    
  
  plot1
  
  if(indice == 1){plot_aux = list(plot1)} else {plot_aux=c(plot_aux,list(plot1))}
  
  
  ggsave(paste0("Previsão//2023//Overleaf//",indice_ENSO[indice],".pdf"), height=14,width=18)
  
}



# Juntando os gráficos
source("multiplot.R")

pdf("Previsão//2023//Overleaf//Previsão_ENSO_2023_Geral.pdf",height=14,width=18)

multiplot(plot_aux[[1]],plot_aux[[2]],plot_aux[[3]],plot_aux[[4]],plot_aux[[5]],
          plot_aux[[6]],cols=2)                                           

dev.off()





# Novas Bases
reconstrucao_ENSO_final <- cbind(data.frame(lista_reconstrucao_ENSO[[1]][,c(1,2)]),
                                 data.frame(lista_reconstrucao_ENSO[[2]][,2]),
                                 data.frame(lista_reconstrucao_ENSO[[3]][,2]),
                                 data.frame(lista_reconstrucao_ENSO[[4]][,2]),
                                 data.frame(lista_reconstrucao_ENSO[[5]][,2]),
                                 data.frame(lista_reconstrucao_ENSO[[6]][,c(2,3)]))

ENSO_final <- rbind(data.frame(dados_ENSO[c(1:1107),]),
                    reconstrucao_ENSO_final)

colnames(ENSO_final) <- colnames(read_excel("ENSO_Anomalias.xlsx"))


# Salvando as series reconstruidas 
write.xlsx(ENSO_final, "Previsão_ENSO_2023.xlsx")





# Introduzindo Anomalias Acumuladas
ENSO <- read_excel("Previsão_ENSO_2023.xlsx")

ENSO$SOI <- ifelse(is.na(ENSO$SOI)==T,0,ENSO$SOI)
ENSO$`SOI Equatorial` <- ifelse(is.na(ENSO$`SOI Equatorial`)==T,0,ENSO$`SOI Equatorial`)
ENSO$ONI <- ifelse(is.na(ENSO$ONI)==T,0,ENSO$ONI)

ENSO$AcumuladoSOI <- cumsum(ENSO$SOI)
ENSO$AcumuladoEqSOI <- cumsum(ENSO$`SOI Equatorial`)
ENSO$AcumuladoNINO12 <- cumsum(ENSO$`Niño 1+2`)
ENSO$AcumuladoNINO3 <- cumsum(ENSO$`Niño 3`)
ENSO$AcumuladoNINO4 <- cumsum(ENSO$`Niño 4`)
ENSO$AcumuladoNINO34 <- cumsum(ENSO$`Niño 3.4`)
ENSO$AcumuladoONI <- cumsum(ENSO$ONI)

ENSO$SOI[c(1:240)] <- NA
ENSO$AcumuladoSOI[c(1:240)] <- NA
ENSO$`SOI Equatorial`[c(1:216)] <- NA
ENSO$AcumuladoEqSOI[c(1:216)] <- NA
ENSO$ONI[c(1:228)] <- NA
ENSO$AcumuladoONI[c(1:228)] <- NA

colnames(ENSO) <- c("Data","SOI","SOI Equatorial","Niño 1+2","Niño 3","Niño 4","Niño 3.4","ONI",
                    "SOI Acumulado","SOI Equatorial Acumulado","Niño 1+2 Acumulado","Niño 3 Acumulado",
                    "Niño 4 Acumulado","Niño 3.4 Acumulado","ONI Acumulado")

write.xlsx(ENSO,"Previsão_ENSO_Completa_2023.xlsx")

















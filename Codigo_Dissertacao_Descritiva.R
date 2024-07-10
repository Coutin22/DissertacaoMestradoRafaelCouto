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
library(dplyr)
library(sf)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggmap)
library(viridis)
library(ggthemes)
}


# Formando a Base
# Alagoas <- read_excel("Alagoas.xlsx")
# Paraíba <- read_excel("Paraíba.xlsx")
# Pernambuco <- read_excel("Pernambuco.xlsx")
# RGNorte <- read_excel("Rio_Grande_do_Norte.xlsx")
# RGSul <- read_excel("Rio_Grande_do_Sul.xlsx")
# SCatarina <- read_excel("Santa_Catarina.xlsx")
# Sergipe <- read_excel("Sergipe.xlsx")
# 
# Ventos <- cbind(Alagoas,Paraíba[,2],Pernambuco[,2],RGNorte[,2],RGSul[,2],SCatarina[,2],Sergipe[,2])
# 
# colnames(Ventos) <- c("Data","Alagoas","Paraíba","Pernambuco","Rio Grande do Norte",
#                       "Rio Grande do Sul","Santa Catarina","Sergipe")
# 
# write.xlsx(Ventos,"Series_Ventos_Tese.xlsx")


 

# Mapa dos Estados
# brasil <- st_read(dsn = "Outros/Mapa/BR_UF_2020.shp")
# brasil <- brasil[,c(2,5)]
# 
# 
# pdf("RESULTADOS_GRAFICOS//Mapa_Estados.pdf",height=8.27,width=11.69)
# 
# ggplot(data = brasil) +
#   geom_sf(fill=c("white","white","white","white","white","white","white","white","white","white",
#                        "#33CC33","#33CC33","#33CC33","#33CC33","#33CC33","white","white","white","white",
#                        "white","white","gold","gold","white","white","white","white")) +
#           theme_void()
# 
# dev.off()






# Analise Descritiva Vento
# Função de plotar vários graficos juntos
source("multiplot.R")

# Séries temporais Velocidade do Vento
Ventos <- ts(read_excel("Series_Ventos_Tese.xlsx")[,-1], start=1980, freq=12)

for(j in 1:ncol(Ventos)){
  plot1 = autoplot(Ventos[,j]) + 
    ggtitle(colnames(Ventos)[j]) +
    theme_minimal() +
    
    theme(
      plot.title = element_text(size = 23), 
      axis.text.x = element_text(size = 18), 
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),  
      axis.title.y = element_text(size = 15)
    ) +
  
    # theme(
    #   axis.text = element_text(size=10),
    #       axis.title = element_text(size=10),
    #       legend.text = element_text(size=8),
    #       plot.title = element_text(hjust=0.5,size=10),
    #       plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
    
    ylab("Wind Speed (m/s)") + xlab("")
  
    # ylab("Velocidade do Vento (m/s)") + xlab("")
  
  if(j == 1){plot_aux = list(plot1)} else {plot_aux=c(plot_aux,list(plot1))}
}


pdf("RESULTADOS_GRAFICOS//Artigo//Series_Temporais_Vento.pdf",height=14,width=18)
# pdf("RESULTADOS_GRAFICOS//Series_Temporais_Vento.pdf",height=14,width=18)

multiplot(plot_aux[[1]],plot_aux[[2]],plot_aux[[3]],plot_aux[[4]],plot_aux[[5]],
          plot_aux[[6]],plot_aux[[7]],cols=2)                                           

dev.off()






# Estatísticas descritivas
Media <- round(apply(Ventos,2,mean),2)
Mediana <- round(apply(Ventos,2,median),2)
Desvio.Padrao <- round(apply(Ventos,2,sd),2)
Coeficiente.de.Variacao <- round(apply(Ventos,2,sd)/apply(Ventos,2,mean),2)
Assimetria <- round(apply(Ventos,2,skewness),2)
Curtose <- round(apply(Ventos,2,kurtosis),2)
Descritiva <- as.data.frame(cbind(Media,Mediana,Desvio.Padrao,Coeficiente.de.Variacao,Assimetria,Curtose))
#xtable(Descritiva, caption = "Estatísticas Descritivas das Séries de Velocidade do Vento por Usina. Fonte: Elaborado pelo autores.")




# Autocorrelação Velocidade do Vento
for(j in 1:ncol(Ventos)){
  plot1 = ggAcf(Ventos[,j]) +
    ggtitle(colnames(Ventos)[j]) +
    theme_minimal() +
    
    theme(
      plot.title = element_text(size = 23), 
      axis.text.x = element_text(size = 18), 
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),  
      axis.title.y = element_text(size = 15)
    ) +
    
    # theme(
    #   axis.text = element_text(size=10),
    #       axis.title = element_text(size=10),
    #       legend.text = element_text(size=8),
    #       plot.title = element_text(hjust=0.5,size=10),
    #       plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
    
    xlab("Lag") +
    ylab("ACF")
  if(j == 1){plot_aux = list(plot1)}else{plot_aux=c(plot_aux,list(plot1))}
}

pdf("RESULTADOS_GRAFICOS//Autocorrelação_Vento.pdf",height=14,width=18)

multiplot(plot_aux[[1]],plot_aux[[2]],plot_aux[[3]],plot_aux[[4]],plot_aux[[5]],
          plot_aux[[6]],plot_aux[[7]],cols=2)                                           

dev.off()




# Teste de Estacionariedade
testes_estacionariedade = matrix(NA,ncol=3,nrow=7)
colnames(testes_estacionariedade) = c("ADF","PP","Estacionária?")
rownames(testes_estacionariedade) = colnames(Ventos)
for(j in 1:ncol(Ventos)){
  testes_estacionariedade[j,1]=round(adf.test(Ventos[,j])$p.value,3)
  testes_estacionariedade[j,2]=round(pp.test(Ventos[,j])$p.value,3)
  testes_estacionariedade[j,3]=ifelse(testes_estacionariedade[j,1] < 0.1 & 
                                        testes_estacionariedade[j,2] < 0.1 ,"Sim","Não")
}
#xtable(testes_estacionariedade, caption = "P-valor dos Testes de Estacionariedade ADF e PP. Fonte: Elaborado pelo autores.")






# Analise Descritiva ENSO
source("multiplot.R")

# Série Temporal
# Anomalias 
# SOI
# ENSO <- read_excel("ENSO_Completo.xlsx")[,c(1:3)]
# colnames(ENSO) <- c("Data","SOI","Equatorial SOI")

ENSO <- read_excel("ENSO_Completo.xlsx")[,c(1:3)]

for(i in 2:ncol(ENSO)){
  ENSO_Data <- as.data.frame(na.omit(ENSO[,c(1,i)]))
  ENSO_Data_aux <- ENSO_Data
  colnames(ENSO_Data_aux) <- c("Data","ENSO")
  
  plot1 = ENSO_Data_aux |> ggplot(mapping = aes(x = Data, y = ENSO)) +
    geom_line() + 
    geom_hline(yintercept = 0.5, color="blue") + 
    geom_hline(yintercept = -0.5, color="red") +
    
    # labs(x= "", y="Anomalie") +
    # ggtitle(paste(colnames(ENSO_Data)[2], "Anomalie")) +
    
    labs(x= "", y="Anomalia") +
    ggtitle(paste("Anomalia", colnames(ENSO_Data)[2])) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(size = 23), 
      axis.text.x = element_text(size = 18), 
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),  
      axis.title.y = element_text(size = 15)
    )
    
    # theme(
    #   axis.text = element_text(size=10),
    #       axis.title = element_text(size=10),
    #       legend.text = element_text(size=8),
    #       plot.title = element_text(hjust=0.5,size=10),
    #       plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
    
  if(i == 2){plot_aux1 = list(plot1)}else{plot_aux1 = c(plot_aux1,list(plot1))}
}

# SST
ENSO <- read_excel("ENSO_Completo.xlsx")[,c(1,4:8)]
for(i in 2:ncol(ENSO)){
  ENSO_Data <- as.data.frame(na.omit(ENSO[,c(1,i)]))
  ENSO_Data_aux <- ENSO_Data
  colnames(ENSO_Data_aux) <- c("Data","ENSO")
  plot1 = ENSO_Data_aux |> ggplot(mapping = aes(x = Data, y = ENSO)) +
    geom_line() + 
    geom_hline(yintercept = 0.5, color="red") + 
    geom_hline(yintercept = -0.5, color="blue") +
    
    # labs(x= "", y="Anomalie") +
    # ggtitle(paste(colnames(ENSO_Data)[2],"Anomalie")) +
    
    labs(x= "", y="Anomalia") +
    ggtitle(paste("Anomalia",colnames(ENSO_Data)[2])) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(size = 23), 
      axis.text.x = element_text(size = 18), 
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),  
      axis.title.y = element_text(size = 15)
    )
    
    # theme(
    #   axis.text = element_text(size=10),
    #       axis.title = element_text(size=10),
    #       legend.text = element_text(size=8),
    #       plot.title = element_text(hjust=0.5,size=10),
    #       plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
    
  if(i == 2){plot_aux2 = list(plot1)}else{plot_aux2 = c(plot_aux2,list(plot1))}
}


# pdf("RESULTADOS_GRAFICOS//Artigo//Series_Temporais_Anomalia_ENSO.pdf",height=14,width=18)
pdf("RESULTADOS_GRAFICOS//Series_Temporais_Anomalia_ENSO.pdf",height=14,width=18)

multiplot(plot_aux1[[1]],plot_aux1[[2]],plot_aux2[[1]],
          plot_aux2[[2]],plot_aux2[[3]],plot_aux2[[4]],
          plot_aux2[[5]],cols=2)
dev.off()







# Acumulado
source("multiplot.R")

# ENSO <- read_excel("ENSO_Completo.xlsx")[,c(1,9:15)]
# colnames(ENSO) <- c("Data","SOI","Equatorial SOI","Niño 1+2","Niño 3","Niño 4","Niño 3.4","ONI")

ENSO <- read_excel("ENSO_Completo.xlsx")[,c(1,9:15)]

for(i in 2:ncol(ENSO)){
  ENSO_Data <- as.data.frame(na.omit(ENSO[,c(1,i)]))
  ENSO_Data_aux <- ENSO_Data
  colnames(ENSO_Data_aux) <- c("Data","ENSO")
  plot1 = ENSO_Data_aux |> ggplot(mapping = aes(x = Data, y = ENSO)) +
    geom_line() + 
    
    # labs(x= "", y="Cumulative Anomalie") +
    # ggtitle(paste("Cumulative", colnames(ENSO_Data)[2])) +
    
    labs(x= "", y="Anomalia Acumulada") +
    ggtitle(colnames(ENSO_Data)[2]) +
    
    theme_minimal() +
    
    theme(
      plot.title = element_text(size = 23), 
      axis.text.x = element_text(size = 18), 
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),  
      axis.title.y = element_text(size = 15)
    ) 
    
    # theme(
    #   axis.text = element_text(size=10),
    #       axis.title = element_text(size=10),
    #       legend.text = element_text(size=8),
    #       plot.title = element_text(hjust=0.5,size=10),
    #       plot.margin = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt")) +
    
  if(i == 2){plot_aux = list(plot1)}else{plot_aux=c(plot_aux,list(plot1))}
}

# pdf("RESULTADOS_GRAFICOS//Artigo//Series_Temporais_ENSO_Acumulado.pdf",height=14,width=18)
pdf("RESULTADOS_GRAFICOS//Series_Temporais_ENSO_Acumulado.pdf",height=14,width=18)

multiplot(plot_aux[[1]],plot_aux[[2]],plot_aux[[3]],
          plot_aux[[4]],plot_aux[[5]],plot_aux[[6]],
          plot_aux[[7]],cols=2)

dev.off()










# Distruibuição da Velocidade do Vento por fase do ENSO - Todos os índices do ENSO alternando os Estados
Ventos <- read_excel("Series_Ventos_Tese.xlsx")[,-1]
ENSO <- read_excel("ENSO_Períodos.xlsx")[c(589:1116),c(3,5,7,9,11,13,15)]
colnames(ENSO) <- c("SOI","SOI Equatorial","Niño 1+2","Niño 3","Niño 4","Niño 3.4","ONI")
source("multiplot.R")

for(i in 1:ncol(ENSO)){
    Ventos_ENSO <- as.data.frame(na.omit(cbind(ENSO[,i],Ventos[,5])))            # Alterna os Estados "Ventos[,1]"
    Ventos_ENSO_aux <- Ventos_ENSO
    colnames(Ventos_ENSO_aux) <- c("ENSO","Ventos")
    plot1 = Ventos_ENSO_aux |> ggplot(mapping = aes(x = ENSO, y = Ventos, fill= ENSO)) +
      geom_boxplot() +  
      scale_fill_manual(values = c("#CC3333","#3366FF","grey50")) +
      ggtitle(paste(colnames(Ventos)[5],"vs",colnames(ENSO)[i])) +               # Alterna os Estados "colnames(Ventos)[1]"
      theme_minimal() +
      theme(axis.text = element_text(size=10),
            axis.title = element_text(size=10),
            legend.text = element_text(size=8),
            plot.title = element_text(hjust=0.5,size=10),
            legend.position = "none") +
      xlab("") +
      ylab("")
    if(i == 1){plot_aux = list(plot1)}else{plot_aux=c(plot_aux,list(plot1))}
}
  
multiplot(plot_aux[[1]],plot_aux[[2]],plot_aux[[3]],
            plot_aux[[4]],plot_aux[[5]],plot_aux[[6]],
            plot_aux[[7]],cols=2)

  



  
# Teste de Kruskal-Wallis
Ventos <- as.data.frame(read_excel("Series_Ventos_Tese.xlsx")[,-1])
ENSO <- as.data.frame(read_excel("ENSO_Períodos.xlsx")[c(589:1116),c(3,5,7,9,11,13,15)])
colnames(ENSO) <- c("SOI","SOI Equatorial","Niño 1+2","Niño 3","Niño 4","Niño 3.4","ONI")
  
kruskal.test(Ventos[,1],ENSO[,1])
round(kruskal.test(Ventos[,1],ENSO[,1])$p.value,2)
  
teste_kruskal_valores = matrix(NA,ncol=7,nrow=7)
teste_kruskal_respostas = matrix(NA,ncol=7,nrow=7)
colnames(teste_kruskal_valores) = colnames(ENSO);colnames(teste_kruskal_respostas) = colnames(ENSO)
rownames(teste_kruskal_valores) = colnames(Ventos);rownames(teste_kruskal_respostas) = colnames(Ventos)
  
for(i in 1:7){
  for(j in 1:7){
    teste_kruskal_valores[i,j] = round(kruskal.test(Ventos[,i],ENSO[,j])$p.value,3)
    teste_kruskal_respostas[i,j] = ifelse(round(kruskal.test(Ventos[,i],ENSO[,j])$p.value,3) >= 0.10,
                                          "Sim","Não")
  }
}
  
teste_kruskal_valores <- as.data.frame(teste_kruskal_valores)
teste_kruskal_respostas <- as.data.frame(teste_kruskal_respostas)  

teste_kruskal_juntos <- rbind(teste_kruskal_valores[1,],teste_kruskal_respostas[1,],
                              teste_kruskal_valores[2,],teste_kruskal_respostas[2,],
                              teste_kruskal_valores[3,],teste_kruskal_respostas[3,],
                              teste_kruskal_valores[4,],teste_kruskal_respostas[4,],
                              teste_kruskal_valores[5,],teste_kruskal_respostas[5,],
                              teste_kruskal_valores[6,],teste_kruskal_respostas[6,],
                              teste_kruskal_valores[7,],teste_kruskal_respostas[7,])


teste_kruskal_juntos$Estado <- c("Alagoas","Alagoas","Paraíba","Paraíba","Pernambuco","Pernambuco",
                                 "Rio Grande do Norte","Rio Grande do Norte","Rio Grande do Sul",
                                 "Rio Grande do Sul","Santa Catarina","Santa Catarina","Sergipe",
                                 "Sergipe")
teste_kruskal_juntos <- select(teste_kruskal_juntos, Estado, everything())

write.xlsx(teste_kruskal_juntos,"Kruskal_Wallis.xlsx")









# Distruibuição da Velocidade do Vento por fase do ENSO - O melhor índice do ENSO para cada Estado
Ventos <- read_excel("Series_Ventos_Tese.xlsx")[,-1]
ENSO <- as.data.frame(read_excel("ENSO_Períodos.xlsx")[c(589:1116),c(3,5,7,9,11,13,15)])
colnames(ENSO) <- c("SOI","SOI Equatorial","Niño 1+2","Niño 3","Niño 4","Niño 3.4","ONI")
ENSO <- ENSO[,c(3,3,3,3,4,3,3)]
colnames(ENSO) <- c("Niño 1+2","Niño 1+2","Niño 1+2","Niño 1+2","Niño 3","Niño 1+2","Niño 1+2")
source("multiplot.R")

for(i in 1:ncol(ENSO)){
  Ventos_ENSO <- as.data.frame(na.omit(cbind(ENSO[,i],Ventos[,i])))
  colnames(Ventos_ENSO) <- c("ENSO","Ventos")
  plot1 = Ventos_ENSO |> ggplot(mapping = aes(x = ENSO, y = Ventos, fill= ENSO)) +
    geom_boxplot() +  
    scale_fill_manual(values = c("#CC3333","#3366FF","grey50")) +
    ggtitle(paste(colnames(Ventos)[i],"vs",colnames(ENSO)[i])) +
    theme_minimal() +
    
    theme(
      plot.title = element_text(size = 23), 
      axis.text.x = element_text(size = 18), 
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(size = 18),  
      axis.title.y = element_text(size = 15),
      legend.position = "none"
    ) +
  
    # theme(axis.text = element_text(size=10),
    #       axis.title = element_text(size=10),
    #       legend.text = element_text(size=8),
    #       plot.title = element_text(hjust=0.5,size=10),
    #       legend.position = "none") +

    xlab("") +
    ylab("")
  if(i == 1){plot_aux = list(plot1)}else{plot_aux=c(plot_aux,list(plot1))}
}


pdf("RESULTADOS_GRAFICOS//Agregados_Geral.pdf",height=14,width=18)

multiplot(plot_aux[[1]],plot_aux[[2]],plot_aux[[3]],
          plot_aux[[4]],plot_aux[[5]],plot_aux[[6]],
          plot_aux[[7]],cols=2)

dev.off()





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



# Erros
# PAR
erros1 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_1.csv")

# PARX
erros2 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_2.csv")
erros3 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_3.csv")
erros4 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_4.csv")
erros5 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_5.csv")
erros6 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_6.csv")
erros7 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_7.csv")
erros8 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_8.csv")
erros9 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_9.csv")
erros10 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_10.csv")
erros11 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_11.csv")
erros12 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_12.csv")
erros13 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_13.csv")
erros14 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_14.csv")
erros15 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_15.csv")

#PAR-Cov
# Nordeste
erros16 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_16.csv")
# Sul
erros17 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_17.csv")

# PARX-Cov
# Nordeste
erros18 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_18.csv")
erros19 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_19.csv")
erros20 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_20.csv")
erros21 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_21.csv")
erros22 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_22.csv")
erros23 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_23.csv")
erros24 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_24.csv")
erros25 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_25.csv")
erros26 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_26.csv")
erros27 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_27.csv")
erros28 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_28.csv")
erros29 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_29.csv")
erros30 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_30.csv")
erros31 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_31.csv")
# Sul
erros32 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_32.csv")
erros33 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_33.csv")
erros34 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_34.csv")
erros35 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_35.csv")
erros36 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_36.csv")
erros37 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_37.csv")
erros38 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_38.csv")
erros39 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_39.csv")
erros40 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_40.csv")
erros41 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_41.csv")
erros42 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_42.csv")
erros43 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_43.csv")
erros44 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_44.csv")
erros45 = read.csv2("ERROS_RESULTADOS//Erros_Modelo_45.csv")

erros <- rbind(erros1,
               erros2,erros3,erros4,erros5,erros6,erros7,erros8,erros9,
               erros10,erros11,erros12,erros13,erros14,erros15,
               erros16,erros17,
               erros18,erros32,
               erros19,erros33,
               erros20,erros34,
               erros21,erros35,
               erros22,erros36,
               erros23,erros37,
               erros24,erros38,
               erros25,erros39,
               erros26,erros40,
               erros27,erros41,
               erros28,erros42,
               erros29,erros43,
               erros30,erros44,
               erros31,erros45
               )

colnames(erros) <- c("Local","RMSE","MAE","R2")

ENSO <- read_excel("ENSO_Completo.xlsx")[,-1]
colnames(ENSO) <- c("SOI","EQ SOI","NINO1+2","NINO3","NINO4","NINO3.4","ONI","SOI ACUM",
                    "EQ SOI ACUM","NINO1+2 ACUM","NINO3 ACUM","NINO4 ACUM","NINO3.4 ACUM","ONI ACUM")

erros$Modelo <- c(rep("PAR",7),
                  paste("PARX +",rep(colnames(ENSO)[1],7)),paste("PARX +",rep(colnames(ENSO)[2],7)),
                  paste("PARX +",rep(colnames(ENSO)[3],7)),paste("PARX +",rep(colnames(ENSO)[4],7)),
                  paste("PARX +",rep(colnames(ENSO)[5],7)),paste("PARX +",rep(colnames(ENSO)[6],7)),
                  paste("PARX +",rep(colnames(ENSO)[7],7)),paste("PARX +",rep(colnames(ENSO)[8],7)),
                  paste("PARX +",rep(colnames(ENSO)[9],7)),paste("PARX +",rep(colnames(ENSO)[10],7)),
                  paste("PARX +",rep(colnames(ENSO)[11],7)),paste("PARX +",rep(colnames(ENSO)[12],7)),
                  paste("PARX +",rep(colnames(ENSO)[13],7)),paste("PARX +",rep(colnames(ENSO)[14],7)),
                  rep("PAR-Cov",7),
                  paste("PARX-Cov +",rep(colnames(ENSO)[1],7)),paste("PARX-Cov +",rep(colnames(ENSO)[2],7)),
                  paste("PARX-Cov +",rep(colnames(ENSO)[3],7)),paste("PARX-Cov +",rep(colnames(ENSO)[4],7)),
                  paste("PARX-Cov +",rep(colnames(ENSO)[5],7)),paste("PARX-Cov +",rep(colnames(ENSO)[6],7)),
                  paste("PARX-Cov +",rep(colnames(ENSO)[7],7)),paste("PARX-Cov +",rep(colnames(ENSO)[8],7)),
                  paste("PARX-Cov +",rep(colnames(ENSO)[9],7)),paste("PARX-Cov +",rep(colnames(ENSO)[10],7)),
                  paste("PARX-Cov +",rep(colnames(ENSO)[11],7)),paste("PARX-Cov +",rep(colnames(ENSO)[12],7)),
                  paste("PARX-Cov +",rep(colnames(ENSO)[13],7)),paste("PARX-Cov +",rep(colnames(ENSO)[14],7))
                  )


write.xlsx(erros,"Erros_All.xlsx")
#









# Análise dos Erros
# RMSE Medidas
erros_RMSE <- as.data.frame(read_excel("Erros_All.xlsx")[,c(1,2,5)])             
colnames(erros_RMSE) <- c("Local","Medida","Modelo")                                      

erros_RMSE_PAR <- erros_RMSE[c(1:7),]

erros_RMSE_PAR_Cov <- erros_RMSE[c(106:112),]
erros_RMSE_PAR_Cov <- erros_RMSE_PAR_Cov[order(erros_RMSE_PAR_Cov$Local), ]

erros_RMSE_PARX <- erros_RMSE[c(8:105),]
erros_RMSE_PARX <- filter(erros_RMSE_PARX,
                            Medida == min(erros_RMSE_PARX$Medida[which(erros_RMSE_PARX$Local=="Rio Grande do Norte")])|
                            Medida == min(erros_RMSE_PARX$Medida[which(erros_RMSE_PARX$Local=="Paraíba")])|
                            Medida == min(erros_RMSE_PARX$Medida[which(erros_RMSE_PARX$Local=="Pernambuco")])|
                            Medida == min(erros_RMSE_PARX$Medida[which(erros_RMSE_PARX$Local=="Alagoas")])|
                            Medida == min(erros_RMSE_PARX$Medida[which(erros_RMSE_PARX$Local=="Sergipe")])|
                            Medida == min(erros_RMSE_PARX$Medida[which(erros_RMSE_PARX$Local=="Rio Grande do Sul")])|
                            Medida == min(erros_RMSE_PARX$Medida[which(erros_RMSE_PARX$Local=="Santa Catarina")]))
erros_RMSE_PARX <- erros_RMSE_PARX[order(erros_RMSE_PARX$Local), ]

erros_RMSE_PARX_Cov <- erros_RMSE[c(113:210),]                                   
erros_RMSE_PARX_Cov <- filter(erros_RMSE_PARX_Cov,
                          Medida == min(erros_RMSE_PARX_Cov$Medida[which(erros_RMSE_PARX_Cov$Local=="Rio Grande do Norte")])|
                            Medida == min(erros_RMSE_PARX_Cov$Medida[which(erros_RMSE_PARX_Cov$Local=="Paraíba")])|
                            Medida == min(erros_RMSE_PARX_Cov$Medida[which(erros_RMSE_PARX_Cov$Local=="Pernambuco")])|
                            Medida == min(erros_RMSE_PARX_Cov$Medida[which(erros_RMSE_PARX_Cov$Local=="Alagoas")])|
                            Medida == min(erros_RMSE_PARX_Cov$Medida[which(erros_RMSE_PARX_Cov$Local=="Sergipe")])|
                            Medida == min(erros_RMSE_PARX_Cov$Medida[which(erros_RMSE_PARX_Cov$Local=="Rio Grande do Sul")])|
                            Medida == min(erros_RMSE_PARX_Cov$Medida[which(erros_RMSE_PARX_Cov$Local=="Santa Catarina")]))
erros_RMSE_PARX_Cov <- erros_RMSE_PARX_Cov[order(erros_RMSE_PARX_Cov$Local), ]


# Gráfico RMSE Artigo
pdf("RESULTADOS_GRAFICOS//Artigo//Grafico_RMSE.pdf", height=14,width=20)
ggplot() +
  geom_point(data = erros_RMSE, mapping = aes(x = Local, y = Medida, fill = "Other Models"), size = 12,
             shape = 21, color = "grey", alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_RMSE_PAR[,2], fill = "PAR"), size = 12,
             shape = 21, alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_RMSE_PAR_Cov[,2], fill = "PAR-Cov"), size = 12,
             shape = 21, alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_RMSE_PARX[,2], fill = "Best PARX"), size = 12,
             shape = 21, alpha = 1) + 
  geom_point(mapping = aes(x = 1:7, y = erros_RMSE_PARX_Cov[,2], fill = "Best PARX-Cov"), size = 12,
             shape = 21, alpha = 1) +
  ggtitle("Obtained Errors - RMSE") +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 40, hjust=0.5),   
    axis.text.x = element_text(size = 17), 
    axis.text.y = element_text(size = 32),
    axis.title.y = element_text(size = 35),
    legend.text = element_text(size = 30),
    legend.position = "bottom"
  ) +
  
  scale_fill_manual(name = "", values = c("Other Models" = "grey", 
                                          "PAR" = "red", 
                                          "PAR-Cov" = "orange",
                                          "Best PARX" = "green",
                                          "Best PARX-Cov" = "purple")) +
  xlab("") +
  ylab("RMSE")
dev.off()




# Gráfico RMSE Dissertação
pdf("RESULTADOS_GRAFICOS//Grafico_RMSE.pdf", height=14,width=20)
ggplot() +
  geom_point(data = erros_RMSE, mapping = aes(x = Local, y = Medida, fill = "Outros Modelos"), size = 12,
             shape = 21, color = "grey", alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_RMSE_PAR[,2], fill = "PAR"), size = 12,
             shape = 21, alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_RMSE_PAR_Cov[,2], fill = "PAR-Cov"), size = 12,
             shape = 21, alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_RMSE_PARX[,2], fill = "Melhor PARX"), size = 12,
             shape = 21, alpha = 1) + 
  geom_point(mapping = aes(x = 1:7, y = erros_RMSE_PARX_Cov[,2], fill = "Melhor PARX-Cov"), size = 12,
             shape = 21, alpha = 1) +
  ggtitle("Erros obtidos - RMSE") +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 40, hjust=0.5),   
    axis.text.x = element_text(size = 17), 
    axis.text.y = element_text(size = 32),
    axis.title.y = element_text(size = 35),
    legend.text = element_text(size = 30),
    legend.position = "bottom"
  ) +

  # theme(axis.text = element_text(size=10),
  #       axis.title = element_text(size=15),
  #       legend.text = element_text(size=15),
  #       plot.title = element_text(hjust=0.5,size=15),
  #       legend.position = "bottom") +
  
  scale_fill_manual(name = "", values = c("Outros Modelos" = "grey", 
                                          "PAR" = "red", 
                                          "PAR-Cov" = "orange",
                                          "Melhor PARX" = "green",
                                          "Melhor PARX-Cov" = "purple")) +
  xlab("") +
  ylab("RMSE")
dev.off()




# Tabela da Melhoria RMSE 
tabela_RMSE <- as.data.frame(matrix(NA, ncol = 8, nrow = 7))
colnames(tabela_RMSE) <- c("Estado", "RMSE (PAR)", "Melhor modelo", "RMSE (Melhor)", "Melhoria (%)",
                           "2°Melhor modelo", "RMSE (2°Melhor)","Melhoria (%)")
tabela_RMSE$Estado <- erros_RMSE_PAR$Local

# Encontrar o mínimo RMSE para cada estado
min_RMSE <- pmin(erros_RMSE_PAR$Medida,
                 erros_RMSE_PAR_Cov$Medida,
                 erros_RMSE_PARX$Medida,
                 erros_RMSE_PARX_Cov$Medida)

# Determinar o modelo correspondente ao mínimo RMSE
melhor_modelo <- ifelse(min_RMSE == erros_RMSE_PAR$Medida, erros_RMSE_PAR$Modelo,
                        ifelse(min_RMSE == erros_RMSE_PAR_Cov$Medida, erros_RMSE_PAR_Cov$Modelo,
                               ifelse(min_RMSE == erros_RMSE_PARX$Medida, erros_RMSE_PARX$Modelo,
                                      erros_RMSE_PARX_Cov$Modelo)))


# Encontrar o segundo menor RMSE para cada linha
min_RMSE_2 <- apply(cbind(erros_RMSE_PAR$Medida, 
                                erros_RMSE_PAR_Cov$Medida, 
                                erros_RMSE_PARX$Medida, 
                                erros_RMSE_PARX_Cov$Medida), 1, function(row) {
                                  unique(sort(row))[2]
                                })

# Determinar o modelo correspondente ao 2° menor RMSE
melhor_modelo_2 <- ifelse(min_RMSE_2 == erros_RMSE_PAR$Medida, erros_RMSE_PAR$Modelo,
                        ifelse(min_RMSE_2 == erros_RMSE_PAR_Cov$Medida, erros_RMSE_PAR_Cov$Modelo,
                               ifelse(min_RMSE_2 == erros_RMSE_PARX$Medida, erros_RMSE_PARX$Modelo,
                                      erros_RMSE_PARX_Cov$Modelo)))


# Preencher a tabela
tabela_RMSE[,2] <- erros_RMSE_PAR$Medida
tabela_RMSE[,3] <- melhor_modelo
tabela_RMSE[,4] <- min_RMSE
tabela_RMSE[,5] <- round(100 * ((tabela_RMSE[,4] - tabela_RMSE[,2]) / tabela_RMSE[,2]), 2)
tabela_RMSE[,6] <- melhor_modelo_2
tabela_RMSE[,7] <- min_RMSE_2
tabela_RMSE[,8] <- round(100 * ((tabela_RMSE[,7] - tabela_RMSE[,2]) / tabela_RMSE[,2]), 2)



# Overleaf
tabela_RMSE <- tabela_RMSE[,c(1:5)]
tabela_RMSE$`RMSE (PAR)` <- round(tabela_RMSE$`RMSE (PAR)`,4)
tabela_RMSE$`RMSE (Melhor)` <- round(tabela_RMSE$`RMSE (Melhor)`,4)
write.xlsx(tabela_RMSE, "RESULTADOS_GRAFICOS//Tabela_RMSE.xlsx")
# 








# MAE Medidas
erros_MAE <- as.data.frame(read_excel("Erros_All.xlsx")[,c(1,3,5)])             
colnames(erros_MAE) <- c("Local","Medida","Modelo")

erros_MAE_PAR <- erros_MAE[c(1:7),]

erros_MAE_PAR_Cov <- erros_MAE[c(106:112),]
erros_MAE_PAR_Cov <- erros_MAE_PAR_Cov[order(erros_MAE_PAR_Cov$Local), ]

erros_MAE_PARX <- erros_MAE[c(8:105),]
erros_MAE_PARX <- filter(erros_MAE_PARX,
                          Medida == min(erros_MAE_PARX$Medida[which(erros_MAE_PARX$Local=="Rio Grande do Norte")])|
                            Medida == min(erros_MAE_PARX$Medida[which(erros_MAE_PARX$Local=="Paraíba")])|
                            Medida == min(erros_MAE_PARX$Medida[which(erros_MAE_PARX$Local=="Pernambuco")])|
                            Medida == min(erros_MAE_PARX$Medida[which(erros_MAE_PARX$Local=="Alagoas")])|
                            Medida == min(erros_MAE_PARX$Medida[which(erros_MAE_PARX$Local=="Sergipe")])|
                            Medida == min(erros_MAE_PARX$Medida[which(erros_MAE_PARX$Local=="Rio Grande do Sul")])|
                            Medida == min(erros_MAE_PARX$Medida[which(erros_MAE_PARX$Local=="Santa Catarina")]))
erros_MAE_PARX <- erros_MAE_PARX[order(erros_MAE_PARX$Local), ]

erros_MAE_PARX_Cov <- erros_MAE[c(113:210),]                              
erros_MAE_PARX_Cov <- filter(erros_MAE_PARX_Cov,
                              Medida == min(erros_MAE_PARX_Cov$Medida[which(erros_MAE_PARX_Cov$Local=="Rio Grande do Norte")])|
                                Medida == min(erros_MAE_PARX_Cov$Medida[which(erros_MAE_PARX_Cov$Local=="Paraíba")])|
                                Medida == min(erros_MAE_PARX_Cov$Medida[which(erros_MAE_PARX_Cov$Local=="Pernambuco")])|
                                Medida == min(erros_MAE_PARX_Cov$Medida[which(erros_MAE_PARX_Cov$Local=="Alagoas")])|
                                Medida == min(erros_MAE_PARX_Cov$Medida[which(erros_MAE_PARX_Cov$Local=="Sergipe")])|
                                Medida == min(erros_MAE_PARX_Cov$Medida[which(erros_MAE_PARX_Cov$Local=="Rio Grande do Sul")])|
                                Medida == min(erros_MAE_PARX_Cov$Medida[which(erros_MAE_PARX_Cov$Local=="Santa Catarina")]))
erros_MAE_PARX_Cov <- erros_MAE_PARX_Cov[order(erros_MAE_PARX_Cov$Local), ]


# Gráfico MAE
pdf("RESULTADOS_GRAFICOS//Grafico_MAE.pdf",height=14,width=20)
ggplot() +
  geom_point(data = erros_MAE, mapping = aes(x = Local, y = Medida, fill = "Outros Modelos"), size = 12,
             shape = 21, color = "grey", alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_MAE_PAR[,2], fill = "PAR"), size = 12,
             shape = 21, alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_MAE_PAR_Cov[,2], fill = "PAR-Cov"), size = 12,
             shape = 21, alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_MAE_PARX[,2], fill = "Melhor PARX"), size = 12,
             shape = 21, alpha = 1) + 
  geom_point(mapping = aes(x = 1:7, y = erros_MAE_PARX_Cov[,2], fill = "Melhor PARX-Cov"), size = 12,
             shape = 21, alpha = 1) +
  ggtitle("Erros obtidos - MAE") +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 40, hjust=0.5),   
    axis.text.x = element_text(size = 17), 
    axis.text.y = element_text(size = 32),
    axis.title.y = element_text(size = 35),
    legend.text = element_text(size = 30),
    legend.position = "bottom"
    ) +
  
  scale_fill_manual(name = "", values = c("Outros Modelos" = "grey", 
                                          "PAR" = "red", 
                                          "PAR-Cov" = "orange",
                                          "Melhor PARX" = "green",
                                          "Melhor PARX-Cov" = "purple")) +
  xlab("") +
  ylab("MAE")
dev.off()



# Tabela da Melhoria MAE 
tabela_MAE <- as.data.frame(matrix(NA, ncol = 8, nrow = 7))
colnames(tabela_MAE) <- c("Estado", "MAE (PAR)", "Melhor modelo", "MAE (Melhor)", "Melhoria (%)",
                          "2°Melhor modelo", "MAE (2°Melhor)","Melhoria (%)")
tabela_MAE$Estado <- erros_MAE_PAR$Local

# Encontrar o mínimo MAE para cada estado
min_MAE <- pmin(erros_MAE_PAR$Medida,
                 erros_MAE_PAR_Cov$Medida,
                 erros_MAE_PARX$Medida,
                 erros_MAE_PARX_Cov$Medida)

# Determinar o modelo correspondente ao mínimo MAE
melhor_modelo <- ifelse(min_MAE == erros_MAE_PAR$Medida, erros_MAE_PAR$Modelo,
                        ifelse(min_MAE == erros_MAE_PAR_Cov$Medida, erros_MAE_PAR_Cov$Modelo,
                               ifelse(min_MAE == erros_MAE_PARX$Medida, erros_MAE_PARX$Modelo,
                                      erros_MAE_PARX_Cov$Modelo)))


# Encontrar o segundo menor MAE para cada linha
min_MAE_2 <- apply(cbind(erros_MAE_PAR$Medida, 
                          erros_MAE_PAR_Cov$Medida, 
                          erros_MAE_PARX$Medida, 
                          erros_MAE_PARX_Cov$Medida), 1, function(row) {
                            unique(sort(row))[2]
                          })

# Determinar o modelo correspondente ao 2° menor MAE
melhor_modelo_2 <- ifelse(min_MAE_2 == erros_MAE_PAR$Medida, erros_MAE_PAR$Modelo,
                          ifelse(min_MAE_2 == erros_MAE_PAR_Cov$Medida, erros_MAE_PAR_Cov$Modelo,
                                 ifelse(min_MAE_2 == erros_MAE_PARX$Medida, erros_MAE_PARX$Modelo,
                                        erros_MAE_PARX_Cov$Modelo)))


# Preencher a tabela
tabela_MAE[,2] <- erros_MAE_PAR$Medida
tabela_MAE[,3] <- melhor_modelo
tabela_MAE[,4] <- min_MAE
tabela_MAE[,5] <- round(100 * ((tabela_MAE[,4] - tabela_MAE[,2]) / tabela_MAE[,2]), 2)
tabela_MAE[,6] <- melhor_modelo_2
tabela_MAE[,7] <- min_MAE_2
tabela_MAE[,8] <- round(100 * ((tabela_MAE[,7] - tabela_MAE[,2]) / tabela_MAE[,2]), 2)




# Overleaf
tabela_MAE <- tabela_MAE[,c(1:5)]
tabela_MAE$`MAE (PAR)` <- round(tabela_MAE$`MAE (PAR)`,4)
tabela_MAE$`MAE (Melhor)` <- round(tabela_MAE$`MAE (Melhor)`,4)
write.xlsx(tabela_MAE, "RESULTADOS_GRAFICOS//Tabela_MAE.xlsx")
#








# R2 Medidas
erros_R2 <- as.data.frame(read_excel("Erros_All.xlsx")[,c(1,4,5)])            
colnames(erros_R2) <- c("Local","Medida","Modelo")

erros_R2_PAR <- erros_R2[c(1:7),]

erros_R2_PAR_Cov <- erros_R2[c(106:112),]
erros_R2_PAR_Cov <- erros_R2_PAR_Cov[order(erros_R2_PAR_Cov$Local), ]

erros_R2_PARX <- erros_R2[c(8:105),]
erros_R2_PARX <- filter(erros_R2_PARX,
                          Medida == max(erros_R2_PARX$Medida[which(erros_R2_PARX$Local=="Rio Grande do Norte")])|
                            Medida == max(erros_R2_PARX$Medida[which(erros_R2_PARX$Local=="Paraíba")])|
                            Medida == max(erros_R2_PARX$Medida[which(erros_R2_PARX$Local=="Pernambuco")])|
                            Medida == max(erros_R2_PARX$Medida[which(erros_R2_PARX$Local=="Alagoas")])|
                            Medida == max(erros_R2_PARX$Medida[which(erros_R2_PARX$Local=="Sergipe")])|
                            Medida == max(erros_R2_PARX$Medida[which(erros_R2_PARX$Local=="Rio Grande do Sul")])|
                            Medida == max(erros_R2_PARX$Medida[which(erros_R2_PARX$Local=="Santa Catarina")]))
erros_R2_PARX <- erros_R2_PARX[order(erros_R2_PARX$Local), ]

erros_R2_PARX_Cov <- erros_R2[c(113:210),]                                
erros_R2_PARX_Cov <- filter(erros_R2_PARX_Cov,
                              Medida == max(erros_R2_PARX_Cov$Medida[which(erros_R2_PARX_Cov$Local=="Rio Grande do Norte")])|
                                Medida == max(erros_R2_PARX_Cov$Medida[which(erros_R2_PARX_Cov$Local=="Paraíba")])|
                                Medida == max(erros_R2_PARX_Cov$Medida[which(erros_R2_PARX_Cov$Local=="Pernambuco")])|
                                Medida == max(erros_R2_PARX_Cov$Medida[which(erros_R2_PARX_Cov$Local=="Alagoas")])|
                                Medida == max(erros_R2_PARX_Cov$Medida[which(erros_R2_PARX_Cov$Local=="Sergipe")])|
                                Medida == max(erros_R2_PARX_Cov$Medida[which(erros_R2_PARX_Cov$Local=="Rio Grande do Sul")])|
                                Medida == max(erros_R2_PARX_Cov$Medida[which(erros_R2_PARX_Cov$Local=="Santa Catarina")]))
erros_R2_PARX_Cov <- erros_R2_PARX_Cov[order(erros_R2_PARX_Cov$Local), ]


# Gráfico R2
pdf("RESULTADOS_GRAFICOS//Grafico_R2.pdf",height=14,width=20)
ggplot() +
  geom_point(data = erros_R2, mapping = aes(x = Local, y = Medida, fill = "Outros Modelos"), size = 12,
             shape = 21, color = "grey", alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_R2_PAR[,2], fill = "PAR"), size = 12,
             shape = 21, alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_R2_PAR_Cov[,2], fill = "PAR-Cov"), size = 12,
             shape = 21, alpha = 1) +
  geom_point(mapping = aes(x = 1:7, y = erros_R2_PARX[,2], fill = "Melhor PARX"), size = 12,
             shape = 21, alpha = 1) + 
  geom_point(mapping = aes(x = 1:7, y = erros_R2_PARX_Cov[,2], fill = "Melhor PARX-Cov"), size = 12,
             shape = 21, alpha = 1) +
  ggtitle(expression(paste("Ajustes obtidos - ", R^2))) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(size = 40, hjust=0.5),   
    axis.text.x = element_text(size = 17), 
    axis.text.y = element_text(size = 32),
    axis.title.y = element_text(size = 35),
    legend.text = element_text(size = 30),
    legend.position = "bottom"
  ) +
  
  scale_fill_manual(name = "", values = c("Outros Modelos" = "grey", 
                                          "PAR" = "red", 
                                          "PAR-Cov" = "orange",
                                          "Melhor PARX" = "green",
                                          "Melhor PARX-Cov" = "purple")) +
  xlab("") +
  ylab(expression(R^2))
dev.off()




# Tabela da Melhoria R2 
tabela_R2 <- as.data.frame(matrix(NA, ncol = 8, nrow = 7))
colnames(tabela_R2) <- c("Estado", "R2 (PAR)", "Melhor modelo", "R2 (Melhor)", "Melhoria (%)",
                         "2°Melhor modelo", "R2 (2°Melhor)","Melhoria (%)")
tabela_R2$Estado <- erros_R2_PAR$Local

# Encontrar o mínimo R2 para cada estado
max_R2 <- pmax(erros_R2_PAR$Medida,
                erros_R2_PAR_Cov$Medida,
                erros_R2_PARX$Medida,
                erros_R2_PARX_Cov$Medida)

# Determinar o modelo correspondente ao mínimo R2
melhor_modelo <- ifelse(max_R2 == erros_R2_PAR$Medida, erros_R2_PAR$Modelo,
                        ifelse(max_R2 == erros_R2_PAR_Cov$Medida, erros_R2_PAR_Cov$Modelo,
                               ifelse(max_R2 == erros_R2_PARX$Medida, erros_R2_PARX$Modelo,
                                      erros_R2_PARX_Cov$Modelo)))


# Encontrar o segundo menor R2 para cada linha
max_R2_2 <- apply(cbind(erros_R2_PAR$Medida, 
                         erros_R2_PAR_Cov$Medida, 
                         erros_R2_PARX$Medida, 
                         erros_R2_PARX_Cov$Medida), 1, function(row) {
                           unique(sort(row))[3]
                         })

# Determinar o modelo correspondente ao 2° menor R2
melhor_modelo_2 <- ifelse(max_R2_2 == erros_R2_PAR$Medida, erros_R2_PAR$Modelo,
                          ifelse(max_R2_2 == erros_R2_PAR_Cov$Medida, erros_R2_PAR_Cov$Modelo,
                                 ifelse(max_R2_2 == erros_R2_PARX$Medida, erros_R2_PARX$Modelo,
                                        erros_R2_PARX_Cov$Modelo)))


# Preencher a tabela
tabela_R2[,2] <- erros_R2_PAR$Medida
tabela_R2[,3] <- melhor_modelo
tabela_R2[,4] <- max_R2
tabela_R2[,5] <- round(100 * ((tabela_R2[,4] - tabela_R2[,2]) / tabela_R2[,2]), 2)
tabela_R2[,6] <- melhor_modelo_2
tabela_R2[,7] <- max_R2_2
tabela_R2[,8] <- round(100 * ((tabela_R2[,7] - tabela_R2[,2]) / tabela_R2[,2]), 2)



# Overleaf
tabela_R2 <- tabela_R2[,c(1:5)]
tabela_R2$`R2 (PAR)` <- round(tabela_R2$`R2 (PAR)`,4)
tabela_R2$`R2 (Melhor)` <- round(tabela_R2$`R2 (Melhor)`,4)
write.xlsx(tabela_R2, "RESULTADOS_GRAFICOS//Tabela_R2.xlsx")
#




## Resumo dos modelos mais escolhidos como melhores.
colnames(tabela_RMSE) <- colnames(tabela_MAE) <- colnames(tabela_R2) <- c("Estado", "PAR", "Modelo", "Medida", "Melhoria (%)")
tabela_resumo <- rbind(tabela_RMSE,tabela_MAE,tabela_R2)
tabela_resumo <- tabela_resumo %>% group_by(Modelo) %>% summarise(Quantidade = n()) 
tabela_resumo <- tabela_resumo[order(-tabela_resumo$Quantidade), ]
# write.xlsx(tabela_resumo, "RESULTADOS_GRAFICOS//Tabela_Resumo.xlsx")
#

# Melhor modelo
tabela_melhores <- data.frame(
  
  Estado = c("Alagoas", 
             "Paraíba", 
             "Pernambuco", 
             "Rio Grande do Norte", 
             "Rio Grande do Sul", 
             "Santa Catarina", 
             "Sergipe"),
  
  Melhor_Modelo = c("PARX + ONI ACUM", 
                    "PARX + NINO4 ACUM", 
                    "PARX-Cov + ONI ACUM", 
                    "PARX-Cov + SOI", 
                    "PARX-Cov + ONI ACUM", 
                    "PARX-Cov + ONI ACUM", 
                    "PARX + ONI ACUM")
)
#











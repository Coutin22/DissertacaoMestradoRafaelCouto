# Diretório
setwd("~/Rafael_Estudo/PUC/Dissertação/Resultados/Bases de Dados/ENSO")

# Pacotes
library(readxl)
library(openxlsx)

# Base Anomalias ENSO
# ENSO1 <- read_excel("ENSO_31-18.xlsx")[c(1:612),-c(3,4,5,7,12,13,14,15,17)]
# ENSO2 <- read_excel("ENSO_82-22.xlsx") 
# colnames(ENSO1) <- colnames(ENSO2)
# ENSO <- rbind(ENSO1,ENSO2)
# 
# ENSO$AnomSOI <- as.numeric(ENSO$AnomSOI)
# ENSO$AnomEqSOI <- as.numeric(ENSO$AnomEqSOI)
# ENSO$AnomONI <- as.numeric(ENSO$AnomONI)
# 
# colnames(ENSO) <- c("Data","SOI","SOI Equatorial","Niño 1+2","Niño 3","Niño 4","Niño 3.4","ONI")
# 
# write.xlsx(ENSO,"ENSO_31-22.xlsx")



# Base Anomalias ENSO 1931-2022
ENSO <- read_excel("ENSO_31-22.xlsx") 


# Atualizando 2023 Histórico até 2024-03
Data <- format(seq(as.Date("2023-01-01"), as.Date("2024-03-01"), by = "month"), "%Y-%m-%d")
Data <- as.Date(Data)


# Baixando dados
# SOI
SOI_At <- c(2.3,2.3,0.3,0.4,-1.7,0.4,-0.4,-1.4,-2.1,-0.8,-1.3,-0.4,0.8,-2.3,0.6)

# Equatorial SOI
url <- "https://www.cpc.ncep.noaa.gov/data/indices/reqsoi.for"
dest_file <- "EqSOI.txt"
download.file(url, dest_file)
EqSOI_At <- read.table(dest_file)[c(75,76),-1]
EqSOI_At <- cbind(EqSOI_At[1,],EqSOI_At[2,c(1:3)])
EqSOI_At <- t(EqSOI_At)

# SST
url <- "https://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices"
dest_file <- "SST.txt"
download.file(url, dest_file)
SST_At <- read.table(dest_file, header = TRUE)[c(493:507),c(4,6,8,10)]

# ONI
url <- "https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt"
dest_file <- "ONI.txt"
download.file(url, dest_file)
ONI_At <- read.table(dest_file, header = TRUE)[c(877:891),4]

# Juntando
ENSO_At <- cbind(Data,SOI_At,EqSOI_At,SST_At,ONI_At)
colnames(ENSO_At) <- colnames(ENSO)
ENSO <- rbind(ENSO,ENSO_At)
#ENSO$Data <- format(ENSO$Data, "%Y-%m")
str(ENSO)

write.xlsx(ENSO,"ENSO_Anomalias.xlsx")




# Introduzindo Anomalias Acumuladas
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

write.xlsx(ENSO,"ENSO_Completo.xlsx")




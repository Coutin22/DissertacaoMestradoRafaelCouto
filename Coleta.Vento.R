# Diretório
setwd("~/Rafael_Estudo/PUC/Dissertação/Base de Dados")

# Pacotes
{library(curl)
library(stringr)
library(dplyr)
library(zoo)
library(openxlsx)
library(readxl)}

# Chamando Funções do Ninja Automator  
source('Funções.R')

# Token
token = 'e04eb8b30bc800a26861efb8830641f5f3292113' #gmail
#token = 'e58a7c26f790bb2501b61f8f3796950a509d82b6' #hotmail
#token = '7bfcb3cd4c4f89fc58c958ff2dd86d270e4e2a20' #puc
#token = '37d00775fec38606c0bb524fabce36bf3207a913' #gustavo
h = new_handle()
handle_setheaders(h, 'Authorization'=paste('Token', token))


# Coordenadas
Base <- read_excel("Coordenadas.xlsx")
latitude = Base$Latitude[21]
longitude = Base$Longitude[21]
altura = 100 


# Coleta anual
dados2023 <- ninja_get_wind(lat=latitude, lon=longitude, from='2023-01-01', to='2023-12-31', height=altura) 

# Tratamento 
{
dados <- dados2023[,c(1,3)]
dados <- na.omit(dados)
colnames(dados) <- c("Data","Vento") 
dados$Data <- str_sub(dados$Data,end = 7)
dados <- dados %>% group_by(Data) %>% summarise(Vento = mean(Vento,na.rm=T))
dados$Data <- as.Date(as.yearmon(dados$Data))
}


# Saves Parciais
dados.1 <- dados
dados.2 <- dados
dados.3 <- dados


# Juntando
Media.Vento <- (dados.1[,2] + dados.2[,2] + dados.3[,2])/3
dados_novos <- cbind(dados.1[,1],Media.Vento)


# Atualizando
dados_originais <- read_excel("SCatarina.xlsx")
dados_finais <- rbind(dados_originais,dados_novos)
write.xlsx(dados_finais,paste(gsub(" ", "_", Base$Estado[19]),".xlsx",sep=""))

















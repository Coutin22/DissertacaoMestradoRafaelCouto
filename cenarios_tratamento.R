
# Funções de tratamento dos cenários

cenarios_tratamento_aux <- function(dados,n_sim) {
  
  cenarios <- data.frame(dados)
  
  cenarios <- data.frame(t(cenarios))
  
  cenarios <- melt(cenarios, measure.vars = names(cenarios))
  cenarios$variable <- as.numeric(str_sub(cenarios$variable,start = 9))
  
  cenarios$mes <- rep(1:12,n_sim)
  cenarios <- cenarios[,c(1,3)]
  
  return(cenarios)
  
}


cenarios_tratamento_final <- function(dados1,dados2) {
  
  cenarios <- data.frame(dados1)
  cenarios <- cenarios*dados2
  
  cenarios <- data.frame(t(cenarios))
  
  cenarios <- melt(cenarios, measure.vars = names(cenarios))
  
  cenarios <- cenarios[,2]
  
  return(cenarios)
  
}



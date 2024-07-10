
# PAR(p)
# Função para selecionar ordem dos modelos
escolha_ordem = function(dados,lag.max){
  rho = rep(NA,12)
  pcorr = pepacf(dados,lag.max,plot=F)$pacf
  N = length(dados)/12
  
  for(m in 1:nrow(pcorr)){
    for(ilag in 1:ncol(pcorr)){
      if(abs(pcorr[m,ilag]) >= (1.96/sqrt(N))){rho[m] = ilag}
    }
  }
  rho[which(is.na(rho)==TRUE)] = 1
  return(rho)
}

 
# Função de simulação padrão
cenarios_par_LogNormal = function(dados, num_periodos=60, num_cenarios=2000, ordem){
  
  dados_media = dados_desvio = var_a = media_residuos = matrix(NA, nrow = 12, ncol = ncol(dados)) #media de cada mes de cada usina
  dados_norm = ruido_par = dados
  
  for(usina in 1:ncol(dados)){
    usina_aux = dados[,usina]
    
    #Obtendo as médias e variâncias por mês, por usina
    for(mes in 1:12){
      linhas = which(cycle(usina_aux) == mes)
      dados_media[mes,usina] = mean(usina_aux[linhas])
      dados_desvio[mes,usina] = sd(usina_aux[linhas])
      dados_norm[linhas,usina] = (usina_aux[linhas] - dados_media[mes,usina])/dados_desvio[mes,usina]
    }
    
    #Ajustando a série com PAR(p) e obtendo resíduos, por usina
    par_ajustado = pear(dados_norm[,usina], ordem[usina,])
    
    if(usina == 1){
      
      ruido_par[,usina] = as.vector(par_ajustado$residuals)
      phi_pm = list(par_ajustado$phi)
    
    }else{
      
      ruido_par[,usina] = as.vector(par_ajustado$residuals)
      phi_pm = append(phi_pm,list(par_ajustado$phi))
      
    }
    
    for(mes in 1:12){
      
      media_residuos[mes,usina] = mean(ruido_par[,usina][cycle(ruido_par[,usina]) == mes])
      var_a[mes,usina] = var(ruido_par[,usina][cycle(ruido_par[,usina]) == mes])
      
    }
  }
  
  #Gerando matriz de cenários (num_cenarios x num_periodos)
  for(usina in 1:ncol(dados)){
    
    usina_aux = dados[,usina]
    aux_cenario = usina_aux[(length(usina_aux)-11):length(usina_aux)]
    
    cenario_fwd = matrix(NA, ncol = num_periodos, nrow = num_cenarios)
    cenario_fwd = cbind(matrix(rep(aux_cenario,num_cenarios),ncol=12,byrow=T),cenario_fwd)
    rownames(cenario_fwd) = paste("Cenario",c(1:num_cenarios))
    colnames(cenario_fwd) = rep(1:12,ncol(cenario_fwd)/12)
    
    #Gerando os cenários
    for(cen in 1:num_cenarios){
      for(per in 13:(num_periodos+12)){
        
        mes = as.numeric(colnames(cenario_fwd)[per])
        W = rnorm(1,mean=0,sd=1)
        ordem_aux = ordem[usina,mes]
        
        phi = phi_pm[[usina]][mes,1:ordem_aux]
        phi <- rev(phi) # to change the order
        
        mes_aux = as.numeric(colnames(cenario_fwd)[(per-ordem_aux):(per-1)])
        
        z_chapeu = phi %*% ((cenario_fwd[cen,(per-ordem_aux):(per-1)]-dados_media[mes_aux,usina])/dados_desvio[mes_aux,usina])
        delta = - ((dados_media[mes,usina]/dados_desvio[mes,usina]) + z_chapeu)
        teta = 1 + (var_a[mes,usina]/((media_residuos[mes,usina] - delta)^2))
        sigma_csi = sqrt(log(teta))
        mi_csi = log(sqrt(var_a[mes,usina])/sqrt(teta*(teta-1)))
        a = exp((W*sigma_csi) + mi_csi) + delta
        
        cenario_fwd[cen,per] = dados_media[mes,usina]+z_chapeu*dados_desvio[mes,usina]+dados_desvio[mes,usina]*a
      }
      #print(cen)
    }
    
    if(usina == 1){cenario_fwd_list = list(cenario_fwd[,-c(1:12)])}else{
      cenario_fwd_list = append(cenario_fwd_list,list(cenario_fwd[,-c(1:12)]))
    }
    
  }
  
  # out = list(Cenarios = cenario_fwd[,-c(1:12)])
  cenario_fwd_list
}




# dados = norm_janela_2_fit_Ventos
# num_periodos = 60
# num_cenarios = 100
# ordem = par_ordem_janela_2

# PAR(p) - Cov
cenarios_par_LogNormal_cov = function(dados, num_periodos=60, num_cenarios=2000, ordem){
  
  dados_media = dados_desvio = var_a = media_residuos = matrix(NA, nrow = 12, ncol = ncol(dados)) #media de cada mes de cada usina
  dados_norm = ruido_par = dados
  
  for(usina in 1:ncol(dados)){
    usina_aux = dados[,usina]
    
    #Obtendo as médias e variâncias por mês, por usina
    
    for(mes in 1:12){
      
      linhas = which(cycle(usina_aux) == mes)
      dados_media[mes,usina] = mean(usina_aux[linhas])
      dados_desvio[mes,usina] = sd(usina_aux[linhas])
      dados_norm[linhas,usina] = (usina_aux[linhas] - dados_media[mes,usina])/dados_desvio[mes,usina]
      
    }
    
    #Ajustando a série com PAR(p) e obtendo resíduos, por usina
    
    par_ajustado = pear(dados_norm[,usina], ordem[usina,])
    if(usina == 1){
      
      ruido_par[,usina] = as.vector(par_ajustado$residuals)
      phi_pm = list(par_ajustado$phi)
      
    }else{
      
      ruido_par[,usina] = as.vector(par_ajustado$residuals)
      phi_pm = append(phi_pm,list(par_ajustado$phi))
      
    }
    
    
    for(mes in 1:12){
      
      media_residuos[mes,usina] = mean(ruido_par[,usina][cycle(ruido_par[,usina]) == mes])
      var_a[mes,usina] = var(ruido_par[,usina][cycle(ruido_par[,usina]) == mes])
      
    }
  }
  

# Covâriancia    
  for(mes in 1:12){
    
    cov_mes = cor(ruido_par[cycle(ruido_par) == mes,])
    
    decomp_espect = eigen(cov_mes)
    
    D_aux = decomp_espect$vectors%*%((diag(ncol(dados))*decomp_espect$values)^(1/2))
    
    if(mes == 1){
      
      D = list(D_aux)
      
    }else{
      
      D = append(D,list(D_aux))
      
    }
    
  }
  
  
  
  #Gerando matriz de cenários (num_cenarios x num_periodos)
  for(usina in 1:ncol(dados)){
    usina_aux = dados[,usina]
    aux_cenario = usina_aux[(length(usina_aux)-11):length(usina_aux)]
    
    cenario_fwd = matrix(NA, ncol = num_periodos, nrow = num_cenarios)
    cenario_fwd = cbind(matrix(rep(aux_cenario,num_cenarios),ncol=12,byrow=T),cenario_fwd)
    rownames(cenario_fwd) = paste("Cenario",c(1:num_cenarios))
    colnames(cenario_fwd) = rep(1:12,ncol(cenario_fwd)/12)
    
    #Gerando os cenários
    for(cen in 1:num_cenarios){
      for(per in 13:(num_periodos+12)){
        
        mes = as.numeric(colnames(cenario_fwd)[per])
        W = D[[mes]]%*%rnorm(ncol(dados),0,1)                        # Covâriancia
        ordem_aux = ordem[usina,mes]
        
        phi = phi_pm[[usina]][mes,1:ordem_aux]
        phi <- rev(phi) # to change the order
        
        mes_aux = as.numeric(colnames(cenario_fwd)[(per-ordem_aux):(per-1)])
        
        z_chapeu = phi %*% ((cenario_fwd[cen,(per-ordem_aux):(per-1)]-dados_media[mes_aux,usina])/dados_desvio[mes_aux,usina])
        delta = - ((dados_media[mes,usina]/dados_desvio[mes,usina]) + z_chapeu)
        teta = 1 + (var_a[mes,usina]/((media_residuos[mes,usina] - delta)^2))
        sigma_csi = sqrt(log(teta))
        mi_csi = log(sqrt(var_a[mes,usina])/sqrt(teta*(teta-1)))
        a = exp((W[usina,1]*sigma_csi) + mi_csi) + delta             # Covâriancia
 
        cenario_fwd[cen,per] = dados_media[mes,usina]+z_chapeu*dados_desvio[mes,usina]+dados_desvio[mes,usina]*a
      }
      #print(cen)
    }
    
    if(usina == 1){cenario_fwd_list = list(cenario_fwd[,-c(1:12)])}else{
      cenario_fwd_list = append(cenario_fwd_list,list(cenario_fwd[,-c(1:12)]))
    }
    
  }
  
  cenario_fwd_list
}


 





# PARX(p,m)
# Função para selecionar os parametros padrão
PARX_melhor_p_m = function(dados1,dados2,dados1_valida,max_dados1=1,dados2_valida,max_p,max_m,num_cenarios_in=500){
  
  dados1_novo = window(dados1,start=start(dados2))
  
  erros_p_m = matrix(NA,ncol=max_m+1,nrow=max_p)
  
  for(m in 0:max_m){
    for(p in 1:max_p){
      
      fit = cenarios_parX_LogNormal(dados1_novo, dados2, dados2_valida, p, m, num_cenarios_in)
      
      for(usina in 1:7){
         prev = colMeans(fit[[usina]])*max_dados1[usina] 
      }

      erros_p_m[p,m+1] = MAE(dados1_valida,prev)
      
    }
  }
  
  erros_finais = matrix(NA,ncol=2,nrow=1,dimnames=list(1,c("Melhor_p","Melhor_m")))
  aux_min = which(erros_p_m == min(erros_p_m),arr.ind=T)
  erros_finais[1,1] = aux_min[1,1]
  erros_finais[1,2] = aux_min[1,2] - 1
  
  return(list(erros_finais,erros_p_m))
  
}



# Função de simulação padrão
cenarios_parX_LogNormal = function(dados_Y, dados_X, dados_X_out, max_p, max_m, num_cenarios=2000){
  
  num_periodos = length(dados_X_out)
  
  dados_media_Y = dados_desvio_Y = var_a = media_residuos = matrix(NA, nrow = 12, ncol = ncol(dados_Y)) 
  dados_media_X = dados_desvio_X = NULL
  
  dados_norm_Y = ruido_par = dados_Y
  ruido_par = ruido_par[-c(1:12),] #removendo o primeiro ano, por causa da var. explicativa
  dados_norm_X = dados_X
  
  for(mes in 1:12){
    linhas = which(cycle(dados_norm_X) == mes)
    
    dados_media_X[mes] = mean(dados_X[linhas])
    dados_desvio_X[mes] = sd(dados_X[linhas])
    dados_norm_X[linhas] = (dados_X[linhas] - dados_media_X[mes])/dados_desvio_X[mes]
  }
  
  for(usina in 1:ncol(dados_Y)){
    usina_aux_Y = dados_Y[,usina]
    
    #Obtendo as médias e variâncias por mês, por usina
    for(mes in 1:12){
      linhas = which(cycle(usina_aux_Y) == mes)
      
      dados_media_Y[mes,usina] = mean(usina_aux_Y[linhas])
      dados_desvio_Y[mes,usina] = sd(usina_aux_Y[linhas])
      dados_norm_Y[linhas,usina] = (usina_aux_Y[linhas] - dados_media_Y[mes,usina])/dados_desvio_Y[mes,usina]
    }
    
    #Ajustando a série com PARX e encontrando a melhor combinação de p & m, por usina
    
    matrix_aux = matrix(NA,ncol=max_m+1,nrow=max_p,dimnames=list(paste("p=",1:max_p,sep=""),paste("m=",0:max_m,sep="")))
    
    val_MAPE = val_BIC = list(matrix_aux,matrix_aux,matrix_aux,matrix_aux,matrix_aux,matrix_aux,
                              matrix_aux,matrix_aux,matrix_aux,matrix_aux,matrix_aux,matrix_aux)
    
    melhor_comb = matrix(NA,ncol=2,nrow=12)
    colnames(melhor_comb) = c("p","m")
    rownames(melhor_comb) = 1:12
    
    for(mes in 1:12){
      
      z_v = dados_norm_Y[,usina][cycle(dados_norm_Y[,usina]) == mes]
      N = length(z_v)
      z_v = z_v[-1]
      
      for(p in 1:max_p){
        
        elementos = seq(mes-p,(N-1)*12+mes-p,12)
        
        if(p == 1){
          aux_Y_v = dados_norm_Y[,usina][elementos[elementos > 0]]
          if(length(aux_Y_v) != length(z_v)){
            Y_v = aux_Y_v[-1]
          }else{
            Y_v = aux_Y_v
          }
        }else{
          aux_Y_v = dados_norm_Y[,usina][elementos[elementos > 0]]
          if(length(aux_Y_v) != length(z_v)){
            Y_v = cbind(Y_v,aux_Y_v[-1])
          }else{
            Y_v = cbind(Y_v,aux_Y_v)
          }
        }
        
        for(m in 0:max_m){
          
          elementos = seq(mes-m,(N-1)*12+mes-m,12)
          
          if(m == 0){
            aux_X_v = dados_norm_X[elementos[elementos > 0]]
            if(length(aux_X_v) != length(z_v)){
              X_v = aux_X_v[-1]
            }else{
              X_v = aux_X_v
            }
          }else{
            aux_X_v = dados_norm_X[elementos[elementos > 0]]
            if(length(aux_X_v) != length(z_v)){
              X_v = cbind(X_v,aux_X_v[-1])
            }else{
              X_v = cbind(X_v,aux_X_v)
            }
          }
          
          Z_v = cbind(Y_v,X_v)
          
          #      linear_dependent_columns <- which(apply(Z_v, 2, function(col) length(unique(col)) == 1))
          #      any(is.na(Z_v)) || any(is.infinite(Z_v)) || any(is.na(z_v)) || any(is.infinite(z_v))
          
          beta = solve(t(Z_v)%*%Z_v)%*%t(Z_v)%*%z_v
          z_chapeu = Z_v%*%beta
          e_v = z_v-z_chapeu
          BIC_v = log(var(e_v))+log(N)/N*(p+m)
          val_BIC[[mes]][p,m+1] = BIC_v
          val_MAPE[[mes]][p,m+1] = sum(abs(e_v/z_v))
        }
      }
      
      #the model with the lowest BIC is preferred
      aux_BIC = which(val_BIC[[mes]] == min(val_BIC[[mes]]),arr.ind = T)
      melhor_comb[mes,1] = aux_BIC[1,1]
      melhor_comb[mes,2] = aux_BIC[1,2]-1
    }
    
    #Ajustando a série com PARX e obtendo resíduos, por usina
    
    # Betas
    beta_Y = matrix(NA,nrow=12,ncol=max_p)
    beta_X = matrix(NA,nrow=12,ncol=max_m+1)
    Y_chapeu_ts = residuos = matrix(NA,nrow=12,ncol=nrow(dados_Y)/12-1)
    
    for(mes in 1:12){
      
      z_v = dados_norm_Y[,usina][cycle(dados_norm_Y[,usina]) == mes]
      N = length(z_v)
      z_v = z_v[-1]
      
      for(p in 1:melhor_comb[mes,1]){
        elementos = seq(mes-p,(N-1)*12+mes-p,12)
        if(p == 1){
          aux_Y_v = dados_norm_Y[,usina][elementos[elementos > 0]]
          if(length(aux_Y_v) != length(z_v)){
            Y_v = aux_Y_v[-1]
          }else{
            Y_v = aux_Y_v
          }
        }else{
          aux_Y_v = dados_norm_Y[,usina][elementos[elementos > 0]]
          if(length(aux_Y_v) != length(z_v)){
            Y_v = cbind(Y_v,aux_Y_v[-1])
          }else{
            Y_v = cbind(Y_v,aux_Y_v)
          }
        }
      }
      
      for(m in 0:melhor_comb[mes,2]){
        elementos = seq(mes-m,(N-1)*12+mes-m,12)
        if(m == 0){
          aux_X_v = dados_norm_X[elementos[elementos > 0]]
          if(length(aux_X_v) != length(z_v)){
            X_v = aux_X_v[-1]
          }else{
            X_v = aux_X_v
          }
        }else{
          aux_X_v = dados_norm_X[elementos[elementos > 0]]
          if(length(aux_X_v) != length(z_v)){
            X_v = cbind(X_v,aux_X_v[-1])
          }else{
            X_v = cbind(X_v,aux_X_v)
          }
        }
      }
      
      Z_v = cbind(Y_v,X_v)
      beta = solve(t(Z_v)%*%Z_v)%*%t(Z_v)%*%z_v
      
      beta_Y[mes,1:melhor_comb[mes,1]] = beta[1:melhor_comb[mes,1]]
      beta_X[mes,1:(melhor_comb[mes,2]+1)] = beta[(melhor_comb[mes,1]+1):nrow(beta)]
      
      z_chapeu = Z_v %*% beta
      Y_chapeu = z_chapeu*dados_desvio_Y[mes]+dados_media_Y[mes]
      e_v = z_v-z_chapeu
      Y_chapeu_ts[mes,] = Y_chapeu
      
      residuos[mes,] = e_v
    }
    
    Y_chapeu_ts = ts(c(Y_chapeu_ts),end=end(dados_Y[,usina]),freq=frequency(dados_Y[,usina]))
    residuos = ts(c(residuos),end=end(dados_Y[,usina]),freq=frequency(dados_Y[,usina]))
    
    # terminou o ajuste via PARX
    
    if(usina == 1){
      
      ruido_par[,usina] = as.vector(residuos)
      phi_pm_Y = list(beta_Y)
      phi_pm_X = list(beta_X)
      melhor_comb_final = list(melhor_comb)
      
    }else{
      
      ruido_par[,usina] = as.vector(residuos)
      phi_pm_Y = append(phi_pm_Y,list(beta_Y))
      phi_pm_X = append(phi_pm_X,list(beta_X))
      melhor_comb_final = append(melhor_comb_final,list(melhor_comb))
      
    }
    
    for(mes in 1:12){
      
      media_residuos[mes,usina] = mean(residuos[cycle(residuos) == mes])
      var_a[mes,usina] = var(residuos[cycle(residuos) == mes])
      
    }
    
  }  
  
  
  #Gerando matriz de cenários (num_cenarios x num_periodos)
  for(usina in 1:ncol(dados_Y)){
    
    usina_aux = dados_Y[,usina]
    aux_cenario = dados_Y[(length(dados_Y)-11):length(dados_Y)]
    
    aux_X = c(dados_X[(length(dados_X)-11):length(dados_X)],dados_X_out)
    
    cenario_fwd = matrix(NA, ncol = num_periodos, nrow = num_cenarios)
    cenario_fwd = cbind(matrix(rep(aux_cenario,num_cenarios),ncol=12,byrow=T),cenario_fwd)
    rownames(cenario_fwd) = paste("Cenario",c(1:num_cenarios))
    colnames(cenario_fwd) = rep(1:12,ncol(cenario_fwd)/12)
    
    #Gerando os cenários
    for(cen in 1:num_cenarios){
      for(per in 13:(num_periodos+12)){
        
        mes = as.numeric(colnames(cenario_fwd)[per])
        W = rnorm(1,mean=0,sd=1) 
        ordem_Y_aux = melhor_comb_final[[usina]][mes,1]
        ordem_X_aux = melhor_comb_final[[usina]][mes,2]
        
        phi_Y = phi_pm_Y[[usina]][mes,1:ordem_Y_aux]
        phi_Y <- rev(phi_Y) # to change the order
        
        phi_X = phi_pm_X[[usina]][mes,1:(ordem_X_aux+1)]
        phi_X <- rev(phi_X) # to change the order
        
        mes_aux_Y = as.numeric(colnames(cenario_fwd)[(per-ordem_Y_aux):(per-1)])
        mes_aux_X = as.numeric(colnames(cenario_fwd)[(per-ordem_X_aux):per])
        
        z_chapeu = phi_Y %*% ((cenario_fwd[cen,(per-ordem_Y_aux):(per-1)]-dados_media_Y[mes_aux_Y,usina])/dados_desvio_Y[mes_aux_Y,usina])+
          phi_X %*% ((aux_X[(per-ordem_X_aux):(per)]-dados_media_X[mes_aux_X])/dados_desvio_X[mes_aux_X])
        delta = - ((dados_media_Y[mes,usina]/dados_desvio_Y[mes,usina]) + z_chapeu)
        teta = 1 + (var_a[mes,usina]/((media_residuos[mes,usina] - delta)^2))
        sigma_csi = sqrt(log(teta))
        mi_csi = log(sqrt(var_a[mes,usina])/sqrt(teta*(teta-1)))
        a = exp((W*sigma_csi) + mi_csi) + delta
        
        cenario_fwd[cen,per] = dados_media_Y[mes,usina]+z_chapeu*dados_desvio_Y[mes,usina]+dados_desvio_Y[mes,usina]*a
      }
      #print(cen)
    }
    
    if(usina == 1){cenario_fwd_list = list(cenario_fwd[,-c(1:12)])}else{
      cenario_fwd_list = append(cenario_fwd_list,list(cenario_fwd[,-c(1:12)]))
    }
    
  }  
  
  # out = list(Cenarios = cenario_fwd[,-c(1:12)],
  #            "p(v)" = melhor_comb[,1], "m(v)" = melhor_comb[,2],
  #            "phi(v)" = beta_Y, "theta(v)" = beta_X)
  
  cenario_fwd_list  
}




 




# PARX(p,m) - Cov
# Função para selecionar os parametros considerando a covariância entre os dados
PARX_melhor_p_m_cov = function(dados1,dados2,dados1_valida,max_dados1=1,dados2_valida,max_p,max_m,num_cenarios_in=500){
  
  dados1_novo = window(dados1,start=start(dados2))
  
  erros_p_m = matrix(NA,ncol=max_m+1,nrow=max_p)
  
  for(m in 0:max_m){
    for(p in 1:max_p){
      
      fit = cenarios_parX_LogNormal_cov(dados1_novo, dados2, dados2_valida, p, m, num_cenarios_in)
      
      for (usina in 1:ncol(dados1)){
        
      prev = colMeans(fit[[usina]])*max_dados1[usina]
      
      }
      
      
      erros_p_m[p,m+1] = MAE(dados1_valida,prev)
      
    }
  }
  
  erros_finais = matrix(NA,ncol=2,nrow=1,dimnames=list(1,c("Melhor_p","Melhor_m")))
  aux_min = which(erros_p_m == min(erros_p_m),arr.ind=T)
  erros_finais[1,1] = aux_min[1,1]
  erros_finais[1,2] = aux_min[1,2] - 1
  
  return(list(erros_finais,erros_p_m))
  
}








# Função de simulação considerando a covariância entre os dados
cenarios_parX_LogNormal_cov = function(dados_Y, dados_X, dados_X_out, max_p, max_m, num_cenarios=2000){
  
  num_periodos = length(dados_X_out)
  
  dados_media_Y = dados_desvio_Y = var_a = media_residuos = matrix(NA, nrow = 12, ncol = ncol(dados_Y)) 
  dados_media_X = dados_desvio_X = NULL
  
  dados_norm_Y = ruido_par = dados_Y
  ruido_par = ruido_par[-c(1:12),] #removendo o primeiro ano, por causa da var. explicativa
  dados_norm_X = dados_X
  
  for(mes in 1:12){
    linhas = which(cycle(dados_norm_X) == mes)
    
    dados_media_X[mes] = mean(dados_X[linhas])
    dados_desvio_X[mes] = sd(dados_X[linhas])
    dados_norm_X[linhas] = (dados_X[linhas] - dados_media_X[mes])/dados_desvio_X[mes]
  }
  
  for(usina in 1:ncol(dados_Y)){
    usina_aux_Y = dados_Y[,usina]
    
    #Obtendo as médias e variâncias por mês, por usina
    for(mes in 1:12){
      linhas = which(cycle(usina_aux_Y) == mes)
      
      dados_media_Y[mes,usina] = mean(usina_aux_Y[linhas])
      dados_desvio_Y[mes,usina] = sd(usina_aux_Y[linhas])
      dados_norm_Y[linhas,usina] = (usina_aux_Y[linhas] - dados_media_Y[mes,usina])/dados_desvio_Y[mes,usina]
    }
    
    #Ajustando a série com PARX e encontrando a melhor combinação de p & m, por usina
    
    matrix_aux = matrix(NA,ncol=max_m+1,nrow=max_p,dimnames=list(paste("p=",1:max_p,sep=""),paste("m=",0:max_m,sep="")))
    
    val_MAPE = val_BIC = list(matrix_aux,matrix_aux,matrix_aux,matrix_aux,matrix_aux,matrix_aux,
                              matrix_aux,matrix_aux,matrix_aux,matrix_aux,matrix_aux,matrix_aux)
    
    melhor_comb = matrix(NA,ncol=2,nrow=12)
    colnames(melhor_comb) = c("p","m")
    rownames(melhor_comb) = 1:12
    
    for(mes in 1:12){
      
      z_v = dados_norm_Y[,usina][cycle(dados_norm_Y[,usina]) == mes]
      N = length(z_v)
      z_v = z_v[-1]
      
      for(p in 1:max_p){
        
        elementos = seq(mes-p,(N-1)*12+mes-p,12)
        
        if(p == 1){
          aux_Y_v = dados_norm_Y[,usina][elementos[elementos > 0]]
          if(length(aux_Y_v) != length(z_v)){
            Y_v = aux_Y_v[-1]
          }else{
            Y_v = aux_Y_v
          }
        }else{
          aux_Y_v = dados_norm_Y[,usina][elementos[elementos > 0]]
          if(length(aux_Y_v) != length(z_v)){
            Y_v = cbind(Y_v,aux_Y_v[-1])
          }else{
            Y_v = cbind(Y_v,aux_Y_v)
          }
        }
        
        for(m in 0:max_m){
          
          elementos = seq(mes-m,(N-1)*12+mes-m,12)
          
          if(m == 0){
            aux_X_v = dados_norm_X[elementos[elementos > 0]]
            if(length(aux_X_v) != length(z_v)){
              X_v = aux_X_v[-1]
            }else{
              X_v = aux_X_v
            }
          }else{
            aux_X_v = dados_norm_X[elementos[elementos > 0]]
            if(length(aux_X_v) != length(z_v)){
              X_v = cbind(X_v,aux_X_v[-1])
            }else{
              X_v = cbind(X_v,aux_X_v)
            }
          }
          
          Z_v = cbind(Y_v,X_v)
          
          #      linear_dependent_columns <- which(apply(Z_v, 2, function(col) length(unique(col)) == 1))
          #      any(is.na(Z_v)) || any(is.infinite(Z_v)) || any(is.na(z_v)) || any(is.infinite(z_v))
          
          beta = solve(t(Z_v)%*%Z_v)%*%t(Z_v)%*%z_v
          z_chapeu = Z_v%*%beta
          e_v = z_v-z_chapeu
          BIC_v = log(var(e_v))+log(N)/N*(p+m)
          val_BIC[[mes]][p,m+1] = BIC_v
          val_MAPE[[mes]][p,m+1] = sum(abs(e_v/z_v))
        }
      }
      
      #the model with the lowest BIC is preferred
      aux_BIC = which(val_BIC[[mes]] == min(val_BIC[[mes]]),arr.ind = T)
      melhor_comb[mes,1] = aux_BIC[1,1]
      melhor_comb[mes,2] = aux_BIC[1,2]-1
    }
    
    #Ajustando a série com PARX e obtendo resíduos, por usina
    
    # Betas
    beta_Y = matrix(NA,nrow=12,ncol=max_p)
    beta_X = matrix(NA,nrow=12,ncol=max_m+1)
    Y_chapeu_ts = residuos = matrix(NA,nrow=12,ncol=nrow(dados_Y)/12-1)
    
    for(mes in 1:12){
      
      z_v = dados_norm_Y[,usina][cycle(dados_norm_Y[,usina]) == mes]
      N = length(z_v)
      z_v = z_v[-1]
      
      for(p in 1:melhor_comb[mes,1]){
        elementos = seq(mes-p,(N-1)*12+mes-p,12)
        if(p == 1){
          aux_Y_v = dados_norm_Y[,usina][elementos[elementos > 0]]
          if(length(aux_Y_v) != length(z_v)){
            Y_v = aux_Y_v[-1]
          }else{
            Y_v = aux_Y_v
          }
        }else{
          aux_Y_v = dados_norm_Y[,usina][elementos[elementos > 0]]
          if(length(aux_Y_v) != length(z_v)){
            Y_v = cbind(Y_v,aux_Y_v[-1])
          }else{
            Y_v = cbind(Y_v,aux_Y_v)
          }
        }
      }
      
      for(m in 0:melhor_comb[mes,2]){
        elementos = seq(mes-m,(N-1)*12+mes-m,12)
        if(m == 0){
          aux_X_v = dados_norm_X[elementos[elementos > 0]]
          if(length(aux_X_v) != length(z_v)){
            X_v = aux_X_v[-1]
          }else{
            X_v = aux_X_v
          }
        }else{
          aux_X_v = dados_norm_X[elementos[elementos > 0]]
          if(length(aux_X_v) != length(z_v)){
            X_v = cbind(X_v,aux_X_v[-1])
          }else{
            X_v = cbind(X_v,aux_X_v)
          }
        }
      }
      
      Z_v = cbind(Y_v,X_v)
      beta = solve(t(Z_v)%*%Z_v)%*%t(Z_v)%*%z_v
      
      beta_Y[mes,1:melhor_comb[mes,1]] = beta[1:melhor_comb[mes,1]]
      beta_X[mes,1:(melhor_comb[mes,2]+1)] = beta[(melhor_comb[mes,1]+1):nrow(beta)]
      
      z_chapeu = Z_v %*% beta
      Y_chapeu = z_chapeu*dados_desvio_Y[mes]+dados_media_Y[mes]
      e_v = z_v-z_chapeu
      Y_chapeu_ts[mes,] = Y_chapeu
      
      residuos[mes,] = e_v
    }
    
    Y_chapeu_ts = ts(c(Y_chapeu_ts),end=end(dados_Y[,usina]),freq=frequency(dados_Y[,usina]))
    residuos = ts(c(residuos),end=end(dados_Y[,usina]),freq=frequency(dados_Y[,usina]))
    
    # terminou o ajuste via PARX
    
    if(usina == 1){
      
      ruido_par[,usina] = as.vector(residuos)
      phi_pm_Y = list(beta_Y)
      phi_pm_X = list(beta_X)
      melhor_comb_final = list(melhor_comb)
      
    }else{
      
      ruido_par[,usina] = as.vector(residuos)
      phi_pm_Y = append(phi_pm_Y,list(beta_Y))
      phi_pm_X = append(phi_pm_X,list(beta_X))
      melhor_comb_final = append(melhor_comb_final,list(melhor_comb))
      
    }
    
    for(mes in 1:12){
      
      media_residuos[mes,usina] = mean(residuos[cycle(residuos) == 1])
      var_a[mes,usina] = var(residuos[cycle(residuos) == mes])
      
    }
    
  }  
  
  
# str(ruido_par)
ruido_par <- ts(ruido_par, start=1980, freq=12)
  
  
  # Covariância  

  for(mes in 1:12){
    
    cov_mes = cor(ruido_par[cycle(ruido_par) == mes,])
    
    decomp_espect = eigen(cov_mes)
    
    D_aux = decomp_espect$vectors%*%((diag(ncol(dados_Y))*decomp_espect$values)^(1/2))
    
    if(mes == 1){
      
      D = list(D_aux)
      
    }else{
      
      D = append(D,list(D_aux))
      
    }
    
  }
  
  
  
  #Gerando matriz de cenários (num_cenarios x num_periodos)
  for(usina in 1:ncol(dados_Y)){
    
    usina_aux = dados_Y[,usina]
    aux_cenario = dados_Y[(length(dados_Y)-11):length(dados_Y)]
    
    aux_X = c(dados_X[(length(dados_X)-11):length(dados_X)],dados_X_out)
    
    cenario_fwd = matrix(NA, ncol = num_periodos, nrow = num_cenarios)
    cenario_fwd = cbind(matrix(rep(aux_cenario,num_cenarios),ncol=12,byrow=T),cenario_fwd)
    rownames(cenario_fwd) = paste("Cenario",c(1:num_cenarios))
    colnames(cenario_fwd) = rep(1:12,ncol(cenario_fwd)/12)
    
    #Gerando os cenários
    for(cen in 1:num_cenarios){
      for(per in 13:(num_periodos+12)){
        
        mes = as.numeric(colnames(cenario_fwd)[per])
        # W = rnorm(1,mean=0,sd=1) 
        
        W = D[[mes]]%*%rnorm(ncol(dados_Y),0,1)                                      # Covâriancia
        ordem_Y_aux = melhor_comb_final[[usina]][mes,1]
        ordem_X_aux = melhor_comb_final[[usina]][mes,2]
        
        phi_Y = phi_pm_Y[[usina]][mes,1:ordem_Y_aux]
        phi_Y <- rev(phi_Y) # to change the order
        
        phi_X = phi_pm_X[[usina]][mes,1:(ordem_X_aux+1)]
        phi_X <- rev(phi_X) # to change the order
        
        mes_aux_Y = as.numeric(colnames(cenario_fwd)[(per-ordem_Y_aux):(per-1)])
        mes_aux_X = as.numeric(colnames(cenario_fwd)[(per-ordem_X_aux):per])
        
        z_chapeu = phi_Y %*% ((cenario_fwd[cen,(per-ordem_Y_aux):(per-1)]-dados_media_Y[mes_aux_Y,usina])/dados_desvio_Y[mes_aux_Y,usina])+
          phi_X %*% ((aux_X[(per-ordem_X_aux):(per)]-dados_media_X[mes_aux_X])/dados_desvio_X[mes_aux_X])
        delta = - ((dados_media_Y[mes,usina]/dados_desvio_Y[mes,usina]) + z_chapeu)
        teta = 1 + (var_a[mes,usina]/((media_residuos[mes,usina] - delta)^2))
        sigma_csi = sqrt(log(teta))
        mi_csi = log(sqrt(var_a[mes,usina])/sqrt(teta*(teta-1)))
        a = exp((W[usina,1]*sigma_csi) + mi_csi) + delta                            # Covâriancia
        
        cenario_fwd[cen,per] = dados_media_Y[mes,usina]+z_chapeu*dados_desvio_Y[mes,usina]+dados_desvio_Y[mes,usina]*a
      }
      #print(cen)
    }
    
    if(usina == 1){cenario_fwd_list = list(cenario_fwd[,-c(1:12)])}else{
      cenario_fwd_list = append(cenario_fwd_list,list(cenario_fwd[,-c(1:12)]))
    }
    
  }  
  
  # out = list(Cenarios = cenario_fwd[,-c(1:12)],
  #            "p(v)" = melhor_comb[,1], "m(v)" = melhor_comb[,2],
  #            "phi(v)" = beta_Y, "theta(v)" = beta_X)
  
  cenario_fwd_list  
}









### MEDIDAS DE ERRO PARA AVALIAÇÃO DO MODELO
RMSE = function(yt,ft){
  RMSE = sqrt(mean((yt-ft)^2))
  return(RMSE)
}


MAE = function(yt,ft){
  MAE = mean(abs(yt - ft))
  return(MAE)
}


R2 = function(yt, ft) {
  ss_res <- sum((yt - ft)^2)
  ss_tot <- sum((yt - mean(yt))^2)
  R2 = 1 - ( ss_res / ss_tot )
  return(R2)
}



# MSRE = function(yt,ft){
#   MSRE = mean(((ft-yt)/yt)^2)
#   return(MSRE)
# }
# 
# MARE = function(yt,ft){
#   MARE = mean(abs((ft-yt)/yt))
#   return(MARE)
# }
# 
# NSC = function(yt,ft){
#   y_barra = mean(yt)
#   numerador = denominador = 0
#   for(i in 1:length(yt)){
#     numerador = numerador + (yt[i] - ft[i])^2
#     denominador = denominador + (yt[i] - y_barra)^2
#   } 
#   NSC = 1 - numerador/denominador
#   return(NSC)
# }




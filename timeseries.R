library(tseries)
setwd("C:\\Users\\ferreas\\Desktop\\Trab. Pós- Graduação\\Exercicio em sala\\Series Temporais");
serie2 <- read.ts("serie7.txt", header = FALSE, sep = "", skip = 0)

acfplot(serie)
estacionaria = adf.test(serie)

par(mfrow=c(1,2))
acf(serie2)
pacf(serie2)



calc_model_serie_temp = function(dados, confianca){
  #dados = serie2;
  
  estacionaria = adf.test(dados)
  if(estacionaria > 0.10)
  {
    cat(sprint("A serie não é estacionaria. \n"));
    
    #IMPLEMENTAR = DIFF()
  }
  else
  {
    #dados = serie2
    res_lag = c();
    res_lag_val = c();
    
    res_lag_pacf = c();
    res_lag_val_pacf = c();
    
    acf_=acf(dados);
    pacf_=pacf(dados);
    
    ic_max_acf_=qnorm((1+0.95)/2)/sqrt(acf_$n.use);
    ic_min_acf_=-qnorm((1+0.95)/2)/sqrt(acf_$n.used);
    ic_max_pacf_=qnorm((1+0.95)/2)/sqrt(pacf_$n.use);
    ic_min_pacf_=-qnorm((1+0.95)/2)/sqrt(pacf_$n.used);
    
    # Verifica se é um auto-regressivo
    for(i in 1:length(acf_$acf))
    {      
      #cat(sprintf("Lim Max %f\n", ic_max_acf_))
      #cat(sprintf("Valor - %f\n",acf_$acf[i]))
      if(acf_$acf[i]<ic_min_acf_ || acf_$acf[i]>ic_max_acf_)
      {
        res_lag = append(res_lag, acf_$lag[i]);
        #res_lag_val = append(res_lag_val, acf_$acf[i]);
      }
    }
    # Verifica o tipo de AR
    if(length(res_lag) > 0)
    {      
      for(i in 1:length(pacf_$acf))
      {
        if(pacf_$acf[i]<ic_min_pacf_ || pacf_$acf[i]>ic_max_pacf_)
        {
          res_lag_pacf = append(res_lag_pacf, pacf_$lag[i]);
          res_lag_val_pacf = append(res_lag_val_pacf, pacf_$acf[i]);
        }    
      }
    }        
    
    
    
  #**************************************************************************** 
    #Realiza o teste
    #arima_ = arima(serie, order = c(max(res_lag_pacf),0,0) );
    test_arima <- function(lista_lag, lista_pacf, includemean)
    {
      #lista_lag = res_lag_pacf
      #includemean = 0
      modelo = c();
      modelo2 = c();
      modelo_resultado = "";
      repeat
      {              
          cat(sprintf("Inicia o teste.\n"));
          if(length(lista_lag)==max(lista_lag))
          {
            tipo = "completo";
            #AR completo - considerando a media
            if(includemean == 0)
            {
              cat(sprintf("Testa com a media - Arima\n"));
              modelo2 = arima(dados, order = c(max(lista_lag),0,0));
              modelo_resultado = modelo2;
            }
            else
            {
              cat(sprintf("Sem media - Arima\n"));
              modelo2 = arima(dados, order = c(max(lista_lag),0,0), include.mean=FALSE);
              modelo_resultado = modelo2;
            }
          }
          else
          {
            tipo = "incompleto";
            #AR incompleto - sem considerar a media
            if(includemean == 0)
            {
              cat(sprintf("Com a media - Arma\n"));
              modelo = arma(dados, order = c(1,1), lag = list(ar=lista_lag));  
              modelo_resultado = modelo;
            }
            else
            {
              cat(sprintf("Sem media - Arma\n"));
              modelo = arma(dados, order = c(1,1), lag = list(ar=lista_lag), include.intercept = FALSE);          
              modelo_resultado = modelo;
            }
          }
          lista_lag_FLAG = modelo_resultado$coef;
          
          arima_coef = c();
          arima_se = c();
          arima_result = c();
          intercept_result = FALSE;
          
          cat(sprintf("=====Calcula o valor para testar a confiança====="));
          if(tipo=="incompleto")
          {            
            for(i in 1:length(modelo$coef))
            {
              arima_coef = append(arima_coef, modelo$coef[i]);
              arima_se = append(arima_se, sqrt(modelo$vcov[i,i]));
              arima_result = append(arima_result, modelo$coef[i]/sqrt(modelo$vcov[i,i]));#arima_$var.coef
            }
          }
          else
          {           
            for(i in 1:length(modelo2$coef))
            {              
              arima_coef = append(arima_coef, modelo2$coef[i]);
              arima_se = append(arima_se, sqrt(modelo2$var.coef[i,i]));
              arima_result = append(arima_result, modelo2$coef[i]/sqrt(modelo2$var.coef[i,i]));#arima_$var.coef
            }
          }
          cat(sprintf("\n"));
          
          cat(sprintf("==== Valor para testar a confiança ====\n"));
          for(i  in 1:length(arima_result))
          {            
            cat(sprintf("%f = ",arima_result[i] ));
            cat(sprintf("%f / ",arima_coef[i] ));
            cat(sprintf("%f",arima_se[i] ));
            cat(sprintf("\n"));
          }
          cat(sprintf("\n"));
          
          lista_cand_sair = c();
          lista_cand_sair_ind = c();
          
          #Testa os valores dentro do intervalo de confiança
           for(i in 1:length(arima_result))
           {
             #testa a media
             if(includemean == 0)
             {
               count_ = length(arima_result);
               cat(sprintf("Teste para retirar a media %i  \n", count_));
               if(arima_result[count_] > -1.64 && arima_result[count_] < 1.64)
               {
                 cat(sprintf("Retira media \n"));
                 includemean = 1;
                 lista_lag = lista_lag[-count_];
                 lista_pacf = lista_pacf[-count_];
                 break;
               }
             }           
               cat(sprintf("Verifica confiança\n"));               
               if(arima_result[i] > -1.64 && arima_result[i] < 1.64)
               {
                 cat(sprintf("Retira valor e executa recursividade\n"));
                 lista_cand_sair = append(lista_cand_sair, abs(abs(arima_result[i]) - 1.64));             
                 lista_cand_sair_ind = append(lista_cand_sair_ind, i);                 
               }             
           }
          
          if(length(lista_cand_sair)>0)
          { 
            cat(sprintf("Lag a ser retirado - lista %f\n", lista_cand_sair));
            cat(sprintf("Lag a ser retirado %f\n - ", lista_lag[lista_cand_sair_ind[which(max(lista_cand_sair)==lista_cand_sair)]]))
            cat(sprintf("Lag a ser retirado - Valor %f\n - ", lista_cand_sair[which(max(lista_cand_sair)==lista_cand_sair)]));
            cat(sprintf("\n"));
            lista_lag = lista_lag[-lista_cand_sair_ind[which(max(lista_cand_sair)==lista_cand_sair)]];
            lista_pacf = lista_pacf[-lista_cand_sair_ind[which(max(lista_cand_sair)==lista_cand_sair)]];
          }
          
          cat(sprintf("Dados na lista_lag %f\n", lista_lag));
          cat(sprintf("Dados na lista_lag_FLAG %f\n", lista_lag_FLAG));
        if(length(lista_lag)==(if(includemean == 0){length(lista_lag_FLAG)-1}else{length(lista_lag_FLAG)})){break;}
      }
      
      
      return (list(mode=modelo_resultado, lags=lista_lag));
    }  
  #****************************************************************************  
    
    #ANALISE DOS RESIDUOS       
    repeat
    {     
      resultado = c();
      new_call = "false";      
      modelo_result3 = c();
      modelo_result2 = test_arima(res_lag_pacf, res_lag_val_pacf,0);
      modelo_result3 = modelo_result2$mode;
      
      res_lag_pacf = c();     
      for(i in 1:length(modelo_result3$coef))
      {
        cat(sprintf("Analisar os residuos\n"));
        cat(sprintf("%f - ", modelo_result3$coef[i]));              
        cat(sprintf("\n"));
      }
      
      #Definir se é um AR completo ou incompleto
      #if(length(res_lag_pacf)==max(res_lag_pacf))
      if(length(modelo_result2$lags)==max(modelo_result2$lags))
      {
        cat(sprintf("Testa residuos completo\n"));
        #Implementar a analise dos residuos - AR COMPLETO
        par(mfrow=c(1,2))
        res_acf = acf(residuals(modelo_result3))
        res_pacf = pacf(residuals(modelo_result3))
        
        ic_max_resacf_=qnorm((1+0.95)/2)/sqrt(res_acf$n.use);
        ic_min_resacf_=-qnorm((1+0.95)/2)/sqrt(res_acf$n.used);
        ic_max_respacf_=qnorm((1+0.95)/2)/sqrt(res_pacf$n.use);
        ic_min_respacf_=-qnorm((1+0.95)/2)/sqrt(res_pacf$n.used); 
        
        for(i in 1:length(res_pacf$acf))
        {
          if(res_pacf$acf[i]<ic_min_respacf_ || res_pacf$acf[i]>ic_max_respacf_)
          {
            new_call = "true";
            res_lag_pacf = append(res_lag_pacf, res_pacf$lag[i]);
            res_lag_val_pacf = append(res_lag_val_pacf, res_pacf$acf[i]);
          }    
        }

        for(i in 1:length(modelo_result3$coef))
        {
          resultado = append(resultado, modelo_result3$coef[i])       
        }
        cat(sprintf("Lag apos analise dos residuos completo: %s\n", res_lag_pacf));
      }
      else
      {
        cat(sprintf("Testa residuos incompleto\n"));
        #Implementar a analise dos residuos - AR INCOMPLETO
        res <- modelo_result3$res;
        cat(sprintf("Tamanho dos residuos %f\n", length(res)));
        n <- length(res);
        par(mfrow=c(1,2))
        cat(sprintf("Max result lags %f\n", max(modelo_result2$lags) +1));
        
        res_acf = acf(res[(max(modelo_result2$lags)+1):length(res)])
        res_pacf = pacf(res[(max(modelo_result2$lags)+1):length(res)])
        
        #res_acf = acf(res[max(modelo_result2$lags)+1:n])
        #res_pacf = pacf(res[max(modelo_result2$lags)+1:n])
        cat(sprintf("Defini IC para teste incompleto de residuos\n"));
        
        ic_max_resacf_=qnorm((1+0.95)/2)/sqrt(res_acf$n.use);
        ic_min_resacf_=-qnorm((1+0.95)/2)/sqrt(res_acf$n.used);
        ic_max_respacf_=qnorm((1+0.95)/2)/sqrt(res_pacf$n.use);
        ic_min_respacf_=-qnorm((1+0.95)/2)/sqrt(res_pacf$n.used);      
        
        for(i in 1:length(res_pacf$acf))
        {
          if(res_pacf$acf[i]<ic_min_respacf_ || res_pacf$acf[i]>ic_max_respacf_)
          {
            new_call = "true";
            res_lag_pacf = append(res_lag_pacf, res_pacf$lag[i]);
            res_lag_val_pacf = append(res_lag_val_pacf, res_pacf$acf[i]);
          }
        }
        
        for(i in 1:length(modelo_result3$coef))
        {
          resultado = append(resultado, modelo_result3$coef[i])       
        }
        cat(sprintf("Lag apos analise dos residuos incompleto: %s\n", res_lag_pacf));
      }
      
      if(new_call == "false"){break;};
    }
    
    #resultado;
    result_final=""; 
    
    for(i in 1:length(modelo_result2$lags))
    {
      result_final = append(result_final, paste(modelo_result3$coef[i], paste("t-",modelo_result2$lags[i])))      
    }
    
    result_final;
    
    #modelo_result2$lags
    #modelo_result3$coef[i]
    
    #cat(sprintf("Fim da execução - END\n"));    
  }
}

debug(calc_model_serie_temp, text = "", condition = NULL)
calc_model_serie_temp(serie2)
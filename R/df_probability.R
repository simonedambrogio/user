df_probability <- function(data, optA_optB, range = c(-20, 20), step){
  
  data$optA_optB <- data[, optA_optB ]
  
  se <- function(x) sqrt(var(x)/length(x))
  
  if(sum(abs(range))%%step == 1){ print("Error, step must be a divisor of range" )
    } else{
      group_diff <- NA
      for(  i in seq(range[1], range[2], step)){
        if (i < 0){
          for(n in i:(i+(step-1)) ){ data[which( data$optA_optB == n), 'optA_optB'] <- i}
        } else if (i == 0){
          data[which( data$optA_optB == i), 'optA_optB'] <- i
        } else if (i > 0){
          for(n in (i-(step-1)):i ){data[which( data$optA_optB == n), 'optA_optB'] <- i}
        }
      }}
    
    exp_0 <- as.data.frame(data)
    
    if ( !all(unique(exp_0$choice) ==  c(-1,1) | unique(exp_0$choice) ==  c(1,-1)) ){
      if ( all(unique(exp_0$choice) ==  c(0,1) | unique(exp_0$choice) ==  c(1,0)) ){
        exp_0[exp_0$choice == 0, 'choice'] <- -1 
      } else { print('la variabile choice dev essere codificata con -1 e 1 oppure con 0 e 1. Non altre codifiche sono ammesse')}}
    
    if ( any(names(data) == 'parcode') | any(names(data) == 'sbj')  ){
      print('la colonna che indica i soggetti deve chiamarsi subject!!')
      break()
    } 
    
    do.call(rbind.data.frame,
            lapply(sort(unique(exp_0$optA_optB)), function(optA_optB_i){
              
              #Si riferisce al numero di TRUE, ossia di acquisti
              n_acquisti_log <-exp_0[exp_0$optA_optB == optA_optB_i, 'choice'] == 1
              
              n_acquisti <- length(n_acquisti_log[n_acquisti_log == TRUE])
              n_totale <- length(exp_0[exp_0$optA_optB == optA_optB_i, 'choice'] == 1) 
              #TRUE corrisponde agli acquisti di acquisti
              n_acquisti_log <-exp_0[exp_0$optA_optB == optA_optB_i, 'choice'] == 1 
              
              n_acquisti <- length(n_acquisti_log[n_acquisti_log == TRUE])
              n_totale <- length(exp_0[exp_0$optA_optB == optA_optB_i, 'choice'] == 1) 
              
              
              data.frame(probability = n_acquisti/n_totale,
                         optA_optB = optA_optB_i)
            })) 
    #%>%
    #  mutate(#STANDARD 
    #    ERROR st_err = sapply( sort(unique(exp_0$optA_optB)), function(optA_optB_i){
    #      prob_optA_optB_i <- sapply(exp_0$subject, function(sbj_ok_i){
    #        n_acquisti_log <-exp_0[exp_0$optA_optB == optA_optB_i &
    #                                 #Si riferisce al numero di TRUE, ossia di acquisti
    #                                 exp_0$subject ==  sbj_ok_i, 'choice'] == 1
    #        n_acquisti <- length(n_acquisti_log[n_acquisti_log == TRUE])
    #        n_totale <- length(exp_0[exp_0$optA_optB == optA_optB_i &
    #                                   exp_0$subject ==  sbj_ok_i, 'choice'] == 1)
    #        if (n_totale == 0){NA}else{n_acquisti/n_totale}
    #      })
    #      st_err = se(na.omit(prob_optA_optB_i))
    #    }))
  }


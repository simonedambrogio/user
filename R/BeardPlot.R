
BeardPlot <- function(data, y ,foltezza_barba = 40, group = NA , outlier = FALSE, distanza = 1, smile = FALSE, color = 'black'){

  IQR.outliers <- function(x) {
    if(any(is.na(x)))
      stop("x is missing values")
    if(!is.numeric(x))
      stop("x is not numeric")
    Q3<-quantile(x,0.75)
    Q1<-quantile(x,0.25)
    IQR<-(Q3-Q1)
    left<- (Q1-(1.5*IQR))
    right<- (Q3+(1.5*IQR))
    c(left, right)
  }
  
  if (is.na(group)){
    
                # Tutti i dati
                
                dens <- data %>% pull(y) %>% density()
              
                ddf <- data.frame(X = dens$x,
                                  density = dens$y)
              
                ddf_discrete <- ddf[seq(1, nrow(ddf), nrow(ddf)/foltezza_barba), ]
              
                iqr <- data %>% pull(y) %>% IQR.outliers()
                
                if (outlier == FALSE){
                box <- geom_boxplot(aes(y = data %>% pull(y)),
                             width = .2 * max(ddf$density),
                             position = position_nudge(x = 0),
                             color = color, 
                             outlier.shape = NA)
                
                seg <- geom_segment(data = ddf_discrete[ddf_discrete$X > iqr[1] &
                                                          ddf_discrete$X < iqr[2], ],
                                    aes(x=density, xend=0, y=X, yend=X), size = 1,
                                    color = color)
                
                if (smile == TRUE){
                  smile <- geom_path( data = ddf_discrete[ddf_discrete$X > iqr[1] &
                                                            ddf_discrete$X < iqr[2], ],
                                      aes(x=density, y=X,),
                                      color = color)
                } else { smile <- theme_bw() }
                
                }else{
                  box <- geom_boxplot(aes(y = data %>% pull(y)),
                                      width = .2 * max(ddf$density),
                                      position = position_nudge(x = 0),
                                      color = color)
                  
                  seg <- geom_segment(data = ddf_discrete,
                                      aes(x=density, xend=0, y=X, yend=X), size = 1,
                                      color = color)
                  
                  if (smile == TRUE){
                    smile <- geom_path( data = ddf_discrete,
                                        aes(x=density, y=X), 
                                        color = color)
                  } else { smile <- theme_bw() }
                  
                }
              
          p <- ggplot()+
                  seg+
                  box +
                  theme_bw() +
                  coord_flip() + scale_x_reverse() +
                  smile
  
  } else {
    
    densità_gruppi <- function(dati, group, y, foltezza_barba, distanza){
      
      attach(dati)
      
      group_col <- which( names(dati) %in% group )
      y_col <- which( names(dati) %in% y )
      
      lev_group <- data %>% pull(group) %>% unique()
      n_lev_group <- lev_group %>% length()
      
      dentistà_grup <- do.call(rbind.data.frame,
                          lapply(1:n_lev_group, function(i){
                            log_group <- dati[, group_col] == lev_group[i]
                            
                            dens <- dati %>% filter(log_group)  %>% pull(y) %>% density()
                            
                            ddf <- data.frame(X = dens$x,
                                              density = dens$y)
                            
                            ddf_discrete <- cbind(ddf[seq(1, nrow(ddf), nrow(ddf)/foltezza_barba), ], 
                                                  group = lev_group[i])
                          }))
      
      for( i in 1:n_lev_group){
        if (i == 1){
        log_group <- dentistà_grup[, 'group'] == lev_group[i]
        dentistà_grup[log_group, 'position'] <- 0
        dentistà_grup
        } else {
          log_group <- dentistà_grup[, 'group'] == lev_group[i]
          dentistà_grup[log_group, 'position'] <- max(dentistà_grup[!log_group, 'density'])*as.numeric(paste0('1.', distanza))*-1
          dentistà_grup[log_group, 'density'] <-  dentistà_grup[log_group, 'density']-
                                                  (max(dentistà_grup[!log_group, 'density'])*as.numeric(paste0('1.', distanza)))
          dentistà_grup
        }
      }
      
      return(dentistà_grup)
    }
    
    ddf <- densità_gruppi(dati = data, group = group, y = y, foltezza_barba = foltezza_barba, distanza = distanza)
    
    group_col <- which( names(data) %in% group )
    y_col <- which( names(data) %in% y )
    lev_group <- levels(data[, group_col])
    n_lev_group <- length(levels(data[, group_col]))
    
    if (outlier == FALSE){
      
      
      ddf <- do.call(rbind.data.frame, 
                lapply(1:n_lev_group, function(i){
                log_group <- data[, group_col] == lev_group[i]
                righe_da_tenere <- which(
                    ddf[ddf$group == lev_group[i], 'X'] > IQR.outliers(data[log_group, y_col ])[1] &
                    ddf[ddf$group == lev_group[i], 'X'] < IQR.outliers(data[log_group, y_col ])[2])
                ddf <- ddf[ddf$group == lev_group[i], ]
                ddf[righe_da_tenere, ]
                })
                )
      
      
      
      box <- geom_boxplot(data = data, 
                          aes(y = data[, y_col], color = group),
                          width = .2 * max(ddf$density),
                          position = position_nudge( x = abs(unique(ddf$position)) ),
                          outlier.shape = NA)

      
      seg <- geom_segment(data = ddf,
                          aes(x=density, xend=position, y=X, yend=X, color = group), 
                          size = 1)
      
      if (smile == TRUE){
        smile <- geom_path( data = ddf,
                            aes(x=density, y=X, color = group))
      } else { smile <- theme_bw() }

      
    }else{
      box <- geom_boxplot(data = data, 
                          aes(y = data[, y_col], color = group),
                          width = .2 * max(ddf$density),
                          position = position_nudge( x = abs(unique(ddf$position)) ) 
      )
      
      seg <- geom_segment(data = ddf,
                          aes(x=density, xend=position, y=X, yend=X, color = group), 
                          size = 1)
      
      if (smile == TRUE){
        smile <- geom_path( data = ddf,
                            aes(x=density, y=X, color = group))
      } else { smile <- theme_bw() }
      
    }
    
   p <- ggplot()+
          seg +
          box +
          theme_bw() +
          coord_flip() + scale_x_reverse()+
          smile
    
  }
  return(p)
}


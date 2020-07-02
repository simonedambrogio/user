summarise_All <- function(data, measurevar, groupvars, FUN){
  
  ###################### 2 groupvars ######################
  if ( length(groupvars) == 2 ){
    arg1 <- unique(data[, groupvars[1]])
    arg2 <- unique(data[, groupvars[2]])
    df <- user::df_lapply(arg1, function(a1) {
      user::df_lapply(arg2, function(a2) {
        data.frame( var1 = a1, var2 = a2, value = FUN( data[ data[, groupvars[1]]==a1 & data[, groupvars[2]]==a2, measurevar ]) )
      }) })
    names(df)[1:2] <- groupvars
    ###################### 3 groupvars ######################
  } else if ( length(groupvars) == 3 ){
    arg1 <- unique(data[, groupvars[1]])
    arg2 <- unique(data[, groupvars[2]])
    arg3 <- unique(data[, groupvars[3]])
    df <- user::df_lapply(arg1, function(a1) {
      user::df_lapply(arg2, function(a2) {
        user::df_lapply(arg3, function(a3) {
          data.frame( var1 = a1, var2 = a2, var3 = a3, value = FUN( data[ data[, groupvars[1]]==a1 & data[, groupvars[2]]==a2 & groupvars[3]==a3, measurevar ]) )
        }) }) })
    names(df)[1:3] <- groupvars
    ###################### 4 groupvars ######################
  } else if ( length(groupvars) == 4 ){
    arg1 <- unique(data[, groupvars[1]])
    arg2 <- unique(data[, groupvars[2]])
    arg3 <- unique(data[, groupvars[3]])
    arg4 <- unique(data[, groupvars[4]])
    df <- user::df_lapply(arg1, function(a1) {
      user::df_lapply(arg2, function(a2) {
        user::df_lapply(arg3, function(a3) {
          user::df_lapply(arg4, function(a4) {
            data.frame( var1 = a1, var2 = a2, var3 = a3, var4 = a4, value = FUN( data[ data[, groupvars[1]]==a1 & data[, groupvars[2]]==a2 & groupvars[3]==a3 & groupvars[4]==a4, measurevar ]) )
          }) }) }) })
    names(df)[1:4] <- groupvars
  }
  
  return(df)
  
}
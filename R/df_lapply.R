#df_lapply
df_lapply <- function(X, FUN) do.call(rbind.data.frame, lapply(X, FUN))
seq_vector <- function(x){
  idx <- c(which(diff(x) != 0), length(x))
  time <- c(idx[1], diff(idx))
  rep(x=1:length(idx), times=time)
  #mapply(rep, x=1:length(idx), times=time) %>% unlist
}

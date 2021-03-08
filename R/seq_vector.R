seq_vector <- function(x){
  idx <- c(which(diff(x) != 0), length(x))
  time <- c(idx[1], diff(idx))
  m=mapply(rep, x=1:length(idx), times=time) %>% unlist
  if(is.matrix(m)) m=as.vector(matrix(m, ncol = 1))
  return(m)
}
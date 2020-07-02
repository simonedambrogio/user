seq_vector <- function(x){
  idx <- c(0, which(diff(x)<0), length(x))
  lapply(1: c(length(idx)-1), function(i){ 
    x_i <- x[ (idx[i]+1) : idx[i+1]]
    rep(1:length(unique(x_i)), rle(x_i)$lengths)
  }) %>% unlist
}
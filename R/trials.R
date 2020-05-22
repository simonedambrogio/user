trials <- function(x){
  idx <- c(0, which(diff(x)<0), length(x))
  lapply(1: c(length(idx)-1), function(i){ 
    tr_i <- tr[ (idx[i]+1) : idx[i+1]]
    rep(1:length(unique(tr_i)), rle(tr_i)$lengths)
  }) %>% unlist
}
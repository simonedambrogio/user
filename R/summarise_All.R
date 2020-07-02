lapply2 <- function(arg1, arg2, FUN){
  lapply( arg1, function(a1){
    lapply( arg2, function(a2) {
      FUN
    })
  })
}
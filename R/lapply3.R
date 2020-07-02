lapply3 <- function(arg1, arg2, arg3, FUN){
  lapply( arg1, function(a1){
    lapply( arg2, function(a2) {
      lapply( arg3, function(a3) {
        FUN
      })
    })
  })
}
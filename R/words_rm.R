#The words_rm function eliminates the worlds that we want to eliminate
#There are only 4 arguments:
#data: name of the dataset
#variable: character variable that we want to modify. don't forget the quotation marks
#words_to_rm: words that we want to eliminate separated by |
#replace: numbers or words we want instead of words_rm. Default is "" and you should 
#keep it if you want to eliminate the words

words_rm <- function(variable, words_to_rm, replace = ""){

library(tidyverse)

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!is.character(data[, variable])) stop("The arg variable must be a character!")

new_number <- sapply(1:length(data[, variable][str_detect(data[, variable], words_to_rm)]), 
                     function(i){
                       data[, variable][str_detect(data[, variable], words_to_rm)][i] <- 
                         gsub(words_to_rm, replace, data[, variable][str_detect(data[, variable], words_to_rm)][i])
                     })

data[str_detect(data[, variable], words_to_rm),variable] <- new_number
return(data[, variable])
}

#words_rm(data = dec, variable = "value_up_boundary", words_to_rm = "\\+")

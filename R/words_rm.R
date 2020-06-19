#The words_rm function eliminates the worlds that we want to eliminate
#There are only 4 arguments:
#data: name of the dataset
#variable: character variable that we want to modify. don't forget the quotation marks
#words_to_rm: words that we want to eliminate separated by |
#replace: numbers or words we want instead of words_rm. Default is "" and you should 
#keep it if you want to eliminate the words

words_rm <- function (data, variable, words_to_rm, replace = "") {
  
  library(dplyr)
  if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
  }
  
  variable <- enquo(variable)
  var <- select(data, !!variable) %>% .[,]
  
  if(!is.character(var)) var <- as.character(var)
  
  for (word_i in 1:length(words_to_rm)) {
    word_to_rm_logic <- str_detect(var, fixed(words_to_rm[word_i]))
    word_to_rm <- words_to_rm[word_i]
    new_words <- sapply(1:sum(word_to_rm_logic), function(i) var[word_to_rm_logic][i] <- gsub(word_to_rm, replace, var[word_to_rm_logic][i], fixed = TRUE))
    var[word_to_rm_logic] <- new_words
  }
  
  return(var)
}




#words_rm(data = dec, variable = value_up_boundary, words_to_rm = "\\+")

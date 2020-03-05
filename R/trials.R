trials <- function(data) {
  
  if( sum(names(data) %in% c("subject", "trial")) < 2 ){ "The dataset must contain the variables subject and trial"}
  
  #total number of trials
  tot_trials <- sapply(unique(data$subject), function(sbj)  length(unique(data[data$subject == sbj, "trial"])) ) %>% sum()
  
  #Create a variable trial  
  trials <- rep(1:tot_trials, 
                data %>% 
                  group_by(subject, trial) %>% 
                  summarise(n()) %>% .[, 3] %>% 
                  unlist() %>% as.double())
  return(trials)
}
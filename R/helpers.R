library(tidyverse)

get_all_colnames_sorted <- function(...){
  # given data frames return unique column names (sorted)
  all <- c()
  for(i in list(...)) {
    all <- c(all,unique(colnames(i)))
  }
  unique(all) %>% sort()
}

create_empty_numeric_dataframe <- function(cnames){
  df <- data.frame(matrix(0, ncol = cnames %>% length, nrow = 1)) %>%
    head(0)
  colnames(df) <- cnames
  return(df)
}

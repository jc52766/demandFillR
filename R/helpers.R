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
  df <- data.frame(matrix(0, ncol = cnames %>% length, nrow = 0))
  colnames(df) <- cnames
  return(df)
}

force_all_options <- function(df, cols_needed){
  # you may get demand for groups a,b,c but options may exist for d,e,f also
  # e.g. d,e,f products are unavoidably produced regardless of where demand lays
  # so then we want to force demand df that has columns: a,b,c to have all: a,b,c,d,e,f
  # or maybe there is demand for something that can not be produced so needs to be acquired elsewhere
  # doing this across multiple data frames forces matching columns
  df %>%
    dplyr::bind_rows(create_empty_numeric_dataframe(cols_needed)) %>%
    mutate_if(is.numeric , replace_na, replace = 0) %>%
    select(any_of(c('from_date', 'to_date', cols_needed)))
}

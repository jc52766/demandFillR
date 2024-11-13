library(tidyverse)

xsolve <- function(solved, # current solve
                  options, # options matrix
                  demand, # outstanding demand
                  dfw, # demand fill weights
                  dew # demand exceeeded weights
){
  # make all matrixes for faster solve
  rowsM <- nrow(options)
  colsM <- ncol(options)
  
  zeroM <- matrix(0, rowsM, colsM, byrow = T)
  dfM <- matrix(dfw, rowsM, colsM, byrow = T)
  deM <- matrix(dew, rowsM, colsM, byrow = T)
  
  beheaded_solved <- solved[-1, ]
  filled <- colSums(beheaded_solved)
  filledM <- matrix(filled, rowsM, colsM, byrow = T)
  demandM <- matrix(demand, rowsM, colsM, byrow = T)
  
  ##
  newFilledM <- filledM + options
  demandFilledM <- pmin(newFilledM, demandM)
  excessFillM <- pmax((newFilledM - demandM), zeroM)
  
  evalDemandFillM <- demandFilledM * dfM
  evalExcessFillM <- excessFillM * deM
  
  evalM <- evalDemandFillM + evalExcessFillM
  
  bestOptionIdx <- which.max(rowSums(evalM))
  
  # now to solved append the best option
  this_best_option <- options[bestOptionIdx, ] %>% matrix(nrow = 1)
  new_solved <- beheaded_solved %>%
    rbind(this_best_option)
  
  return(new_solved)
  
}

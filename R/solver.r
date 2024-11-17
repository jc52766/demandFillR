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

xsolve_multiple <- function(solved, # current solve
                                    options_l, # options list of matrixs 1 for each drop
                                    demand, # outstanding demand
                                    dfw, # demand fill weights
                                    dew # demand exceeeded weights
){
  # just like solve but for multiple primary drops
  # so basically run solve for each primary drop
  ndrops <- length(options)
  original_solved <- solved
  solves_l <- list()
  for (i in 1:ndrops) {
    this_opt <- options_l[[i]]
    solve1 <- xsolve(
      solved = solved,
      options = this_opt,
      demand = demand,
      dfw = dfw,
      dew = dew
    )
    # store this solve
    this_solve <- solve1 %>% tail(1) %>% matrix(ncol=ncol(solve1))
    solves_l[[i]] <- this_solve
    
    # temporarily update solved to row bind this primary drops solve
    solved <- rbind(solved, this_solve)
  }
  allSolve <- do.call(rbind,solves_l) %>% rowSums()
  
  # now behead original_solved and row bind the solve across all primary drops
  beheaded_original_solved <- original_solved[-1, ]
  final_solve <- beheaded_original_solved %>% rbind(allSolve) %>% matrix(ncol=ncol(solved))
  
  return(final_solve)
}

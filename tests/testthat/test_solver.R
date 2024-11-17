source("../../R/solver.r") # This only needed if project is not a package
library(testthat)

test_that("test basic solve", {
  solved <- matrix(rep(0, 12), ncol = 3, byrow = T)
  options <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), ncol = 3, byrow = T)
  demand <- c(2, 1, 1)
  dfw <- c(1, 1, 1)
  dew <- c(-0.5, -0.5, -0.5)
  expect <- matrix(c(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1),
                   ncol = 3,
                   byrow = T)
  for (i in 1:4) {
    solved <- xsolve(
      solved = solved,
      options = options,
      demand = demand,
      dfw = dfw,
      dew = dew
    )
  }
  expect_identical(solved, expect)
})


test_that("test all zero selected", {
  # Very heavily punish exceed demand so should pick all 0 option at that point
  solved <- matrix(rep(0, 12), ncol = 3, byrow = T)
  options <- matrix(c(1, 1, 1, 0, 0, 0), ncol = 3, byrow = T)
  demand <- c(2, 1, 1)
  dfw <- c(1, 1, 1)
  dew <- c(-50, -50, -50)
  expect <- matrix(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                   ncol = 3,
                   byrow = T)
  for (i in 1:4) {
    solved <- xsolve(
      solved = solved,
      options = options,
      demand = demand,
      dfw = dfw,
      dew = dew
    )
  }
  expect_identical(solved, expect)
})

test_that("test all not zero selected", {
  # Don't punish exceed demand so should not pick all 0 option ever
  solved <- matrix(rep(0, 12), ncol = 3, byrow = T)
  options <- matrix(c(1, 1, 1, 0, 0, 0), ncol = 3, byrow = T)
  demand <- c(2, 1, 1)
  dfw <- c(0.1, 0.1, 0.1)
  dew <- c(1, 1, 1)
  expect <- matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                   ncol = 3,
                   byrow = T)
  for (i in 1:4) {
    solved <- xsolve(
      solved = solved,
      options = options,
      demand = demand,
      dfw = dfw,
      dew = dew
    )
  }
  expect_identical(solved, expect)
})

test_that("test can maximise exceed", {
  # Promote exceed demand so should maximise excess
  solved <- matrix(rep(0, 12), ncol = 3, byrow = T)
  options <- matrix(c(2, 0, 0, 0, 1, 1), ncol = 3, byrow = T)
  demand <- c(2, 0, 0)
  dfw <- c(0.1, 0.1, 0.1)
  dew <- c(1, 1, 1)
  expect <- matrix(c(0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1),
                   ncol = 3,
                   byrow = T)
  for (i in 1:4) {
    solved <- xsolve(
      solved = solved,
      options = options,
      demand = demand,
      dfw = dfw,
      dew = dew
    )
  }
  expect_identical(solved, expect)
})

test_that("test that can improve with more solves", {
  # Check 1st solve is non as good as later solves in this specific case
  solved <- matrix(rep(0, 3*4), ncol = 3, byrow = T)
  options <- matrix(c(2, 0, 0, 1, 1, 0, 1, 0, 1), ncol = 3, byrow = T)
  demand <- c(4, 2, 2)
  dfw <- c(10, 1, 1)
  dew <- c(-10, -0.5, -0.5)
  expect1 <- matrix(c(2, 0, 0, 2, 0, 0, 1, 1, 0, 1, 1, 0),
                   ncol = 3,
                   byrow = T)
  expect2 <- matrix(c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 0),
                    ncol = 3,
                    byrow = T)
  for (i in 1:4) {
    solved <- xsolve(
      solved = solved,
      options = options,
      demand = demand,
      dfw = dfw,
      dew = dew
    )
  }
  expect_identical(solved, expect1)
  for (i in 1:4) {
    solved <- xsolve(
      solved = solved,
      options = options,
      demand = demand,
      dfw = dfw,
      dew = dew
    )
  }
  expect_identical(solved, expect2)
})

test_that("test multiple primary drops", {
  solved <- matrix(rep(0, 12), ncol = 3, byrow = T)
  options1 <- matrix(c(1, 0, 0, 0.5, 0.5, 0), ncol = 3, byrow = T)
  options2 <- matrix(c(0, 1, 0, 0, 0.5, 0.5), ncol = 3, byrow = T)
  options3 <- matrix(c(0, 0, 1, 0, 0.5, 0.5), ncol = 3, byrow = T)
  options_l <- list(options1, options2, options3)
  demand <- c(1, 1, 1)
  dfw <- c(1, 1, 1)
  dew <- c(-0.5, -0.5, -0.5)
  expect_l <- list(
    matrix(c(1, 0, 0),
                   ncol = 3,
                   byrow = T),
    matrix(c(0, 1, 0),
                    ncol = 3,
                    byrow = T),
    matrix(c(0, 0, 1),
                    ncol = 3,
                    byrow = T)
  )
  
  expect_final_solve <- matrix(c(rep(0,9), c(1, 1, 1)),
                               ncol = 3,
                               byrow = T)
  
  solves_l <- list()
  original_solved <- solved
  for (i in 1:3) {
    this_opt <- options_l[[i]]
    solve1 <- xsolve(
      solved = solved,
      options = this_opt,
      demand = demand,
      dfw = dfw,
      dew = dew
    )
    # store this solve
    this_solve <- solve1 %>% tail(1) %>% matrix(ncol=3)
    solves_l[[i]] <- this_solve
    
    # temporarily update solved to row bind this primary drops solve
    solved <- rbind(solved, this_solve)
    
    expect_identical(solves_l[[i]], expect_l[[i]])
  }
  allSolve <- do.call(rbind,solves_l) %>% rowSums()
  
  # now behead original_solved and row bind the solve across all primary drops
  beheaded_original_solved <- original_solved[-1, ]
  final_solve <- beheaded_original_solved %>% rbind(allSolve) %>% matrix(ncol=3)
  
  expect_identical(final_solve, expect_final_solve)
})


test_that("test multiple primary drops solve function", {
  solved <- matrix(rep(0, 12), ncol = 3, byrow = T)
  options1 <- matrix(c(1, 0, 0, 0.5, 0.5, 0), ncol = 3, byrow = T)
  options2 <- matrix(c(0, 1, 0, 0, 0.5, 0.5), ncol = 3, byrow = T)
  options3 <- matrix(c(0, 0, 1, 0, 0.5, 0.5), ncol = 3, byrow = T)
  options_l <- list(options1, options2, options3)
  demand <- c(1, 1, 1)
  dfw <- c(1, 1, 1)
  dew <- c(-0.5, -0.5, -0.5)
  
  expect_final_solve <- matrix(c(rep(0, 9), c(1, 1, 1)), ncol = 3, byrow = T)
  
  multiple_solved <- xsolve_multiple(
    solved = solved,
    options_l = options_l,
    demand = demand,
    dfw = dfw,
    dew = dew
  )
  expect_identical(multiple_solved, expect_final_solve)
})


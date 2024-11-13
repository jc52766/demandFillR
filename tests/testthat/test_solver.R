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
  #identical(solved, expect)
})

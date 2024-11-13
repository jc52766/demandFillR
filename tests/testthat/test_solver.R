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


source("../../R/helpers.r") # This only needed if project is not a package
library(testthat)

test_that("get unique colnames amongst for frames", {
  df1 <- data.frame(z = 5, m = 1, a = 6)
  df2 <- data.frame(b = 9, f = 6, y = 1)
  expect <- c('a', 'b', 'f', 'm', 'y', 'z')
  ans <- get_all_colnames_sorted(df1, df2)
  expect_identical(ans, expect)
})

test_that("test empty dataframe created", {
  expect <- data.frame(aa = 1, bb = 2, z = 3) %>% filter(aa == 666)
  ans <- create_empty_numeric_dataframe(c('aa', 'bb', 'z'))
  expect_identical(ans, expect)
})

test_that("test that columns get added to df simple", {
  df <- data.frame('A' = 1, 'Z' = 20, 'B' = 2)
  expect <- data.frame(
    'A' = 1,
    'B' = 2,
    'C' = 0,
    'D' = 0,
    'Z' = 20
  )
  ans <- force_all_options(df, c('A', 'B', 'C', 'D', 'Z'))
  expect_identical(ans, expect)
})

test_that("test that columns get added to df include from_date to_date", {
  df <- data.frame(
    from_date = as.Date('2024-11-04'),
    to_date = as.Date('2024-11-10'),
    'A' = 1,
    'Z' = 20,
    'B' = 2
  )
  expect <- data.frame(
    from_date = as.Date('2024-11-04'),
    to_date = as.Date('2024-11-10'),
    'A' = 1,
    'B' = 2,
    'C' = 0,
    'D' = 0,
    'Z' = 20
  )
  ans <- force_all_options(df, c('A', 'B', 'C', 'D', 'Z'))
  expect_identical(ans, expect)
})

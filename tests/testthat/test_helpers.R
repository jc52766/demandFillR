source("../../R/helpers.r") # This only needed if project is not a package
library(testthat)

test_that("get unique colnames amongst for frames", {
  df1 <- data.frame(z=5, m=1, a=6)
  df2 <- data.frame(b=9, f=6, y=1)
  expect <- c('a','b','f','m','y','z')
  ans <- get_all_colnames_sorted(df1, df2)
  expect_identical(ans, expect)
})

test_that("test empty dataframe created", {
  expect <- data.frame(aa=1,bb=2,z=3) %>% filter(aa == 666)
  ans <- create_empty_numeric_dataframe(c('aa','bb','z'))
  expect_identical(ans, expect)
})

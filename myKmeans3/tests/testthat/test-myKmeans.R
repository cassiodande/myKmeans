library(testthat)
test_that("Euclidean distance computation", {
  expect_equal(Dist(c(1,2),c(2,3)), sqrt(2))
})

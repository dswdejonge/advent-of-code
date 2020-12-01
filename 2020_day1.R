library(testthat)

findTermsToSum <- function(A, B, result){
  complement <- resSum - A
  i <- which(complement %in% B)
  return(c(A[i], complement[i]))
}

A <- c(200, 300, 400)
B <- c(750, 800, 200)
resSum <- 1000

test_that("function finds correct terms to sum",{
  expect_equal(sum(findTermsToSum(vecA, vecB, resSum)), resSum)
})
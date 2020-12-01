library(testthat)

### PART 1 ###
findTermsToSum <- function(num_vec, resulting_sum){
  complement <- resulting_sum - num_vec
  return(num_vec[which(complement %in% num_vec)])
}

num_vec <- c(200, 300, 400, 700)
resulting_sum <- 1000

test_that("function finds correct terms to sum",{
  expect_equal(sum(findTermsToSum(num_vec, resulting_sum)), resulting_sum)
})

input <- as.numeric(readLines("2020_day1_input.txt"))
resulting_sum <- 2020
result <- findTermsToSum(input, resulting_sum)
answer <- prod(result)

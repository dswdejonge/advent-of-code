library(testthat)

### PART 1 ###
findTermsToSum <- function(num_vec, resulting_sum){
  complement <- resulting_sum - num_vec
  return(num_vec[which(complement %in% num_vec)])
}

findTermsToSum2 <- function(num_vec, resulting_sum, n){
  combs <- combn(num_vec, n)
  i <- which(colSums(combs) == resulting_sum)
  return(combs[,i])
}

num_vec <- c(200, 300, 400, 700)
resulting_sum <- 1000
test_that("function finds correct terms to sum",{
  expect_equal(sum(findTermsToSum(num_vec, resulting_sum)), resulting_sum)
  expect_equal(sum(findTermsToSum2(num_vec, resulting_sum, 2)), resulting_sum)
})

input <- as.numeric(readLines("2020_day1_input.txt"))
resulting_sum <- 2020
result <- findTermsToSum(input, resulting_sum)
answer <- prod(result)
#1009899

result <- findTermsToSum2(input, resulting_sum, 2)
answer <- prod(result)
#1009899


### PART 2 ####
num_vec <- c(200, 300, 500, 700, 800)
resulting_sum <- 1000
test_that("function finds correct terms to sum",{
  expect_equal(sum(findTermsToSum2(num_vec, resulting_sum, 3)), resulting_sum)
})

input <- as.numeric(readLines("2020_day1_input.txt"))
resulting_sum <- 2020
result <- findTermsToSum2(input, resulting_sum, 3)
system.time(findTermsToSum2(input, resulting_sum, 3))
answer <- prod(result)

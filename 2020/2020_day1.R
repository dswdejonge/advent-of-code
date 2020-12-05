library(testthat)

##### Functions
# Option 1: Fails test, doesn't work with exactly half-values
# findTermsToSum <- function(num_vec, resulting_sum){
#   complement <- resulting_sum - num_vec
#   return(num_vec[which(complement %in% num_vec)])
# }

# Option 2: Bit of a brute force, but quite fast nevertheless
findTermsToSum2 <- function(num_vec, resulting_sum, n){
  combs <- combn(num_vec, n)
  i <- which(colSums(combs) == resulting_sum)
  return(combs[,i])
}

# Option 3: Tail recursion, is not supported in R
# Stack overflow with larger numbers
counter <- function(v, n, i = length(v)){
  v[i] <- v[i]+1
  if(v[i] > n){
    v[i] <- 1
    return(counter(v, n, i-1))
  }else{
    return(v)
  }
}

findTermsToSumRecursively <- function(num_vec, resulting_sum, n, i = 1:n){
  if(sum(num_vec[i]) == resulting_sum){
    return(num_vec[i])
  }else{
    i <- counter(i,length(num_vec))
    return(findTermsToSumRecursively(num_vec, resulting_sum, n, i))
  }
}


### Tests
num_vec <- c(200, 300, 500, 700)
resulting_sum <- 1000
test_that("function finds correct terms to sum",{
  #expect_equal(sum(findTermsToSum(num_vec, resulting_sum)), resulting_sum)
  expect_equal(sum(findTermsToSum2(num_vec, resulting_sum, 2)), resulting_sum)
  expect_equal(sum(findTermsToSum2(num_vec, resulting_sum, 3)), resulting_sum)
  expect_equal(sum(findTermsToSumRecursively(num_vec, resulting_sum, 3)), resulting_sum)
  expect_equal(sum(findTermsToSumRecursively(num_vec, resulting_sum, 2)), resulting_sum)
})


#### PART 1 ####
input <- as.numeric(readLines("2020_day1_input.txt"))
resulting_sum <- 2020
# result <- findTermsToSum(input, resulting_sum)
# answer <- prod(result)
#1009899
result <- findTermsToSum2(input, resulting_sum, 2)
answer <- prod(result)
#1009899


### PART 2 ####
input <- as.numeric(readLines("2020_day1_input.txt"))
resulting_sum <- 2020
result <- findTermsToSum2(input, resulting_sum, 3)
system.time(findTermsToSum2(input, resulting_sum, 3))
answer <- prod(result)
# 44211152

### Recursive
# Stack overflow, tail recursion not supported in R
# input <- as.numeric(readLines("2020_day1_input.txt"))
# resulting_sum <- 2020
# result <- findTermsToSumRecursively(input, resulting_sum, 3)
# system.time(findTermsToSum2(input, resulting_sum, 3))
# answer <- prod(result)


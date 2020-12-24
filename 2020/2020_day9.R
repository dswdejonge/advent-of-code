library(testthat)

### Functions
# Function from Day 1
findTermsToSum <- function(num_vec, resulting_sum, n){
  combs <- combn(num_vec, n)
  i <- which(colSums(combs) == resulting_sum)
  return(combs[,i])
}

findXMASmistake <- function(preamble, input){
  start <- 1
  stop <- preamble
  for(nr in input[(preamble+1):length(input)]){
    result <- findTermsToSum(input[start:stop], nr, 2)
    if(sum(result) == 0){
      return(nr)
    }
    start <- start+1; stop <- stop+1
  }
  return("No mistakes found")
}

findXMASweakness <- function(mistake_value, input){
  start <- 1
  stop <- start+1
  mysum <- sum(input[start:stop]) 
  while(mysum != mistake_value){
    if(mysum > mistake_value){
      start <- start+1
      stop <- start+1
    }else{
      stop <- stop+1
    }
    mysum <- sum(input[start:stop]) 
  }
  return(min(input[start:stop]) + max(input[start:stop]))
}

### Tests
input <- as.numeric(readLines("2020_day9_input_test.txt"))
test_that("Function works", {
  expect_equal(findXMASmistake(5, input), 127)
  expect_equal(findXMASweakness(127, input), 62)
})

### Part 1
input <- as.numeric(readLines("2020_day9_input.txt"))
findXMASmistake(25, input)
# 14360655

### Part 2 
input <- as.numeric(readLines("2020_day9_input.txt"))
findXMASweakness(14360655, input)

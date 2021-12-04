library(testthat)

getInput <- function(filename){
  return(readLines(filename))
}

# return numeric vector
getGammaBin <- function(input){
  pos <- as.list(1:nchar(input[1]))
  getCommonBit <- function(posit, input){
    vec <- as.numeric(substr(input, posit, posit))
    if(sum(vec) > length(vec)/2){return(1)}
    else{return(0)}
  }
  gamma <- sapply(pos, getCommonBit, input = input)
  return(gamma)
}

getEpsilonBin <- function(GammaBin){
  return(ifelse(GammaBin == 1, 0, 1))
}

BinToDec <- function(Bin){
  binstr <- paste(Bin, collapse = "")
  return(strtoi(binstr, base = 2))
}


getPwrCon <- function(Gamma, Epsilon){
  return(Gamma * Epsilon)
}

filename <- "3_test.txt"
input <- getInput(filename)
test_that("the function provides correct output", {
  expect_equal(getGammaBin(input), c(1,0,1,1,0))
  expect_equal(BinToDec(c(1,0,1,1,0)), 22)
  expect_equal(getEpsilonBin(c(1,0,1,1,0)), c(0,1,0,0,1))
  expect_equal(BinToDec(c(0,1,0,0,1)), 9)
  expect_equal(getPwrCon(22, 9), 198)
})

filename <- "3_input.txt"
input <- getInput(filename)
GammaBin <- getGammaBin(input)
Gamma <- BinToDec(GammaBin)
Epsilon <- BinToDec(getEpsilonBin(GammaBin))
getPwrCon(Gamma, Epsilon)

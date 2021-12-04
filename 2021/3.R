library(testthat)

# get vector of binstrings
getInput <- function(filename){
  return(readLines(filename))
}

# return numeric vector at position i
getPosVec <- function(posit, input){
  return(as.numeric(substr(input, posit, posit)))
}

# return 1 or 0
getCommonBit <- function(posit, input){
  vec <- getPosVec(posit, input)
  if(sum(vec) >= length(vec)/2){return(1)}
  else{return(0)}
}

# PART 1

# return numeric vector
getGammaBin <- function(input){
  pos <- as.list(1:nchar(input[1]))
  gamma <- sapply(pos, getCommonBit, input = input)
  return(gamma)
}

# return numeric vector
getEpsilonBin <- function(GammaBin){
  return(ifelse(GammaBin == 1, 0, 1))
}

# return decimal
BinToDec <- function(Bin){
  binstr <- paste(Bin, collapse = "")
  return(strtoi(binstr, base = 2))
}

# Get Power Consumption value
getPwrCon <- function(input){
  GammaBin <- getGammaBin(input)
  Gamma <- BinToDec(GammaBin)
  Epsilon <- BinToDec(getEpsilonBin(GammaBin))
  return(Gamma * Epsilon)
}

filename <- "3_test.txt"
input <- getInput(filename)
test_that("the function provides correct output", {
  expect_equal(getGammaBin(input), c(1,0,1,1,0))
  expect_equal(BinToDec(c(1,0,1,1,0)), 22)
  expect_equal(getEpsilonBin(c(1,0,1,1,0)), c(0,1,0,0,1))
  expect_equal(BinToDec(c(0,1,0,0,1)), 9)
  expect_equal(getPwrCon(input), 198)
})

filename <- "3_input.txt"
input <- getInput(filename)
getPwrCon(input)

# PART 2
filterOptions <- function(input, type = c("O2", "CO2")){
  i <- 1
  options <- input
  while(length(options) > 1){
    posVec <- getPosVec(i, options)
    cmn <- getCommonBit(i, options)
    if(type == "CO2"){cmn <- ifelse(cmn == 1, 0, 1)}
    keep <- which(posVec == cmn)
    options <- options[keep]
    i <- i+1
  }
  return(options)
}

getLifeSupport <- function(input){
  Oxygen <- BinToDec(filterOptions(input, type = "O2"))
  CarbDiOx <- BinToDec(filterOptions(input, type = "CO2"))
  return(Oxygen * CarbDiOx)
}

filename <- "3_test.txt"
input <- getInput(filename)
test_that("function works", {
  expect_equal(filterOptions(input, type = "O2"), "10111")
  expect_equal(filterOptions(input, type = "CO2"), "01010")
  expect_equal(getLifeSupport(input), 230)
})

filename <- "3_input.txt"
input <- getInput(filename)
getLifeSupport(input)

library(testthat)
library(stringr)
library(dplyr)

### PARAMETERS
trajX <- 3
trajY <- 1

### FUNCTIONS
inputToMatrix <- function(input){
  indices <- stringr::str_locate_all(input, "#")
  grid <- matrix(0, nrow = length(indices), ncol = nchar(input[1]))
  for(i in 1:length(indices)){
    grid[i, indices[[i]][,"start"]] <- 1
  }
  return(grid)
}

obtainSleighPath <- function(input, trajX, trajY){
  df <- data.frame(
    Y = seq(from = 1, to = length(input), by = trajY),
    X = seq(from = 1, to = length(input)*trajX, by = trajX)
  ) %>%
    dplyr::mutate(
      X = X %% nchar(input[1])
    )
  df$X[which(df$X == 0)] <- nchar(input[1])
  return(df)
}

findEncounteredTrees <- function(input, trajX, trajY){
  grid <- inputToMatrix(input)
  positions <- obtainSleighPath(input, trajX, trajY)
  objects <- grid[as.matrix(positions)]
  return(sum(objects))
}

### TESTS
input <- readLines("2020_day3_input_test.txt")
test_that("the function provides correct output", {
  expect_equal(findEncounteredTrees(input, trajX = trajX, trajY = trajY), 7)
})

### PART 1
input <- readLines("2020_day3_input.txt")
findEncounteredTrees(input, trajX = trajX, trajY = trajY)

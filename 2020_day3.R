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
  X = seq(from = 1, to = ceiling(length(input)/trajY)*trajX, by = trajX)
  Y = seq(from = 1, to = length(input), by = trajY)
  df <- data.frame(Y = Y, X = X) %>%
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

trajectories <- list(
  c(1,1),
  c(3,1),
  c(5,1),
  c(7,1),
  c(1,2)
)
sums <- vector(mode = "integer", length = length(trajectories)) ; i <- 1
for(trajectory in trajectories){
  sums[i] <- findEncounteredTrees(input, trajX = trajectory[1], trajY = trajectory[2])
  i <- i+1
}
print(sums)
# these slopes would find 2, 7, 3, 4, and 2 tree(s)

### PART 1
input <- readLines("2020_day3_input.txt")
findEncounteredTrees(input, trajX = trajX, trajY = trajY)

### PART 2
trajectories <- list(
  c(1,1),
  c(3,1),
  c(5,1),
  c(7,1),
  c(1,2)
)
sums <- vector(mode = "integer", length = length(trajectories)); i <- 1
for(trajectory in trajectories){
  sums[i] <- findEncounteredTrees(input, trajX = trajectory[1], trajY = trajectory[2])
  i <- i+1
}
print(sums)
#90 278  88  98  45

prod(sums)
# 9709761600
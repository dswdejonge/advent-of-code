library(testthat)

sliding_window <- function(sonar_sweep, window_size){
  inc <- window_size-1
  return(unlist(lapply(as.list(1:(length(sonar_sweep)-inc)), 
                    function(X, Y){return(sum(Y[X:(X+inc)]))}, 
                    Y = sonar_sweep)))
}

nr_increases <- function(myvec){
  return(length(which(diff(myvec) > 0)))
}

### Test
input <- read.csv("1_test.txt", header = FALSE)[,1]
test_that("function words", {
  expect_equal(nr_increases(input), 7) #p1
  expect_equal(nr_increases(sliding_window(input, 3)), 5)
})

## Answers
sonar_sweep <- read.csv("1_input.txt", header = FALSE)[,1]
nr_increases(sonar_sweep)
#1502
nr_increases(sliding_window(sonar_sweep, 3))

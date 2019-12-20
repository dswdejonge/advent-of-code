matches_requirements <- function(nr){
  is_increasing <- all(nr == cummax(nr))
  is_adjacent <- 
    length(which(duplicated(nr) | duplicated(nr, fromLast = T))) >= 2
  #is_six_digits <- length(nr) == 6
  #if(is_increasing & is_adjacent & is_six_digits){
  if(is_increasing & is_adjacent){
      return(T)
  }else{
    return(F)
  }
}

get_nr_vec <- function(nr){
  nr <- as.integer(strsplit(as.character(nr), split = "")[[1]])
  return(nr)
}

library(testthat)
test1 <- 111111
test2 <- 223450
test3 <- 123789
test4 <- 1234
test5 <- 123332
test6 <- 111222
test_that("matches_requirements finds correct values", {
  expect_equal(matches_requirements(get_nr_vec(test1)), T)
  expect_equal(matches_requirements(get_nr_vec(test2)), F)
  expect_equal(matches_requirements(get_nr_vec(test3)), F)
  expect_equal(matches_requirements(get_nr_vec(test4)), F)
  expect_equal(matches_requirements(get_nr_vec(test5)), F)
  expect_equal(matches_requirements(get_nr_vec(test6)), T)
})

run <- function(start, stop){
  count <- 0
  nr <- start
  nr_vec <- get_nr_vec(nr)
  i <- which(diff(nr_vec) < 0)[1]
  nr_vec[i:length(nr_vec)] <- nr_vec[i]
  nr <- as.integer(paste0(nr_vec, collapse = ""))
  repeat{
    nr <- format(nr, scientific = F)
    nr_vec <- cummax(get_nr_vec(nr))
    nr <- as.integer(paste0(nr_vec, collapse = ""))
    if(nr > stop){
      break
    }
    TF <- matches_requirements(nr_vec)
    if(TF){
      count <- count + 1
    }
    nr <- nr + 1
  }
  return(count)
}

start <- 272091 # too low
stop <- 815432
#start <- 178416 # harry
#stop <- 676461
system.time(count <- run(start, stop))
# harry: 1650
# 931 is too low


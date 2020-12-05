library(testthat)
library(dplyr)
library(stringr)

### FUNCTIONS
inputToDF <- function(input){
  df <- sapply(input, strsplit, split = " ", USE.NAMES = FALSE) %>%
    do.call(rbind, .) %>%
    dplyr::as_tibble() %>% 
    dplyr::rename(min_max = V1, letter = V2, password = V3) %>%
    mutate(
      letter = substr(letter, start = 1, stop = (nchar(letter)-1)),
      Int1 = as.integer(substr(min_max, start = 1, stop = (regexpr("-",min_max)-1))),
      Int2 = as.integer(substr(min_max, start = regexpr("-",min_max)+1, stop = nchar(min_max))),
      min_max = NULL)
  return(df)
}

findWrongPasswordsPolicy1 <- function(input){
  df <- dplyr::mutate(input,
                      check = stringr::str_count(password, letter),
                      is_wrong = ifelse(check < Int1 | check > Int2, TRUE, FALSE))
  return(df)
}

findWrongPasswordsPolicy2 <- function(input){
  df <- dplyr::mutate(input,
                      Let1 = stringr::str_sub(password, start = Int1, end = Int1),
                      Let2 = stringr::str_sub(password, start = Int2, end = Int2),
                      Comb = paste0(Let1, Let2),
                      LetCount = stringr::str_count(Comb, letter),
                      is_wrong = ifelse(LetCount == 1, FALSE, TRUE)
                      )
  return(df)
}

checkNrCorrectPWs <- function(input, policy = 1){
    df <- inputToDF(input) 
    if(policy == 1){
      df <- findWrongPasswordsPolicy1(df)
    }else if(policy == 2){
      df <- findWrongPasswordsPolicy2(df)
    }else{
      stop("unknown policy")
    }
    n <- length(which(!df$is_wrong))
    return(n)
}

### TESTS
input <- readLines("2020_day2_input_test.txt")
input2 <- readLines("2020_day2_input_test2.txt")
test_that("the function provides correct output", {
  expect_equal(checkNrCorrectPWs(input), 2)
  expect_equal(checkNrCorrectPWs(input2), 9)
  expect_equal(checkNrCorrectPWs(input2, policy = 2), 8)
})

### PART 1
input <- readLines("2020_day2_input.txt")
checkNrCorrectPWs(input)
#548

## PART 2
input <- readLines("2020_day2_input.txt")
checkNrCorrectPWs(input, policy = 2)
#502
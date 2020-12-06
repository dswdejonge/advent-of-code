library(testthat)
library(dplyr)
library(stringr)

### FUNCTIONS
inputToData <- function(input){
  my_list <- list()
  list_index <- 1
  for(i in 1:length(input)){
    if(input[i] == ""){
      names(my_list)[list_index] <- paste0("item",list_index)
      list_index <- list_index+1
    }else{
      my_list$temp <- c(my_list$temp, input[i])
    }
  }
  names(my_list)[list_index] <- paste0("item",list_index)
  return(my_list)
}

findAnswers <- function(input, who = c("anyone", "everyone")){
  getAllLetters <- function(answers_vector){
    all <- unique(unlist(strsplit(answers_vector, "")))
    return(all)
  }
  getCommonLetters <- function(answers_vec){
    letter_vec <- getAllLetters(answers_vec)
    x <- sapply(answers_vec, str_detect, pattern = letter_vec)
    are_common <- vector(mode = "logical", length = length(letter_vec))
    for(i in 1:length(are_common)){
      if(is.null(nrow(x))){
        are_common[i] <- all(x)
      }else{
        are_common[i] <- all(x[i,]) 
      }
    }
    return(letter_vec[are_common])
  }
  
  my_list <- inputToData(input)
  if(who == "anyone"){
    all_letters <- sapply(my_list, getAllLetters)
    return(all_letters)
  }else if(who == "everyone"){
    common_letters <- sapply(my_list, getCommonLetters)
    return(common_letters)
  }
}

countAnswers <- function(all_letters){
  my_count <- sapply(all_letters, length)
  return(my_count)
}

### TESTS
input <- readLines("2020_day6_input_test.txt")
test_that("the correct number of unique answers are found", {
  expect_equal(sum(countAnswers(findAnswers(input, "anyone"))),11)
  expect_equal(sum(countAnswers(findAnswers(input, "everyone"))),6)
})

### PART 1
input <- readLines("2020_day6_input.txt")
sum(countAnswers(findAnswers(input, "anyone")))
# 7128
### PART 2
sum(countAnswers(findAnswers(input, "everyone")))
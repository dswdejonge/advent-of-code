library(testthat)
library(dplyr)

### FUNCTIONS
inputToData <- function(input){
  my_list <- list()
  list_index <- 1
  for(i in 1:length(input)){
    if(input[i] == ""){
      names(my_list)[list_index] <- paste0("item",list_index)
      list_index <- list_index+1
    }else{
      #answers <- unlist(strsplit(,""))
      my_list$temp <- 
        c(my_list$temp, input[i])
    }
  }
  names(my_list)[list_index] <- paste0("item",list_index)
  return(my_list)
}

findUniqueAnswers <- function(input, who = c("anyone", "everyone")){
  my_list <- inputToData(input)
  if(who == "anyone"){
    unique_answers <- sapply(my_list, strsplit, split = "") %>%
      sapply(., unlist) %>%
      sapply(., unique)
  }else if(who == "everyone"){
    
  }
  return(unique_answers)
}

countUniqueAnswers <- function(unique_answers){
  unique_answer_count <- sapply(unique_answers, length)
  return(unique_answer_count)
}

### TESTS
input <- readLines("2020_day6_input_test.txt")
test_that("the correct number of unique answers are found", {
  expect_equal(sum(countUniqueAnswers(findUniqueAnswers(input, "anyone"))),11)
})

### PART 1
input <- readLines("2020_day6_input.txt")
sum(countUniqueAnswers(findUniqueAnswers(input, "anyone")))
# 7128

### PART 2
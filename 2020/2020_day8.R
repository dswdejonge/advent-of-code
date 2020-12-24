library(testthat)

### Functions
inputToData <- function(input){
  lt <- sapply(input, strsplit, split = " ")
  names(lt) <- NULL
  return(lt)
}

executeCode <- function(code){
  global_counter <- 0
  executed_lines <- vector(mode = "numeric", length = 0)
  this_line <- 1
  while(!(this_line %in% executed_lines)){
    executed_lines <- c(executed_lines, this_line)
    action <- code[[this_line]][1]
    value <- as.numeric(code[[this_line]][2])
    if(action == "nop"){
      this_line <- this_line + 1
    }else if(action == "jmp"){
      this_line <- this_line + value
    }else if(action == "acc"){
      global_counter <- global_counter + value
      this_line <- this_line + 1
    }
    if(this_line > length(code)){
      return(paste0("Successfully terminated with counter: ", global_counter))
    }
  }
  return(paste0("Error, terminated with counter: ", global_counter))
}

fixCorruption <- function(code){
  func <- function(x, arg1){return(arg1 %in% x)}
  try_jmp <- which(sapply(code, func, arg1 = "jmp"))
  try_nop <- which(sapply(code, func, arg1 = "nop"))
  result <- executeCode(code)
  for(i in try_jmp){
    temp.code <- code
    temp.code[[i]][1] <- "nop"
    result <- executeCode(temp.code)
    if(grepl("Success", result)){
      return(result)
    }
  }
  for(i in try_nop){
    temp.code <- code
    temp.code[[i]][1] <- "jmp"
    result <- executeCode(temp.code)
    if(grepl("Success", result)){
      return(result)
    }
  }
  return("No success fixing corruption")
}

### Test
input <- readLines("2020_day8_input_test.txt")
test_that("function words", {
  expect_equal(executeCode(inputToData(input)), "Error, terminated with counter: 5")
  expect_equal(fixCorruption(inputToData(input)), "Successfully terminated with counter: 8")
})

### Part 1
input <- readLines("2020_day8_input.txt")
executeCode(inputToData(input))
# 1134

### Part 2
input <- readLines("2020_day8_input.txt")
fixCorruption(inputToData(input))

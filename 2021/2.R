library(testthat)
library(dplyr)

getInput <- function(filename){
  input <- readLines(filename) %>%
    sapply(strsplit, split = " ", USE.NAMES = FALSE) %>%
    do.call(rbind, .) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::rename("command" = V1, "value" = V2) %>%
    dplyr::mutate(value = as.integer(value))
  return(input)
}

getPosition <- function(df){
  X = 0; Y = 0
  for(i in 1:nrow(df)){
    cmd <- df[i,"command"]
    vl <- df[i,"value"]
    if(cmd == "forward"){X <- X+vl}
    else if(cmd == "backward"){X <- X-vl}
    else if(cmd == "down"){Y <- Y+vl}
    else if(cmd == "up"){Y <- Y-vl}
    else(message("Unknown command"))
  }
  return(c(X, Y))
}

prod(getPosition(getInput("2_test.txt")))
prod(getPosition(getInput("2_input.txt")))

# part 2
getPosition2 <- function(df){
  X = 0; Y = 0; aim = 0
  for(i in 1:nrow(df)){
    cmd <- df[i,"command"]
    vl <- df[i,"value"]
    if(cmd == "forward"){
      X <- X+vl
      Y <- Y+(aim*vl)
      }
    #else if(cmd == "backward"){X <- X-vl}
    else if(cmd == "down"){aim <- aim+vl}
    else if(cmd == "up"){aim <- aim-vl}
    else(message("Unknown command"))
  }
  return(c(X, Y))
}

prod(getPosition2(getInput("2_test.txt")))
prod(getPosition2(getInput("2_input.txt")))

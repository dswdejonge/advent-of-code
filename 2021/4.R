library(testthat)

# get vector of binstrings
getInput <- function(filename){
  return(readLines(filename))
}

getBalls <- function(firstLine){
  return(as.numeric(unlist(strsplit(firstLine, split = ","))))
}

getBoards <- function(input){
  brdNr <- 0
  brdList <- list()
  for(i in 2:length(input)){
    if(input[i] == ""){
      brdNr <- brdNr + 1
      rNr <- 1
      tempMatrix <- matrix(nrow = 5, ncol = 5)
      next
    }
    strvec <- unlist(strsplit(input[i], split = " "))
    vec <- as.numeric(strvec[which(strvec != "")])
    tempMatrix[rNr,] <- vec
    if(rNr == 5){brdList[[brdNr]] <- tempMatrix}
    rNr <- rNr + 1
  }
  return(brdList)
}

findWin <- function(balls, brdList){
  # storage matrix for marked nr as indices
  results <- matrix(ncol = length(balls), nrow = length(brdList))
  # storage matrix for marked nr as row and col nr. 
  # in matrix, rows are the boards, columns are the ball nrs
  rows <- matrix(ncol = length(balls), nrow = length(brdList))
  columns <- matrix(ncol = length(balls), nrow = length(brdList))
  
  for(j in 1:length(balls)){
    # Get marked nr based on index
    positionsIndex <- lapply(brdList,
                        function(brd, ball){
                          index_position <- which(brd == ball)
                          if(length(index_position) == 0){index_position <- NA}
                          return(index_position)
                        },
                        ball = balls[j])
    results[,j] <- unlist(positionsIndex)
    
    # Get marked nr based on row and col nr
    positions <- lapply(brdList, 
                        function(brd, ball){return(which(brd == ball, arr.ind = TRUE))},
                        ball = balls[j])
    rows[,j] <- sapply(positions, function(x){return(x[1])})
    columns[,j] <- sapply(positions, function(x){return(x[2])})
    
    # Find winning moment. 
    # k is boardnr, l is 1 to 5 (max 5 rows or columns)
    winning_bal <- balls[j]
    for(k in 1:length(brdList)){
      for(l in 1:5){
        if(length(which(rows[k,] == l)) == 5){
          return(list(
            winBal = winning_bal,
            winBoard = k,
            markings = results[k,]))
        }
        if(length(which(columns[k,] == l)) == 5){
          return(list(
            winBal = winning_bal,
            winBoard = k,
            markings = results[k,]))
        }
      }
    }
  }
}

finalCalc <- function(winBal, brd, markings){
  brd[markings] <- 0
  sumUnmarked <- sum(brd)
  return(winBal * sumUnmarked)
}

totalWorkflow <- function(input){
  balls <- getBalls(input[1])
  brdList <- getBoards(input)
  win <- findWin(balls, brdList)
  return(finalCalc(win$winBal, brdList[[win$winBoard]], win$markings))
}

# Part 1
filename <- "4_test.txt"
input <- getInput(filename)
test_that("workflow works", {
  expect_equal(totalWorkflow(input), 4512)
})

filename <- "4_input.txt"
input <- getInput(filename)
totalWorkflow(input)

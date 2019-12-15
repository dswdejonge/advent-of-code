to_R_index <- function(i){
  return(i + 1)
}

intcode <- function(input){
  index <- to_R_index(0)
  pc <- input[index]
  while(pc != 99 & !is.na(pc)){
    take_values_from <- to_R_index(input[c(1,2)+index])
    store_values_at <- to_R_index(input[index+3])
    if(pc == 1){
      sum(input[take_values_from]) -> 
        input[store_values_at]
    }else if(pc == 2){
      prod(input[take_values_from]) ->
        input[store_values_at]
    }else{
      stop("Program code unknown.")
    }
    index <- index + 4
    pc <- input[index]
  }
  return(input)
}

tests <- list(
  t1 = c(1,9,10,3,2,3,11,0,99,30,40,50),
  t2 = c(1,0,0,0,99),
  t3 = c(2,3,0,3,99),
  t4 = c(2,4,4,5,99,0),
  t5 = c(1,1,1,4,99,5,6,0,99)
)

for(test in tests){
  print(intcode(test))
}

# Real question
input <- c(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,2,19,9,23,1,23,5,27,2,6,27,31,1,31,5,35,1,35,5,39,2,39,6,43,2,43,10,47,1,47,6,51,1,51,6,55,2,55,6,59,1,10,59,63,1,5,63,67,2,10,67,71,1,6,71,75,1,5,75,79,1,10,79,83,2,83,10,87,1,87,9,91,1,91,10,95,2,6,95,99,1,5,99,103,1,103,13,107,1,107,10,111,2,9,111,115,1,115,6,119,2,13,119,123,1,123,6,127,1,5,127,131,2,6,131,135,2,6,135,139,1,139,5,143,1,143,10,147,1,147,2,151,1,151,13,0,99,2,0,14,0)
input[to_R_index(1)] <- 12
input[to_R_index(2)] <- 2
result <- intcode(input)
print(result[1])

library(testthat)

### PARAMETERS
row_nrs <- c(0, 127) #128 # 0 to 127 = 2^7
col_nrs <- c(0, 7) #8 # 0 to 7 = 2^3

### FUNCTIONS
# Binary partitioning: only works with even values
binary_partitioning <- function(range_in = c(0,127), partition = c("lower", "upper")){
  mid = mean(range_in)
  if(partition == "lower"){
    out <- c(range_in[1], floor(mid))
  }else if(partition == "upper"){
    out <- c(ceiling(mid), range_in[2])
  }
  if(out[1] == out[2]){out <- out[1]}
  return(out)
}

getSeatCoords <- function(boarding_nr, row_nrs, col_nrs){
  if(nchar(boarding_nr) != 10){stop("incorrect boarding pass")}
  row_instructions <- substr(boarding_nr, 1, 7)
  col_instructions <- substr(boarding_nr, 8, 10)
  # get row nr
  for(i in 1:nchar(row_instructions)){
    instruction <- substr(row_instructions, i, i)
    if(instruction == "F"){
      instruction <- "lower"
    }else if(instruction == "B"){
      instruction <- "upper"
    }
    row_nrs <- binary_partitioning(row_nrs, instruction)
  }
  # get col nr
  for(i in 1:nchar(col_instructions)){
    instruction <- substr(col_instructions, i, i)
    if(instruction == "L"){
      instruction <- "lower"
    }else if(instruction == "R"){
      instruction <- "upper"
    }
    col_nrs <- binary_partitioning(col_nrs, instruction)
  }
  return(c(row_nrs, col_nrs))
}

getSeatID <- function(seat_coords){
  return(seat_coords[1]*8+seat_coords[2])
}


### TESTS
test_that("binary partitioning is done correctly",{
  expect_equal(binary_partitioning(range_in = c(1,2), "lower"), 1)
  expect_equal(binary_partitioning(range_in = c(1,2), "upper"), 2)
  expect_equal(binary_partitioning(range_in = c(0,3), "lower"), c(0,1))
  expect_equal(binary_partitioning(range_in = c(0,3), "upper"), c(2,3))
  expect_equal(binary_partitioning(range_in = c(0,127), "lower"), c(0,63))
  expect_equal(binary_partitioning(range_in = c(0,63), "upper"), c(32,63))
})

test_that("right seat coordinates are generated",{
  expect_equal(getSeatCoords("FBFBBFFRLR", row_nrs, col_nrs), c(44, 5))
})

test_that("the right seat ID is generated",{
  expect_equal(getSeatID(getSeatCoords("BFFFBBFRRR", row_nrs, col_nrs)), 567)
  expect_equal(getSeatID(getSeatCoords("FFFBBBFRRR", row_nrs, col_nrs)), 119)
  expect_equal(getSeatID(getSeatCoords("BBFFBBFRLL", row_nrs, col_nrs)), 820)
})

input <- c("BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL")
expected_out <- c(567, 119, 820)

### PART 1
input <- readLines("2020_day5_input.txt")
seatIDs <- vector(mode = "integer", length = length(input))
for(i in 1:length(input)){
  boarding_nr <- input[i]
  seatID <- getSeatID(getSeatCoords(boarding_nr, row_nrs, col_nrs))
  seatIDs[i] <- seatID
}
# 861
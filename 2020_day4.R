library(stringr)
library(dplyr)
library(testthat)

### PARAMETERS
required <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
optional <- c("cid")
validation <- list(
  byr = list(reg = "^\\d{4}$", mini = 1920, maxi = 2002),
  iyr = list(reg = "^\\d{4}$", mini = 2010, maxi = 2020),
  eyr = list(reg = "^\\d{4}$", mini = 2020, maxi = 2030),
  hgt = list(reg = "\\d\\d(cm|in)$", 
             min_cm = 150, max_cm = 193,
             min_in = 59, max_in = 76),
  hcl = list(reg = "^#[\\d|a-f]{6}$" ),
  ecl = list(reg = "^amb$|^blu$|^brn$|^gry$|^grn$|^hzl$|^oth$"),
  pid = list(reg = "^\\d{9}$")
)

### FUNCTIONS
inputToData <- function(input){
  passport_list <- list()
  values <- list()
  passport_i <- 1
  for(i in 1:length(input)){
    if(input[i] == ""){
      names(passport_list)[passport_i] <- paste0("pass",passport_i)
      passport_i <- passport_i+1
    }else{
      items <- unlist(strsplit(input[i]," ")) %>%
        sapply(strsplit, split = ":") %>%
        as_tibble()
      items_l <- as.list(items[2,])
      names(items_l) <- items[1,]
      passport_list$temp <- 
        c(passport_list$temp, items_l)
    }
  }
  names(passport_list)[passport_i] <- paste0("pass",passport_i)
  return(passport_list)
}

checkValidity <- function(passport_vector, required, validation = NULL){
  # Are all required fields present?
  isValid <- all(required %in% names(passport_vector))
 
   # Is the data valid?
  if(isValid & !is.null(validation)){
    for(key in names(passport_vector)){
      # Ignore cid
      if(key == "cid"){next}
      # Check regex
      isValidKey <- str_detect(passport_vector[key], validation[[key]]$reg) 
      if(!isValidKey){
        isValid <- isValidKey
        break
      }
      # Check byr
      if(key == "byr"){
        isValidKey <- as.numeric(passport_vector[key]) >= validation$byr$mini &
          as.numeric(passport_vector[key]) <= validation$byr$maxi
      }
      if(!isValidKey){
        isValid <- isValidKey
        break
      }
      
      # Check iyr
      if(key == "iyr"){
        isValidKey <- as.numeric(passport_vector[key]) >= validation$iyr$mini &
          as.numeric(passport_vector[key]) <= validation$iyr$maxi
      }
      if(!isValidKey){
        isValid <- isValidKey
        break
      }
      
      # Check eyr
      if(key == "eyr"){
        isValidKey <- as.numeric(passport_vector[key]) >= validation$eyr$mini &
          as.numeric(passport_vector[key]) <= validation$eyr$maxi
      }
      if(!isValidKey){
        isValid <- isValidKey
        break
      }
      
      # Check hgt
      if(key == "hgt"){
        val <- passport_vector[key]
        cm_in <- substr(val, nchar(val)-1, nchar(val))
        hgt <- as.numeric(substr(val, 1, nchar(val)-2))
        if(cm_in == "cm"){
          isValidKey <- hgt >= validation$hgt$min_cm &
            hgt <= validation$hgt$max_cm
        }else if(cm_in == "in"){
          isValidKey <- hgt >= validation$hgt$min_in &
            hgt <= validation$hgt$max_in
        }
      }
      if(!isValidKey){
        isValid <- isValidKey
        break
      }
    }
  }
  return(isValid)
}

getValidPassports <- function(input, required, validation = NULL){
  passport_list <- inputToData(input)
  validities <- sapply(passport_list, checkValidity, required, validation)
  return(validities)
}

countValidPassports <- function(validities){
  valid_count <- length(which(validities))
  return(valid_count)
}

### TESTS
input <- readLines("2020_day4_input_test.txt")
input2 <- readLines("2020_day4_input_test2.txt")
test_that("the function provides correct output", {
  expect_equal(countValidPassports(getValidPassports(input, required)), 2)
  expect_equal(countValidPassports(getValidPassports(input2, required, validation)), 4)
})

### PART 1
input <- readLines("2020_day4_input.txt")
validities <- getValidPassports(input, required)
countValidPassports(validities)
# 200

### PART 2
input <- readLines("2020_day4_input.txt")
validities <- getValidPassports(input, required, validation)
countValidPassports(validities)
# 116
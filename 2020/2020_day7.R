library(tidyverse)
library(testthat)

### FUNCTIONS
inputToData <- function(input){
  # split outer and inner bags
  x <- sapply(input, stringr::str_split, pattern = "contain")
  # remove 'bag(s)' and trailing white spaces
  x <- sapply(x, gsub, pattern = "bag|bags|bag\\.|bags\\.", replacement = "")
  outer_bags <- sapply(x[1,], str_trim)
  inner_bags <- sapply(x[2,], str_split, pattern = ",") %>%
    sapply(str_trim)
  # Make df
  df <- tibble::tibble(
    outer_bag = rep(outer_bags, sapply(inner_bags, length)),
    inner_bag = unlist(inner_bags)
  ) %>%
    dplyr::mutate(
      inner_per_outer = as.numeric(stringr::str_extract(inner_bag, pattern = "[[:digit:]]")),
      inner_per_outer = ifelse(is.na(inner_per_outer), 0, inner_per_outer)
    ) %>%
    dplyr::mutate(
      inner_bag = stringr::str_trim(stringr::str_replace(inner_bag, "[[:digit:]]", ""))
    )
  return(df)
}

findHoldingBags <- function(start_bag, df){
  holding_bags <- df$outer_bag[which(df$inner_bag == start_bag)]
  names(holding_bags) <- NULL
  tested_bags <- vector(mode = "character", length = 0)
  while(!all(holding_bags %in% tested_bags)){
    new_holding <- unique(df$outer_bag[which(df$inner_bag %in% holding_bags)])
    tested_bags <- unique(c(tested_bags, holding_bags))
    holding_bags <- unique(c(holding_bags, new_holding))
  }
  return(holding_bags)
}

findBagsInBag <-function(start_bag, df){
  outer_b <- start_bag
  df.temp <- df %>%
    dplyr::mutate(
      N_outer = ifelse(outer_bag %in% outer_b, 1, 0),
      outer_times_inner = inner_per_outer * N_outer,
      total = outer_times_inner)
  
  
  while(!all(outer_b == "no other")){
    # How many outer bags?
    df.sub <- df.temp[which(df.temp$outer_bag %in% outer_b),c("inner_bag", "outer_times_inner")]
    df.sub <-
      dplyr::group_by(df.sub, inner_bag) %>%
      dplyr::summarise(outer_times_inner = sum(outer_times_inner)) %>%
      dplyr::rename(
        outer_bag = inner_bag,
        N_outer = outer_times_inner)
    outer_b <- df.sub$outer_bag
    
    # New N_outer column
    df.temp <- dplyr::left_join(
      df.temp, df.sub, 
      by = c("outer_bag"))
    df.temp <- dplyr::mutate(
      df.temp,
      N_outer.x = NULL,
      N_outer = N_outer.y,
      N_outer.y = NULL,
      N_outer = ifelse(is.na(N_outer), 0, N_outer)
    )
    
    # outer times inner, add to total
    df.temp <- dplyr::mutate(
      df.temp,
      outer_times_inner = N_outer * inner_per_outer,
      total = total + outer_times_inner
    )
  }
  return(sum(df.temp$total))
}

### TEST
if(T){
  start_bag <- "shiny gold"
  input <- readLines("2020_day7_input_test.txt")
  df <- inputToData(input)
  input2 <- readLines("2020_day7_input_test2.txt")
  df2 <- inputToData(input2)
  input3 <- readLines("2020_day7_input_test3.txt")
  df3 <- inputToData(input3)
  answer3 <- c(4, 6, 16, 16, 32, 16, 4, 8)
}
test_that("holding bags are found correctly", {
  expect_equal(length(findHoldingBags(start_bag, df)), 4)
  expect_equal(findBagsInBag(start_bag, df), 32)
  expect_equal(findBagsInBag(start_bag, df2), 126)
  expect_equal(findBagsInBag(start_bag, df3), sum(answer3))
})


### Part 1
input <- readLines("2020_day7_input.txt")
start_bag <- "shiny gold"
df <- inputToData(input)
clrs <- findHoldingBags(start_bag, df)
length(clrs)
#337

### Part 2
input <- readLines("2020_day7_input.txt")
start_bag <- "shiny gold"
df <- inputToData(input)
findBagsInBag(start_bag, df)
# 50100
# import list of module masses
masses <- as.numeric(readLines("aoc1.txt"))

# per module: divide mass by 3, round down, subtract 2.
fuel_per_module <- function(mass){
  fuel <- floor(mass / 3) - 2
  return(fuel)
}

# sum to get total mass
fuel <- sum(fuel_per_module(masses))

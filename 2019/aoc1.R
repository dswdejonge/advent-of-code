# import list of module masses
masses <- as.numeric(readLines("aoc1.txt"))

# per module: divide mass by 3, round down, subtract 2.
fuel_per_module <- function(mass){
  fuel <- floor(mass / 3) - 2
  return(fuel)
}

# sum to get total mass
fuel <- sum(fuel_per_module(masses))

# take into account mass of fuel itself
fuel_equation <- function(mass){
  fuel <- floor(mass / 3) - 2
  fuel[which(fuel < 0)] <- 0
  if(all(fuel == 0)){
    return(fuel)
  } else {
    return(fuel + fuel_equation(fuel))
  }
}

fuel2 <- sum(fuel_equation(masses))

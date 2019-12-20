
# takes in two rows of factors or characters in the form
# R<int>, L<int>, U<int>, D<int>
get_path <- function(input){
  A_vec <- input[1,] ; A_vec <- A_vec[A_vec != ""]
  B_vec <- input[2,] ; B_vec <- B_vec[B_vec != ""]
  lA <- length(A_vec)+1
  lB <- length(B_vec)+1
  paths <- list(
    A = list(xs = rep(0,lA), ys = rep(0,lA)), 
    B = list(xs = rep(0,lB), ys = rep(0,lB)))
  for(rowi in 1:2){
    if(rowi == 1){vec <- A_vec}
    if(rowi == 2){vec <- B_vec}
    for(i in 1:length(vec)){
      instruction <- as.character(vec[i])
      letter <- substr(instruction, 1, 1)
      move <- as.integer(substr(instruction, 2, nchar(instruction)))
      if(letter == "R"){
        paths[[rowi]]$xs[i+1] <- paths[[rowi]]$xs[i]+move
        paths[[rowi]]$ys[i+1] <- paths[[rowi]]$ys[i]
      }else if(letter == "L"){
        paths[[rowi]]$xs[i+1] <- paths[[rowi]]$xs[i]-move
        paths[[rowi]]$ys[i+1] <- paths[[rowi]]$ys[i]
      }else if(letter == "U"){
        paths[[rowi]]$xs[i+1] <- paths[[rowi]]$xs[i]
        paths[[rowi]]$ys[i+1] <- paths[[rowi]]$ys[i]+move
      }else if(letter == "D"){
        paths[[rowi]]$xs[i+1] <- paths[[rowi]]$xs[i]
        paths[[rowi]]$ys[i+1] <- paths[[rowi]]$ys[i]-move
      }
    }
  }
  return(paths)
}

get_lines <- function(xs, ys){
  xs <- as.integer(xs)
  ys <- as.integer(ys)
  path.df <- data.frame(
    x1 = xs[-length(xs)],
    y1 = ys[-length(ys)],
    x2 = xs[-1],
    y2 = ys[-1]
  )
  sum_moves <- abs(path.df$x1 - path.df$x2)+abs(path.df$y1 - path.df$y2)
  for(i in 2:length(sum_moves)){
    sum_moves[i] <- sum_moves[i] + sum_moves[i-1]
  }
  path.df$sum_moves <- sum_moves
  return(path.df)
}

get_line_line_intersection <- function(p, q){
  if(p$x1 == p$x2){
    x <- p$x1
  }else if(q$x1 == q$x2){
    x <- q$x2
  }
  if(p$y1 == p$y2){
    y <- p$y1
  }else if(q$y1 == q$y2){
    y <- q$y1
  }
  is_point_on_line <- 
    x %in% p$x1:p$x2 &
    x %in% q$x1:q$x2 &
    y %in% p$y1:p$y2 &
    y %in% q$y1:q$y2
  if(is_point_on_line){
    return(c(x, y))
  }else{
    return(c(NA,NA))
  }
}

# A and B are dataframes with lines in the format
# line1 = x1, y1, x2, y2
# lapply(crossings, FUN = function(x){crossing %in% x})
get_crossings <- function(A, B){
  crossings <- list()
  index <- 1
  for(i in 1:dim(A)[1]){
    p <- A[i,]
    for(j in 1:dim(B)[1]){
      q <- B[j,]
      crossing <- get_line_line_intersection(p, q)
      if(!NA %in% crossing){
        total_moves <- 
          p$sum_moves - (abs(crossing[1]-p$x2)+abs(crossing[2]-p$y2)) +
          q$sum_moves - (abs(crossing[1]-q$x2)+abs(crossing[2]-q$y2))
        crossings[[index]] <- c(crossing, total_moves)
        index <- index + 1
      }
    }
  }
  return(unique(crossings))
}

# xy is c(x,y)
get_manhattan_dist <- function(xy){
  xy <- xy[1:2]
  return(sum(abs(xy)))
}

# 4. Actual question
input <- read.csv("aoc3.txt", header = F)
paths <- get_path(input)
A <- get_lines(xs = paths$A$xs, ys = paths$A$ys)
B <- get_lines(xs = paths$B$xs, ys = paths$B$ys)
# get crossings
crossings <- get_crossings(A, B)
# get manhattan distances
manh_distances <- unlist(lapply(crossings, get_manhattan_dist))
# remove origin intersections
manh_distances <- manh_distances[which(manh_distances != 0)]
# get closest intersection
closest_crossing <- min(manh_distances)
print(closest_crossing)


################################################
# Ugly stuff
# 1. small test
p <- matrix(c(0, 0, 3, 0), byrow = T, nrow = 2, ncol = 2)
q <- matrix(c(1, 1, 1, -1), byrow = T, nrow = 2, ncol = 2)
get_line_line_intersection(p, q)
# 1, 0

# 2. medium test
xs <- c(0, 3, 3, -5)
ys <- c(0, 0, -5, -5)
A <- get_lines(xs, ys)

xs = c(0, 0, 5, 5)
ys = c(0, -7, -7, -5)
B <- get_lines(xs, ys)

# get crossings
crossings <- get_crossings(A, B)
# Crossing at 0, -5
# get manhattan distances
manh_distances <- unlist(lapply(crossings, get_manhattan_dist))
# remove origin intersections
manh_distances <- manh_distances[which(manh_distances != 0)]
# get closest intersection
closest_crossing <- min(manh_distances)
print(closest_crossing)
# 5

# 3. question tests
input <- read.csv("aoc3a.txt", header = F)
input <- read.csv("aoc3b.txt", header = F)
input <- read.csv("aoc3c.txt", header = F)
paths <- get_path(input)
A <- get_lines(xs = paths$A$xs, ys = paths$A$ys)
B <- get_lines(xs = paths$B$xs, ys = paths$B$ys)
# get crossings
crossings <- get_crossings(A, B)
# get manhattan distances
manh_distances <- unlist(lapply(crossings, get_manhattan_dist))
# remove origin intersections
manh_distances <- manh_distances[which(manh_distances != 0)]
# get closest intersection
closest_crossing <- min(manh_distances)
print(closest_crossing)
#6, 159, 135


# p and q are points defined by matrix (x1, y1)
#                                      (x2, y2)
get_line_line_intersectionDEPRACATED <- function(p, q){
  p1 <- p ; p1[,2] <- 1
  p2 <- p ; p2[,1] <- p[,2] ; p2[,2] <- 1
  
  q1 <- q ; q1[,2] <- 1
  q2 <- q ; q2[,1] <- q[,2] ; q2[,2] <- 1
  
  intersection <- c(
    det(
      matrix(c(det(p), det(q), det(p1), det(q1)), nrow = 2, ncol = 2)
    ) / det(
      matrix(c(det(p1), det(q1), det(p2), det(q2)), nrow = 2, ncol = 2)
    ),
    det(
      matrix(c(det(p), det(q), det(p2), det(q2)), nrow = 2, ncol = 2)
    ) / det(
      matrix(c(det(p1), det(q1), det(p2), det(q2)), nrow = 2, ncol = 2)
    )
  )
  return(intersection)
}

#get_line_line_intersection <- function(p, q){
# intersection <- c(
#   ((p$x1*p$y2 - p$y1*p$x2)*(q$x1-q$x2) - (p$x1-p$x2)*(q$x1*q$y2 - q$y1*q$x2)) /
#     ((p$x1-p$x2)*(q$y1-q$y2) - (p$y1-p$y2)*(q$x1-q$x2)),
#   ((p$x1*p$x2 - p$y1*p$x2)*(q$y1-q$y2) - (p$y1-p$y2)*(q$x1*q$y2 - q$y1*q$x2)) /
#     ((p$x1-p$x2)*(q$y1-q$y2) - (p$y1-p$y2)*(q$x1-q$x2))
# )
# return(intersection)
#}
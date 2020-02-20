
library(dplyr)
library(ggplot2)
library(assertthat)


# check if a segment [A, B] intersects a target alkalinity line x = target (parallel to Oy)
is_segment_sca <- function(A, B, x){
  if ((max(A[1], B[1]) < x) | (min(A[1], B[1]) > x)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# compute the intersection of a segment with a target alkalinity line x = target given as t*A+(1-t)*B
compute_segment_sca <- function(A, B, x) {
  t = 1 - (x-A[1])/(B[1]-A[1])
  if (t>=0 & t<=1){
    return(t)  
  } else {
    return(NULL)
  }
}

# compute coordinates of a point t*A+(1-t)*B on a segment [A, B]
compute_segment_coord <- function(A, B, t) {
  if (!is.null(t)) {
    x <- t*A[1] + (1-t)*B[1]
    y <- t*A[2] + (1-t)*B[2]
    return(c(x, y))
  } else {
    return(NULL)
  }
}







# check if three point for a non-trivial triangle (area > delta)
is_triangle <- function(A, B, C, delta) {
    area <- abs(0.5*(A[1]*(B[2]-C[2])+B[1]*(C[2]-A[2])+C[1]*(A[2]-B[2])))
    if (area > delta) {
      return(TRUE) 
    } else {
      return(FALSE)
    }
}


# check if a triangle [A, B, C] intersects a target alkalinity line x = target (parallel to Oy)
is_triangle_sca <- function(A, B, C, x){
  AB = is_segment_sca(A, B, x)
  AC = is_segment_sca(A, C, x)
  BC = is_segment_sca(B, C, x)
  if (sum(AB, AC, BC) == 2) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# compute the intersection of a triangle with a target alkalinity line x = target given as coord y1, y2
compute_triangle_sca <- function(A, B, C, x) {
  y1 <- compute_segment_coord(A, B, compute_segment_sca(A, B, x))
  y2 <- compute_segment_coord(B, C, compute_segment_sca(B, C, x))
  y3 <- compute_segment_coord(C, A, compute_segment_sca(C, A, x))
  coord <- c(AB = y1, BC = y2, CA = y3)
  return(coord)
}


# compute baricentric coordinates for a point P relative to a triangle (A, B, C)
compute_baricentric <- function(A, B, C, P){
  matrix <- rbind(c(A[1], B[1], C[1]), c(A[2], B[2], C[2]), c(1, 1, 1))
  matrix_inv <- solve(matrix)
  polar_coord <- rbind(c(P[1]), c(P[2]), 1)
  bari_coord <- as.vector(matrix_inv %*% polar_coord)
  return(bari_coord)
}






















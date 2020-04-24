##Function creates a "matrix" object that can cache its inverse.
makeCacheMatrix <- function(matrix = matrix()) {
  
  #Stores the inverse value
  inverse <- NULL
  
  #Stores the original matrix and reset the inverse to NULL
  set <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }
 
  #Get original matrix
  get <- function() matrix
  
  #Set the inverse value
  set_inverse <- function(inv) inverse <<- inv
  
  #Get the inverse value
  get_inverse <- function() inverse
  
  #Return the list of 4 functions
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

cacheSolve <- function(special_matrix, ...) {
  inverse <- special_matrix$get_inverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- special_matrix$get()
  inverse <- solve(data, ...)
  special_matrix$set_inverse(inverse)
  inverse
}


## Unit tests for Programming Assignment 2
source("cachematrix.R")

amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()     

cacheSolve(amatrix)
amatrix$get_inverse()

cacheSolve(amatrix)
amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))

cacheSolve(amatrix)
amatrix$get()
amatrix$get_inverse()

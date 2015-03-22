## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" with a list to the following functions:
  #  * set the value of the matrix
  #  * get the value of the matrix
  #  * set the value of the inverse
  #  * get the value of the inverse
  # The object itself will contain two attributes:
  #  - x: the matrix
  #  - s: the cached inverse of the matrix if it's been already calculated.
  #
  # Args:
  #   x: matrix to be created.
  #
  # Returns:
  #   A list of functions to:
  #  * set the value of the matrix
  #  * get the value of the matrix
  #  * set the value of the inverse
  #  * get the value of the inverse
  s <- NULL
  
  # Definition of the set function for a new matrix
  set <- function(y) {
    # We will update x if it's different from y.
    if(!identical(x,y)) { 
      # We set the matrix
      x <<- y
      # We initialize the inverse since the matrix is new
      s <<- NULL
    }
  }
  # Definition of the get function for getting the current matrix
  get <- function() x
  # Definition of the set function for a new inverse of the matrix
  setsolve <- function(solve) s <<- solve
  # Definintion of the get function for getting the current inverse of the matrix
  getsolve <- function() s
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  # Calculates the inverse of the matrix if it has not been calculated yet.
  #
  # Args:
  #   x: matrix from which we want to get its inverse.
  #
  # Returns:
  #  Return a matrix that is the inverse of 'x'
  
  # We get the current value of the inverse.
  s <- x$getsolve()
  
  # We check if it's different from null.
  if(!is.null(s)) {
    # If it's different from null, it means that it has been already calculated,
    # so we return that value.
    message("getting cached data")
    return(s)
  }
  # If it's different from null, we get the matrix and calculate the inverse.
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

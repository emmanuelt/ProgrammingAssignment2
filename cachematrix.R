# The two functions are used to cache the inverse of a matrix, 
# because matrix inversion can be too long to compute.

# makeCacheMatrix creates a list containing 4 functions:
# 1. One function setting the value of the matrix.
# 2. One function getting the value of the matrix.
# 3. One function setting the value of inverse of the matrix.
# 4. One function getting the value of inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL 
  }
  get <- function() x
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# The following function returns the inverse of a matrix. 
# It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data.")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data)
  x$setinverse(inverse_matrix)
  inverse_matrix
}

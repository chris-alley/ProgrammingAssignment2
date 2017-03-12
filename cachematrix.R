## Pair of functions to compute the inverse of a matrix by caching data in order
## to save computational time.


## This function creates a special 'matrix' object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
  x <<- y
  inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special 'matrix' created by the
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data,...)
  
  x$setInverse(inv)
  return(inv)
}
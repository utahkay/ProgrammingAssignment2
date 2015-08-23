## Class and companion function to store a matrix 
##   together with its cached inverse
## Kay Johansen 

## Create an object that contains a matrix
##   and a place to cache its inverse
## This object does not calculate the inverse itself;
##   use the companion function cacheSolve() to do that
makeCacheMatrix <- function(theMatrix = matrix()) {
  theInverse <- NULL
  
  set <- function(m) {
    theMatrix <<- m
    theInverse <<- NULL
  }
  
  get <- function() 
    theMatrix
  
  setInverse <- function(inv) 
    theInverse <<- inv
  
  getInverse <- function(inv)
    theInverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of the matrix contained in the 
##   cacheMatrix object
## Uses the cached inverse if it's already cached,
##   otherwise, calls solve to calculate the inverse
##   and sets the cached value
cacheSolve <- function(cacheMatrix, ...) {
  inv <- cacheMatrix$getInverse()
  if (!is.null(inv)) {
    message("cache hit")
    return(inv)
  }
  message("cache miss")
  m <- cacheMatrix$get()
  inv <- solve(m)
  cacheMatrix$setInverse(inv)
  inv
}

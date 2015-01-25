## Put comments here that give an overall description of what your
## functions do

## This function wraps a matrix in an object which can calculate its inverse
## but also cache the result
makeCacheMatrix <- function(x = matrix()) {
  matrixinverse <- NULL
  set <- function(y) {
    x <<- y
    matrixinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) matrixinverse <<- mean
  getinverse <- function() matrixinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function expects to take as its first parameter an object
## created with makeCacheMatrix, by passing in a matrix object,
## and the function gets the matrix's inverse. It checks for a cached
## version of the inverse to avoid computing the inverse more than once.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixinverse <- x$getinverse()
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  data <- x$get()
  matrixinverse <- solve(data, ...)
  x$setinverse(matrixinverse)
  matrixinverse
}

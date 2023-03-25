## These functions were created to fulfill the Coursera programming assignment
## number 2, which was to write a pair of functions cache the inverse of
## of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
## Adding this line to verify that new version was pushed to github successfully

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y)  {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)   i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cachesolve retrieves the inverse
##from the cache.

cacheinverse <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
  
}

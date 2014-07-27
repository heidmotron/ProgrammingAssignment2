## cacheMatrix.R includes two functions for making a matrix
##   and the ability to cache the inverse of the matrix


## makeCacheMatrix returns a cacheMatrix object with the following functions defined:
##   $get - returns the matrix
##   $set - overides the matrix & resets the inverse cache
##   $setinverse - sets the inverse matrix
##   $getinverse - gets the inverse matrix
##
## params:
##   x -> a square matrix() 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  set <- function(m) { 
    x <<- m
    i <<- NULL
  }
  getinverse <- function() i
  setinverse <- function(m) i <<- m
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}

## cacheSolve returns the inverse of matrix in cacheMatrix object either from the cache or 
## by computing the inverse
## params:
##    x -> cachedMatrix object
##  ... -> optional arguments when computing the inverse for the first time
## 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting inverse from cache")
    return(inverse)
  }
  
  inverse <- solve(x$get(), ...)
  x$setinverse(inverse)
  inverse
}


## These functions are useful for saving time when calculating 
## the inverse of matrices

## This function creates a special "Matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  set <- function(y) {
    x <<- y
    m<<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getmean <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the invere of the special "matrix" returned
## by the previous function. If the invere has already been calculated 
## (and the matrix has not changed), then this function retrieves the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
        ## Return a matrix that is the inverse of 'x'
}

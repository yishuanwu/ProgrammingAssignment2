## First function caches the inverse of a matrix and the second function retrieve
## the inverse from the cache if the inverse has already been calculated

## makeCacheMatrix creates a special "matrix" object that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get = function() x
  setinv <- function(x) m <<- solve(x)
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above or retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  } else {
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  return(m)}
}

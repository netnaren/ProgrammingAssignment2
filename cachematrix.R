## The two functions below are used to create a special object that stores a matrix and cache's its inverse.

## makeCacheMatrix creates a special "matrix", containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## The cacheSolve function returns the inverse of the special matrix created with the makeCacheMatrix function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse of the matrix in the cache via the setSolve function.

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
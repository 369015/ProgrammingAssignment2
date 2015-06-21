## Two functions that cache the inverse of a matrix. 

## First function stores a matrix and cache's its inverse.
### The output of this is a list containing 4 functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following second function calculates the inverse of the matrix, by first checking to see if the inverse
## has already been calculated above. If so, it loads that inverse from the cache and skips the computation. 
## In other case it calculates the inverse of the new matrix and sets that inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


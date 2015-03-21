
## Create a special matrix object capable of caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  getinverted <- function() i 
  setinverted <- function(inverted) i <<- inverted
  
  list(get = get, getinverted = getinverted,
       set = set, setinverted = setinverted)
}


## Computes inverse matrix or returns cached version if present

cacheSolve <- function(x, ...) {
  # Retrieve the cache
  i <- x$getinverted()
  if(!is.null(i)) {
    message('Inverted matrix in cache')
    return(i)
  }
  # Calculate and cache the inverted
  i <- solve(x$get())
  x$setinverted(i)
  i
}

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Reading cache data")
    return(inverse)
  }
  m <- x$get()
  inverse <- solve(m, ...)
  x$setInverse(inverse)
  inverse
}
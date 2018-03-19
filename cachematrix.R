## Make Cache Matrix Function

## First create a square invertible matrix
## Then reutn a list that 1.) sets the matrix, 2.) gets the matrix, 3.) Sets the inverse 4.) Gets the Invers

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Utilizes output of makeCacheMatrix


cacheSolve <- function(x, ...) {
        ## Return inverse of x matrix
  inv <- x$getinv()
  ## if the inverse has been previously calculated and cahced
  if (!is.null(inv)) {
    message("retrieved cached data")
        ## get from cache and message
    return(inv)
  }
  
  ## if not already cached, solve for inverse
  
  matrixdata <- x$get()
  inv <- solve(matrixdata, ...)
  
  ## Sets value of inverse in the cache
  
  x$setinv(inv)
  return(inv)
  
}

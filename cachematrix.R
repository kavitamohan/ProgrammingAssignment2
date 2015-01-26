## The following code includes functions that cache and compute the inverse of a matrix

## This function creates a matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
set <- function(y) {
  x <<- y
  inverse <<- NULL
}
get <- function() x
setinverse <- function(inv) inverse <<- inv
getinverse <- function() inverse
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## This function computes the inverse of the special
## matrix returned by `makeCacheMatrix` above. If the inverse has
## been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()        ##Getting the inverse
  if(!is.null(inverse)) {          ## Checking if inverse is present
    message("Getting cached data")   ##Displaying the message
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)         ##Using solve to compute the inverse
  x$setinverse(inverse)
  inverse
 
}

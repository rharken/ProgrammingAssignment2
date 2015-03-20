## This set of functions creates a "cacheable" matrix that can speed up 
## a matrix inverse operation
## It consists of 2 functions: makeCacheMatrix and cacheSolve
## To use - first call the makeCacheMatrix function on your matrix and store the result
## Then, you can use cacheSolve instead of solve to return the inverse of your CacheMatrix in any type
## of repetitive operation that needs the inverse

## makeCacheMatrix - creates a "cachable" matrix object that has supporting functions
## *** You would first use this function to create a cachable matrix and then use
## *** the next function cacheSolve to return the inverse - thus speeding up
## *** repeat calls to get the inverse of the matrix
## get() - gets the content of the matrix
## set() - sets the content of the matrix
## getinverse() - returns the cached inverse matrix
## setinverse() - sets the cached inverse matrix

makeCacheMatrix <- function(m = matrix()) {
  ## m - contains the matrix
  ## i - contains the inverse of the matrix m
  ## default inverse to null to indicate the cache does not exist
  i <- NULL
  
  ## Helper function to initially set the matrix value
  ## and reset the inverse to null since there is
  ## a change to the original matrix
  set <- function(newMatrix) {
    m <<- newMatrix
    i <<- NULL
  }
  
  ## Helper function to return the matrix value
  get <- function() m

  ## Helper function to set or "cache" the inverse
  setinverse <- function(inverse) i <<- inverse

  ## Helper function to retrieve the cached inverse matrix
  getinverse <- function() i

  ## List containing the functions available in this object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve - returns a matrix that is the inverse of the matrix supplied
## It speeds up the inverse calculation, however, if the function has already
## been called because it will cache the inverse of the supplied matrix instead
## of calculating it each time

cacheSolve <- function(x, ...) {
  ## x - needs to be a matrix created by the createCacheMatrix function 
  
  ## Use the helper functions in the cacheable matrix created in the
  ## above function
    
  ##  First retrieve the stored inverse matrix if it exists
  i <- x$getinverse()

  ##  Null indicates the inverse cache does not exist
  ##  If it does then go ahead and return it
  ##  and show a message that we're returning the cached inverse matrix...
  if(!is.null(i)) {
    message("getting cached inverse data")
    return(i)
  }
  
  ## If it doesn't then we'll execute the inverse function on the matrix 
  ## but before returning it - we'll store it for later use
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i  
}

## The following two functions will cache a "square' matrix and its inverse.

## This function creates list of functions that let you "set" a matrix that is  
## cached in a different environment.  When you call cacheSolve the inverse of
## the "set" matrix will be cached in that same environment.

makeCacheMatrix <- function(x = matrix()) {
      library(MASS)
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <-function() x
      setinv <- function(ginv) m <<- ginv
      getinv <- function() m
      list(set = set,
            get = get,
            setinv = setinv,
            getinv = getinv)
}

## This function computes the inverse of the "set" matrix returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed by calling makeCacheMatrix again), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- ginv(data)
      x$setinv(m)
      m
}

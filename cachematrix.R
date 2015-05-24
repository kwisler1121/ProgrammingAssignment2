## These functions will take a matrix and store the inverse of the matrix 
## in the cache. The second function will then be able to return the inverse 
## without doing additional calculations. 

## This function returns a list containing functions 
## that will be stored in the cache.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
      x <<- y
      inverse<<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inverse <<- solve
    getsolve <- function() inverse
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
  
}


## Returns the inverse of a matrix by returning information stored in the cache. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    inverse <- x$getsolve()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setsolve(inverse)
    inverse
}


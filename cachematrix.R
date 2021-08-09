## The Assignment goal is to write a pair of functions that cache 
## the inverse of a matrix

## Giving to the function the argument X, where x is a square invertible matrix, 
## this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
  }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## IMPORTANT: For these two functions to work, first, one has to assign 
## the result of calling makeCacheMatrix on a square invertible matrix
## to a new variable and run cacheSolve on this variable:
## var1 <- makeCacheMatrix (X) (X=square invertible matrix)
## then run: cacheSolve (var1)

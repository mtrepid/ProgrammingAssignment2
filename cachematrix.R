## R Programming Assignment 2
## Includes 2 functions:  makeCacheMatrix, which creates and caches the inverse of x,
## and cacheSolve, which retrieves the inverse out of the cache, and if it doesn't 
## exist, computes the inverse of x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() m
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get,
        setSolve = setSolve,
        getSolve = getSolve)
}

## Computes the inverse of the matrix returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## cachesolve retrieves the inverse from the cache.

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

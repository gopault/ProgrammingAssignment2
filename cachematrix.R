## The functions below take advantage of lexical scoping to cache
## the inverse of a matrix. As computing the inverse of a matrix
## is potentially costly, we want to avoid having to do so
## repeatedly.

## The makeCacheMatrix function creates an "R" object to store a
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
                s <- NULL
                set <- function(y) {
                        x <<- y
                        s <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) s <<- solve
                getsolve <- function() s
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
}


## The cacheSolve function relies on makeCacheMatrix to retrieve
## the inverse of a matrix cached in the makeCacheMatrix
## environment.

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

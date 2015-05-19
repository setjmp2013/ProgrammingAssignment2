## These 2 functions set a cache of a matrix, and then calculate the
## inverse of it.

## This function returns the passed matrix as a functional cache

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set=set, get=get,
             setsolve=setsolve,
             getsolve=getsolve)
}


## This function solves, then caches the resulting inverse matrix on the first
## call. Subsequent calls return the cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

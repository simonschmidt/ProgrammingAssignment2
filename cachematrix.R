# - Coursera rprog-006 Programming Assignment 2

# Functions to cache the matrix inverse
# Create a matrix with makeCacheMatrix and use cacheSolve to get the inverse.
#
# Example:
#  Set up data:
#   n  <- 1000
#   m  <- matrix(rnorm(n*n), ncol=n)
#   cm <- makeCacheMatrix(m)
#
#  Compare time between first and second calculation:
#   system.time(cacheSolve(cm))
#   system.time(cacheSolve(cm))

# makeCacheMatrix(x) creates a list with the following attributes:
#  $get - Get the underlying matrix
#  $set - Set the underlying matrix
#  $getInv - Get the cached inverse, NULL if not yet computed
#  $setInv - Set the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    invCache <- NULL

    get <- function() x

    set <- function(y) {
        # New matrix, reset the cache
        x <<- y
        invCache <<- NULL
    }

    getinv <- function() invCache

    setinv <- function(m) invCache <<- m

    list(set = set,
         get = get,
         getinv = getinv,
         setinv = setinv)
}

# cacheSolve(cm) calculates inverse of a matrix created by
# makeCacheMatrix and caches the result.
cacheSolve <- function(x, ...) {
    m <- x$getinv()

    if (!is.null(m)) {
        # Found cache, use it
        return(m)
    }

    # Calculate and update cache
    m <- solve(x$get(), ...)
    x$setinv(m)
    x$getinv()
}

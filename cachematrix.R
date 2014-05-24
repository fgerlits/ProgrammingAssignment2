## Cache the computation of the inverse of a matrix.
## Usage:
##   matrix <- ...
##   cacheableMatrix <- makeCacheMatrix(matrix)
##   inverse <- cacheSolve(cacheableMatrix)   # computes the inverse
##   inverse2 <- cacheSolve(cacheableMatrix)  # returns the inverse
##                                              from the cache
##
## This is the same code as given in the assignment as an example,
## with small modifications.

## Creates a wrapper around the x matrix which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(new.inverse) inverse <<- new.inverse
        getInverse <- function() inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Inverts ("solves") the matrix.
## The input x is assumed to be a wrapper object returned by makeCacheMatrix.
## Caches the result, so a second call with the same argument will return
## immediately with the previously computed result.
## Throws an exception if the matrix is not invertible. The exception is not cached.

cacheSolve <- function(x, ...) {
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setInverse(inverse)
        inverse
}

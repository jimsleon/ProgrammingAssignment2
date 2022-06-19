## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are two functions that are used to create a special list object that
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
## The special "matrix" is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## then return the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (is.null(inv)) {
        inv <- solve(x)
        x$setInverse(inv)
    }

    inv
}

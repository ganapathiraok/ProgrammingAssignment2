## Caching the Matrix Inverse:
## Matrix inversion is usually a costly computation and Its better to cache the inverse of a matrix.
## Below two functions are used to create a special matrix and it stores a matrix and caches its inverse.

## This function creates a special "matrix" and it can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}

## below fuction calculates the matrix inverse and caches.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv
}

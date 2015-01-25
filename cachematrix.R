## This file functions for creating matrices that cache their inverses

## Creates a Matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
   
    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Updates cached value of inverse of x
## Retrieves cached data if the inverse is already computed
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
        inv <- x$getInverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}

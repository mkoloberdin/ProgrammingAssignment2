## Avoid repetetive calculations of inverse of a matrix by caching the inverse

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL # Invalidate the cache due to change of matrix
    }
    get <- function() x
    setInv <- function(invM) inv <<- invM
    getInv <- function() inv
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Return a matrix that is the inverse of "x".
## Use cached result when available, otherwise first calculate the reverse and update the cache.
## "x" is a "matrix" object created by makeCacheMatrix()

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}

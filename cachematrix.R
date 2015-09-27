## Cache the inverse of a square invertible matrix by the following 
## two functions

## makeCachematrix function creates an object that caches
## the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        set <- function(y) {
                x <<- y
                r <<- NULL
        }
        get <- function() x
        setreverse <- function(reverse) r <<- reverse
        getreverse <- function() r
        list(set = set, get = get,
             setreverse = setreverse,
             getreverse = getreverse)
}


## cacheSolve function computes the inverse of the matrix returned by
## makeCacheMatrix and stores it in the object created by 
## makeCachematrix. If the inverse has been caculated, then the 
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r <- x$getreverse()
        if(!is.null(r)) {
                message("getting cached data")
                return(r)
        }
        data <- x$get()
        r <- solve(data)
        x$setreverse(r)
        r
}

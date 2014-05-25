## The two functions below help to illustrate the concept of storing the output of
## time-consuming calculations in the cache using lexical scoping.

## makeCacheMatrix takes an invertible matrix as an input and returns a list of
## four functions that: (1) set the value of the matrix, (2) get the value of
## the matrix, (3) set the value of the inverse of the matrix and (4) get the
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## cacheSolve calculates the inverse of the list of functions created by
## makeCacheMatrix.  It first checks if the inverse has been calculated, and if so
## it extracts the inverse from the cache and skips the computation.  Otherwise it
## calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}

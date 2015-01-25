## These functions support calculating and caching the inverse of
## a matrix. The caching is done to improve performance.

## makeCacheMatrix returns a list of functions to manage the inverse
## of a matrix. The set function stores the passed matrix and clears
## the inverse since it may no longer be valid. The get function
## returns the passed matrix. The setinverse function stores the provided
## inverse and the getinverse function returns the stored inverse.

makeCacheMatrix <- function(x = matrix()) {

    im <- NULL
    set <- function(y) {
        x <<- y;
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns either the stored inverse or calculates the
## inverse and then stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if (!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    m <- x$get()
    im <- solve(m)
    x$setinverse(im)
    im
}

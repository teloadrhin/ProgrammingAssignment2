################################################################################
## Functions for caching of the inverse of a matrix
################################################################################

## Function to generate a special "matrix" type, which can cache the matrix
## inverse

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function () x
    setInverse <- function(Inv) I <<- Inv
    getInverse <- function() I
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)    
}


## This function computes the inverse of a "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated it the cached
## value is used instead.  

cacheSolve <- function(x, ...) {
    I <- x$getInverse()
    if(!is.null(I)){
        message("getting cachend Inverse")
        return(I)
    }
    my_matrix = x$get()
    I <- solve(my_matrix, ...)
    x$setInverse(I)
    I
}

## makeCacheMatrix() and cacheSolve() function to take in an
## matrix and cache it for further computation.This computation is
## carried out by the cacheSolve() function incase the

## makeCacheMatrix() takes in an invertible matrix (optional) and returns
## a special vector which is a list of functions. The function also caches 
## the matrix given as an input through the set() function.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## cacheSolve() takes in the list of functions produced by makeCacheMatrix()
## as well as the cached matrix and returns the inverted matrix. This may be
## calculated on the spot if the inverted matrix was not cached beforehand or
## would return the previously calculated inverse if the initial parameters 
## remain the same.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}

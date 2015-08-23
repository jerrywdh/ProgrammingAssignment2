## The functions below allow to cache the inverse of a matrix. This helps reduce
## computation time especially when the matrix is large and it has to be
## computed repeatedly (e.g. in a loop).

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  ## initializes the inverse matrix
    set <- function(y) {  ## sets the value of the matrix
        x <<- y ## feeds the matrix in the parent environment with a new value
        inv <<- NULL ## intializes the inverse matrix in the parent environment
    }
    get <- function() x  ## sets the value of the matrix
    setinv <- function(inverse) inv <<- inverse  ## sets the value of the inverse
                                                 ## in the parent environment
    getinv <- function() inv  ## gets the value of the inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  ## returns a list of 4 functions
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()  ## tries to fetch the inverse matrix
    if(!is.null(inv)) {  ## checks if the inverse has already been calculated
        message("getting cached data")
        return(inv)  ## retrieves the inverse from the cache and skips the
                     ## computation
    }
    data <- x$get()  ## otherwise passes the new value
    inv <- solve(data, ...)  ## calculates the inverse of the data
    x$setinv(inv)  ## sets the value of the inverse in the cache
    inv ## returns the inverse matrix of 'x'
}

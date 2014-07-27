## Two functions here provide the ability to cache a matrix and it's inverse so
## that the costly calculation of the inverse is not required as long as the
## matrix does not change. The approach used is to make use of the special <<-
## assignment operator which cause a search through parent environments for an
## existing variable.

## makeCacheMatrix: This function creates a special "matrix" object that can cache
## its inverse. The return object maintains the matrix, the variable for the
## inverse, and 4 supporting functions:
##      set: can be used to set the value of the vector
##      get: can be used to access the matrix
##      setinvm: is used compute the inverse matrix
##      getinvm: is used to get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvm <- function(invm) m <<- invm
    getinvm <- function() m
    list(set = set, get = get,
         setinvm = setinvm,
         getinvm = getinvm)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
    m <- x$getinvm()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinvm(m)
    m
}

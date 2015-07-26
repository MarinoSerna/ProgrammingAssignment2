## calculate and keep in cache the inverse of a matrix


## create a new cache matrix. will be clean up any previous cached matrix
makeCacheMatrix <- function(x = matrix()) {
    # clean up any previous cached value
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # the new matrix will be cached
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## get the solve of a matrix that has been defined in makeCacheMatrix, and will be returned with x$get()
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## the solve is not cached, so will be processed and cache in m throwght x$setsolve(m)
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    ## Return a matrix that is the inverse of 'x'
}

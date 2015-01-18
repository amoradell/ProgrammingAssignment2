## makeCacheMatrix stores matrix and its inverse calculated by cacheSolve
## cacheSolve returns inverse of matrix trying to get cached value first

## makeCacheMatrix returns a list with 4 functions 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setsolve <- function(solved) m <<- solved
    getsolve <- function() m
    list(set = set, get = get, 
        setsolve = setsolve, 
        getsolve = getsolve)
}


## cacheSolve returns inverse of matrix with cached result 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## trying to get stored result
        m <- x$getsolve()
        ## if m exists, no calculation
        if (!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        ## here, inverse is not calculated
        data <- x$get()
        m <- solve(data, ...)
        ## stores result so it isn't calculated each time cacheSolve is called 
        x$setsolve(m)
        m
}

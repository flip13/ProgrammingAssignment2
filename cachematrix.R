## The functions will cache a matrix.  The next function checks for a cached
## inverted matrix or else will do the inversion.

## Put the matrix into cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(mat) m <<- mat
    getmatrix <- function() m
    list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Check if the inverse of the matrix is in cache or solve it.

cacheSolve <- function(x, ...) {
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
        ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    m <- solve(data, ...)
    x$setmat(m)
    m
}

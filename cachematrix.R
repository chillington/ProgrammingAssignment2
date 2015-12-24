## "makeCacheMatrix" creates a special "matrix" object that
## can cache its inverse
## "cachesolve" computes the inverse of the special "matrix"
## returned by "makeCacheMatrix" or retrieves the inverse from 
## the cache if it has already been calculated

## "makeCacheMatrix" stores a list of functions
## "set" changes the stored matrix (x)
## "get" returns the stored matrix (x)
## "setinv" stores the inverse of the matrix (m)
## "getinv" returns the stored inverse of the matrix (m)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## "cachesolve" checks the stored value (m) for the inverse
## of the matrix and either returns the stored value
## from the cache and skips the computation or calculates 
## the inverse of the matrix (x) and sets the value in the
## cache using "setinv" 

cacheSolve <- function(x, ...) {
    m<- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m    ## Return a matrix that is the inverse of 'x'
}

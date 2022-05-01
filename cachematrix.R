## makeCacheMatrix stores four functions (set, get, setinv, and getinv) 
## as well as x and inv in the cache, without executing the solve function

## starting value of inv is set to NULL to clear all previous values from 
## the cache
## x is set to y in the parent environment
## the << operator ensures that the assignment happens in the parent environment

## if called, get will retrieve the value of x from the parent environment of 
## makeCacheMatrix
## if called, setinv is going to calculate the inverse of a matrix, and getinv 
## will retrieve the value for inv from the parent environment of 
## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function () x
        setinv <- function(solve) inv <<- solve
        getinv <- function () inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve checks, whether there is a value for inv (other than NULL) 
## in the cache 
## If yes, inverted matrix will not be calculated, but retrieved from cache,
## and the respective message will be displayed
## If not, the function solve will be executed on the object x as stored in the
## parent environment of makeCacheMatrix
## output is the inverse of the matrix passed to the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

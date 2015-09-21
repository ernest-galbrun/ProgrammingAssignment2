## Theses function are the implementation of coursera's second programming 
## assignment for the data science R programming course
## The assignment consists in writing a function allowing the caching of 
## a matrix inverse. We provide the input matrix to the makeCacheMatrix
## The object built can be used to 
## - Get the matrix value, or Set a new one
## - Get or set the inverse value

## A call to cacheSolve will check if there is an inverse value available 
## If not, it is computed and set.
## If so, it is returned


## build an object consisting of 4 functions : get, set, getinverse, setinverse
## this object keeps the value of the matrix, and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## call cacheSolve on a CacheMatrix object to get the inverse without recomputing 
## it if not necessary.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

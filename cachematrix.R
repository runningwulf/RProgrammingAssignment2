## The main purpose of these two functions is set the matrix in cache
## and meantime calculate the inverse matrix of the origins.
## Precondition: All the matrix input is assumed to be invertible

## This function creates a special "matrix" object that can cache itself 
## and meanwhile create a the NULL object for inverse
## This function also include 4 sub functions
## set - set or reset the value of the matrix
## get - get the value of the matrix
## setinverse - set the value of inverse matrix
## getinverse - get the value of inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    s<-NULL
    set <- function(y = matrix()){
        x <<- y
        s <<- NULL
    }
    get <-function() x
    setinverse <- function(x) s<<- x
    getinverse <- function() s
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## This functino will check if there is a inverse matrix in cache, if there is not, 
## then calculate it and set it into cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data)
    x$setinverse(s)
    s

}

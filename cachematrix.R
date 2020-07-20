## This function creates a special "matrix" object that can cache its inverse
## it contains two setters and getters, chacheSolve function can access data by the list in the end

library(MASS)   # not only solve squared matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y){
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inverse_x <<- inverse
    getInverse <- function(){
        inver <- ginv(x)
        inver%*%x
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function access the matrix from makeCacheMatrix and return the value of inverse matrix
## if the matrix has not been inversed yet, the function will calculate its inverse.
## if the matrix inversed value is already existed, the inverse value will be returned.

cacheSolve <- function(x, ...) {
    inverse_x <- x$getInverse()
    if(!is.null(inverse_x)){
        message("getting cached data")
        return(inverse_x)
    }
    data <- x$get()
    inverse_x <- solve(data,...)
    x$setInverse(inverse_x)
    inverse_x
    ## Return a matrix that is the inverse of 'x'
}

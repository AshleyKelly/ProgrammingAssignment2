## cachematrix.R
## Programming Assignment 2 for Coursera R Programming Week 3 programming assignement

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  matI <- NULL
    set <- function(y) {
        x <<- y
        matI <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) matI <<- solve
    getInverse <- function() matI
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  matI <- x$getInverse()
    if(!is.null(matI)) {
        message("getting cached data")
        return(matI)
    }
    data <- x$get()
    matI <- solve(data, ...)
    x$setInverse(matI)
    matI
}

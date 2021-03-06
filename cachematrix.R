## cachematrix.R
## Programming Assignment 2 for Coursera R Programming Week 3 programming assignement

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    matI <- NULL
    
    ## Set the value of the matrix
    set <- function(y) {
        x <<- y
        matI <<- NULL
    }
    
    ## Get the value of the matrix
    get <- function() x
    
    ## Set the inverse of the matrix
    setInverse <- function(solve) matI <<- solve
    
    ## Get the inverse of the matrix
    getInverse <- function() matI
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    ## Calculate the matrix inverse
    matI <- x$getInverse()
    
    ## If the inverse exists in the cache, get the cached inverse matrix
    if(!is.null(matI)) {
        message("getting cached data")
        return(matI)
    }
    data <- x$get()
    
    ## Calculate the inverse
    matI <- solve(data, ...)
    
    ## Set the matrix inverse
    x$setInverse(matI)
    matI
}

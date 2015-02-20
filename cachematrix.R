## This script is used to cache the inverse of a Matrix with the help of two functions
## called "makeCacheMatrix" and "cacheSolve"

## The makeCacheMatrix function creates a special matrix object that can cache its inverse
## and also creates a list contaning following functions:
## set the value of matrix object
## get the value of matrix object
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
      x <<- y
      minv <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(matrixInverse) minv <<- matrixInverse
    getMatrixInverse <- function() minv
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)

}

## The cacheSolve function computes the inverse of the "matrix" returned by makeCacheMatrix 
## function. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getMatrixInverse()
    if(!is.null(minv)) {
      message("getting cached matrix inverse")
      return(minv)
    }
    data <- x$get()
    minv <- solve(data)
    x$setMatrixInverse(minv)
    minv
}
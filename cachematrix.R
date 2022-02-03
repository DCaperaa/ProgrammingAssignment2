## Below are two functions that are used to create a special object that stores 
## a numeric matrix and cache its inverse.

## makeCacheMatrix creates a "special matrix", which is in fact a list
## containing four functions to :
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse matrix
## 4- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    iMatrix <- NULL
    set <- function(y) {
        x <<- y
        iMatrix <<- NULL
    }
    get <- function() x
    setiMatrix <- function(m) iMatrix <<- m
    getiMatrix <- function() iMatrix
    list(set = set, get = get,
         setiMatrix = setiMatrix,
         getiMatrix = getiMatrix)
}


## cacheSolve computes the inverse of the "special matrix" created with
## makeCacheMatrix. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it computes the inverse of the matrix, named "data"
## in the function, and sets the inverse matrix in the cache via the setiMatrix
## function.

## /!\ The supplied matrix ('data' in cacheSolve) is assumed to be invertible
## (which also implies that the matrix is square).

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    iMatrix <- x$getiMatrix()
    if(!is.null(iMatrix)) {
        message("getting cached data")
        return(iMatrix)
    }
    data <- x$get()
    iMatrix <- solve(data, ...)
    x$setiMatrix(iMatrix)
    iMatrix
}



##           Course "R Programming" - Programming Assignment 2"
##
## Computing and caching the inverse of a matrix (the purpose is to cache the 
## result the first time that the inverse is requested, so further requests of
## the same value will have returned the cached value instead of computing it
## again)
##
## Usage:
##   a <- makeCacheMatrix(...put here your square invertible matrix...)
##   cacheSolve(a)
##   cacheSolve(a)


## makeCacheMatrix : return a list of 4 functions:
## -  "set" : set the value of the matrix
## -  "get" : get the value of the matrix
## -  "setinverse" : set the the inverse of the matrix
## -  "getinverse" : get the the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##  cacheSolve : calculates the inverse of the special "matrix" created with
##               the above function.
##  It first checks to see if the inverse has already been calculated.
##  If so, it gets the inverse from the cache and skips the computation.
##  Otherwise, it calculates the inverse of the matrix and sets the value
##  in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
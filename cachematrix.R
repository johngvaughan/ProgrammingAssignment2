## About
## This file contains 2 functions, makeCacheMatrix() and cacheSolve(), that
## (1) allow the creation of a cached matrix object when provided with an
## ordinary "uncached" invertible matrix and 
## (2) return the inverse of the matrix object from the cache (if it has already
## been inverted) or by a new matrix inversion
##
## Usage example
## > source("yourdirectoryhere/cachematrix.R")
## > a_matrix <- matrix(c(1,1,4,0,3,1,4,4,0), nrow=3, ncol=3)
## > AC <- makeCacheMatrix(a_matrix)
## > cacheSolve(AC)
## > cacheSolve(AC)          # 2nd call checks caching
##
## History
## The stubs for makeCacheMatrix() and cacheSolve() were obtained from Roger
## Peng's GitHub repository at
## https://github.com/rdpeng/ProgrammingAssignment2
## The structure of makeCacheMatrix() and cacheSolve() is taken from the code
## for the makeVector() and cachemean() functions provided as examples in the
## same repository
##
## Last Modified: 21 December 2014 by JV

## Function makeCacheMatrix() creates a special "matrix" object that can cache
## its inverse.
## Input: A standard "uncached" invertible matrix 'x'
## Returns: A cached invertible matrix, accessible via
##          x$set(), x$get(), x$setInverse(), x$getInverse()
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <-function(y) {     ## Create a cached version of matrix x
                x <<- y
                inv <<- NULL
        }
        get <- function() x     ## Return cached matrix x
        setInverse <- function(inverse) inv <<- inverse  ## Cache matrix inverse
        getInverse <- function() inv                  ## Retrieve matrix inverse 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function cacheSolve() calculates the inverse of the special matrix object
## returned by makeCacheMatrix(). If the inverse has been calculated already,
## and the matrix has not changed, then cacheSolve() retrieves the inverse from
## the cache.
## Input: An invertible matrix 'x', previously returned by makeCacheMatrix()
## Returns: The inverse of 'x', either from cache or by a new call to solve(x)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
        ## According to the Programming Assignment 2 specification, assume that
        ## the matrix 'x' is always invertible.
        
        inv <- x$getInverse()
        ## Return inverse from cache if already calculated
        if(!is.null(inv)) {                  
                message("Getting cached data")
                return(inv)
        }
        ## else calculate matrix inverse, cache it and return it
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv    
}

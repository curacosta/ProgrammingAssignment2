#
# Emanuel Cura Costa 
# 26/04/15
#
# R Programming (Coursera)
# por Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
#
###############################################################################
##                                                                           ##
##  Programming Assignment 2: Caching the Inverse of a Matrix                ##
##                                                                           ##
##                                                                           ##
##  Write the following functions:                                           ##
##                                                                           ##
##       1.  makeCacheMatrix: This function creates a special "matrix"       ##
##           object that can cache its inverse.                              ##
##       2.  cacheSolve: This function computes the inverse of the special   ##
##           "matrix" returned by makeCacheMatrix above. If the inverse has  ##
##           already been calculated (and the matrix has not changed), then  ##
##           the cachesolve should retrieve the inverse from the cache.      ##
##                                                                           ##
##  Computing the inverse of a square matrix can be done with the solve      ##
##  function in R. For example, if X is a square invertible matrix, then     ## 
##  solve(X) returns its inverse.                                            ##
##                                                                           ##
###############################################################################

makeCacheMatrix <- function(x) { 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

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

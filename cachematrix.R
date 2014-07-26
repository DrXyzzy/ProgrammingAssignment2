## Put comments here that give an overall description of what your
## functions do

## Given an invertible matrix, return a list containing functions to:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse of the matrix
##   get the valud of the inverse of the matrix

## Note: getinverse() will return NULL if cacheSolve() has not been called

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


## Given a CacheMatrix produced by makeCacheMatrix,
##   if the inverse was not already computed
##     compute the inverse, save inverse in CacheMatrix, and return the inverse
##   otherwise, inverse was already computed, so return the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}


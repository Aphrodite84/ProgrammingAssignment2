## Caching the Inverse of a matrix
## Matrix inversion is basically a costly computation and easier to
## caching the inverse of  a matrix instead of computing it repeatedly
## Below there are a pair of funtions that are being used to creating special object
## which stores matrix and caches its iverse

## This funtion creats special "matrix" which help to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) 
	inv <<- inverse
	getInverse <- function() inv
	list(set= set,
		get= get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## Cache function computes the inverse of the special "matrix" (shown in avobe)
## creating by makeCacheMatrix. If the inverse has already been calculted from the cache without changing 
## matrix functio, it would retrive the inverse from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

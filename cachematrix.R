# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieve the inverse from the cache.
# The following function returns the inverse of the matrix. However,it first checks if
# the inverse has already been calculated(and the matrix has not changed). If so, cachesolve retrieve the inverse from the cache
# If not, it calculates the inverse, sets the value in the cache via the setinverse function.


# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample run:
## > x <- matrix(1:4,2,2)
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1 		3
## [2,]  2  	4

## No cache in the first run
## > cacheSolve(m)
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## Retrieving from the cache in the second run
## > cacheSolve(m)
##getting cached data.
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## > 
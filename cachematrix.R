## Pair of functions that cache the inverse of a matrix.
##
## Matrix inversion is usaually a costly computation and there may be some
## benefit to chaching the inverse of a matrix rather than computing it
## repeatedly.
##
## Usage:
## Cache a matrix via makeCacheMatrix
## m <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2))
## Solve the matrix using cacheSolve
## ms <- cacheSolve(m)
## The calculation is only done the first time cacheSolve is called. Repeated
## calls will get the result from the cache.



## makeCacheMatrix
## Wrap matrix x in a vector of functions for managing cached version of solved x
## The functions are: set - set the value of the cached matrix x and resets 
##                          the solved cache 
##                    get - get the value of the matrix x
##                    setsolve - set the solved value of matrix x
##                    getsolve - get the solved value of matrix x

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, 
             get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve
## Compute the inverse of a matrix returned by makeCahceMatrix.
## The inverse is only calculated once for unchanged matracies by using a cache

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if (!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

## Implement the second assignment of the rprog-005 Coursera course.

## Convert a matrix to a list with methods to get/set its value and its inverse.
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinv <- function(inv) inverse <<- inv
	getinv <- function() inverse
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}

## Calculate the inverse of a cached matrix or retrieve its cached value when
## available
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat)
        x$setinv(i)
        i
}

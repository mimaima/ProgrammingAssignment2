## functions to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# a special object that stores a matrix x and cache's its inverse in m.
	m <- NULL
	
	# set the value of the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	# get the value of the matrix
	get <- function() x
	
	# set the value of the inverse
	setinverse <- function(inverse) m <<- inverse
	
	# get the value of the inverse
	getinverse <- function() m
	
	# a special "matrix", which is really a list containing above 4 functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # first checks to see if the mean has already been calculated
	m <- x$getinverse()
	
	if(!is.null(m)) {
        # If so, gets the inverse from the cache and skips the computation.
		message("getting cached data")
		return(m)
	}
	
    # Otherwise, calculates the inverse of the data and sets the value of the inverse in the cache.
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
